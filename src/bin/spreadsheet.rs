use anyhow::{anyhow, bail, Result};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fmt::Display;

type Val = usize;
type Id = (usize, usize); // (row, col)
type CellLookup = HashMap<Id, HashSet<Id>>;

enum CachedValue {
    Cached(Val),
    Dirty,
}

struct GridCell {
    cache: RefCell<CachedValue>,
    formula: Box<dyn CellFormula<Val>>,
}

trait CellFormula<T> {
    fn ref_cells(&self) -> Option<HashSet<Id>>;
    fn compute_cell_value(&self, sheet: &Spreadsheet) -> Result<T>;
}

struct Literal(Val);
impl CellFormula<Val> for Literal {
    fn ref_cells(&self) -> Option<HashSet<Id>> {
        None
    }
    fn compute_cell_value(&self, _: &Spreadsheet) -> Result<Val> {
        Ok(self.0)
    }
}

struct Sum2(Id, Id);
impl CellFormula<Val> for Sum2 {
    fn ref_cells(&self) -> Option<HashSet<Id>> {
        Some(HashSet::from([self.0, self.1]))
    }
    fn compute_cell_value(&self, sheet: &Spreadsheet) -> Result<Val> {
        Ok(sheet.get(&self.0)? + sheet.get(&self.1)?)
    }
}

/*
Problem Statement:

Define an API for a simple "spreadsheet". Every cell of the sheet can be in 3 states:
- Uninitialized
- Initialized to a concrete integer value
- Initialized to a formula which is the sum of two other cells

Define an api for
- Setting a cell to one of its allowable values
- Retrieving a cell's current value

NOTE: The solution here slightly generalizes the problem statement.

It
*/
struct Spreadsheet {
    pub(crate) rdeps: CellLookup,
    pub(crate) vals: HashMap<Id, GridCell>,
    max_col: usize,
    max_row: usize,
}
impl Spreadsheet {
    pub fn new() -> Self {
        Spreadsheet {
            rdeps: HashMap::new(),
            vals: HashMap::new(),
            max_row: 0,
            max_col: 0,
        }
    }

    // Recursively dirty the rdeps of id, if there is no dependency cycle
    // If there is, don't dirty anything and return Err.
    fn check_cycles_or_mark_descendants_as_dirty(self: &Self, id: &Id) -> Result<()> {
        let mut seen = HashSet::new();

        // Note: deliberately not add the starting cell id to the list of visited cells.
        // We know we're starting from it. We need to find out if we can also reach
        // it again via travelling through all of its rdeps. Such a state indicates a dep cycle.
        // In that case, the update will be rejected, and the state shouldn't change. Nothing
        // should be marked as dirty.
        self.update_rdeps_recursive(id, &mut seen);
        if seen.contains(id) {
            bail!("Update would create a dependency cycle");
        }
        for id in &seen {
            let cell = self.vals.get(id).unwrap();
            *cell.cache.borrow_mut() = CachedValue::Dirty;
        }
        Ok(())
    }

    fn update_rdeps_recursive(self: &Self, id: &Id, seen: &mut HashSet<Id>) {
        if let Some(rdeps) = self.rdeps.get(id) {
            for rd in rdeps {
                if seen.contains(rd) {
                    continue;
                }
                self.collect_dirty(rd, seen);
            }
        }
    }
    fn collect_dirty(self: &Self, id: &Id, seen: &mut HashSet<Id>) {
        seen.insert(*id);
        self.update_rdeps_recursive(id, seen);
    }

    pub fn get(&self, id: &Id) -> Result<Val> {
        let cell_val = self.vals.get(id).ok_or(anyhow!("(uninit)"))?;

        if let CachedValue::Cached(v) = *cell_val.cache.borrow() {
            return Ok(v);
        }
        let computed = cell_val.formula.compute_cell_value(self);
        match computed {
            Ok(v) => (*cell_val.cache.borrow_mut()) = CachedValue::Cached(v),
            _ => {} // TODO: Can also cache errors
        };
        return computed;
    }

    pub fn set_val(&mut self, id: &Id, formula: Box<dyn CellFormula<Val>>) -> Result<()> {
        self.max_row = self.max_row.max(id.0);
        self.max_col = self.max_col.max(id.1);
        /*
        1. Speculatively update the spreadsheet with the new rdeps that this update would create.
        2. Transitively visit all of the rdeps of the cell we're trying to update.
        2a. If we reach the cell we're trying to update would become a transitive reverse dep of it self, that means it's also a dep of itself. This is invalid. Roll back the speculative change to rdeps, and return an error.
        2b. If we don't reach the cell we're trying to update, no cycles are being created. Don't roll back the speculative change to rdeps. Instead, mark all of the transitive rdeps of this cell as dirty.
        3. Add this cell and mark it as dirty.
         */

        if let Some(rdeps) = formula.ref_cells() {
            for d in rdeps {
                self.rdeps.entry(d).or_default().insert(*id);
            }
        };

        match self.check_cycles_or_mark_descendants_as_dirty(id) {
            Ok(_) => {
                if let Some(current) = self.vals.get(id) {
                    // If the cell is currently a Sum, then it's about to stop depending on two other cells.
                    // Update the other cell's rdep set to remove the cell we're mutating.
                    if let Some(rdeps) = current.formula.ref_cells() {
                        for rdep in rdeps {
                            self.rdeps.entry(rdep).and_modify(|set| {
                                set.remove(&id);
                            });
                        }
                    }
                }

                self.vals.insert(
                    *id,
                    GridCell {
                        cache: RefCell::new(CachedValue::Dirty),
                        formula,
                    },
                );
            }
            Err(_) => {
                // We may have speculatively added some rdeps to check if they'd create cycles.
                // If we got here, it means they did, so we have to roll back.
                if let Some(rdeps) = formula.ref_cells() {
                    for d in rdeps {
                        self.rdeps.entry(d).or_default().remove(id);
                    }
                }
                bail!("Did not set value, as doing so would have created a dependency cycle.");
            }
        }

        Ok(())
    }
}

impl Display for Spreadsheet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:^12}|", "")?;
        for i in 0..=self.max_col {
            write!(f, "{:^12}|", i)?; // col header
        }
        write!(f, "\n")?;
        write!(f, "{}", "-".repeat(13 * (self.max_col + 2)))?;
        write!(f, "\n")?;
        for i in 0..=self.max_row {
            write!(f, "{:^12}|", i)?; // row header
            for j in 0..=self.max_col {
                match self.get(&(i, j)) {
                    Ok(v) => write!(f, "{:^12}|", v)?,
                    Err(e) => write!(f, "{:^12}|", e.to_string())?,
                };
            }
            write!(f, "\n")?;
        }
        Ok(())
    }
}
fn main() -> Result<()> {
    fn lit(v: Val) -> Box<dyn CellFormula<Val>> {
        Box::new(Literal(v))
    }
    let mut spreadsheet = Spreadsheet::new();
    for i in 0..=10 {
        for j in 0..=10 {
            spreadsheet.set_val(&(i, j), lit(i * j))?;
        }
    }
    for i in 0..2 {
        spreadsheet.set_val(&(0, i), lit(1))?;
    }
    for i in 2..=10 {
        spreadsheet.set_val(&(0, i), Box::new(Sum2((0, i - 1), (0, i - 2))))?;
    }

    println!("{}", spreadsheet);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    fn lit(v: Val) -> Box<dyn CellFormula<Val>> {
        Box::new(Literal(v))
    }
    fn sum(id1: Id, id2: Id) -> Box<dyn CellFormula<Val>> {
        Box::new(Sum2(id1, id2))
    }

    #[test]
    fn basic_test() -> Result<()> {
        let mut actual = Spreadsheet::new();
        for i in 0..=10 {
            for j in 0..=10 {
                let id = (i, j);
                let value = i + j;
                actual.set_val(&id, lit(value))?;
            }
        }
        for i in 0..=10 {
            for j in 0..=10 {
                let id = (i, j);
                assert_eq!(actual.get(&id)?, i + j);
            }
        }
        for i in 0..=10 {
            let id = (i, 0);
            actual.set_val(&id, lit(i * 20))?;
        }
        for i in 0..=10 {
            let id = (i, 0);
            assert_eq!(actual.get(&id)?, i * 20);
        }

        for i in 0..=10 {
            let id = (i, 0);
            actual.set_val(&id, Box::new(Sum2((i, 1), (i, 2))))?;
        }
        for i in 0..=10 {
            let id = (i, 0);
            let expected = actual.get(&(i, 1))? + actual.get(&(i, 2))?;
            let actual_val = actual.get(&id)?;
            assert_eq!(actual_val, expected);
        }
        Ok(())
    }

    #[test]
    fn fibonnaci_test() -> Result<()> {
        let mut actual = Spreadsheet::new();
        actual.set_val(&(0, 0), lit(0))?;
        actual.set_val(&(1, 0), lit(1))?;
        for i in 2..=1000 {
            // fib(1000) is an enormous number. If we were eager computing it this test would never finish.
            // So implicitly we are testing that cell computation is lazy.
            actual.set_val(&(i, 0), sum((i - 1, 0), (i - 2, 0)))?;
        }

        let val = actual.get(&(57, 0));
        assert!(val.is_ok());
        assert_eq!(val.unwrap(), 365435296162);
        Ok(())
    }

    #[test]
    fn cycle_detection_test() -> Result<()> {
        let mut actual = Spreadsheet::new();
        actual.set_val(&(0, 0), lit(0))?;
        actual.set_val(&(1, 0), lit(1))?;
        for i in 2..=10 {
            actual.set_val(&(i, 0), sum((i - 1, 0), (i - 2, 0)))?;
        }

        for i in 0..=10 {
            actual
                .get(&(i, 0))
                .expect("Should have a value for all cells [0, 10]");
            // Force computation so everything is cached.
            // This is to validate that attempting to add a cycle doesn't dirty anything.
        }
        for i in 0..=10 {
            let id = (i, 0);

            match *actual.vals.get(&id).unwrap().cache.borrow() {
                CachedValue::Cached(_) => {}
                _ => assert!(false, "Something was dirty but all should be cached."),
            }
            // Force computation so everything is cached.
            // This is to validate that attempting to add a cycle doesn't dirty anything.
        }

        // 10 transitively depends on 0. So if 0 were to start depending on 10, that'd be a cycle.
        let result = actual.set_val(&(0, 0), sum((10, 0), (11, 0)));
        for i in 0..=10 {
            let id = (i, 0);

            match *actual.vals.get(&id).unwrap().cache.borrow() {
                CachedValue::Cached(_) => {}
                _ => assert!(false, "Something was dirty but all should be cached."),
            }
            // Force computation so everything is cached.
            // This is to validate that attempting to add a cycle doesn't dirty anything.
        }
        assert!(result.is_err());
        Ok(())
    }

    #[test]
    fn rdeps_correct() -> Result<()> {
        fn fib(i: usize) -> usize {
            match i {
                0 => 0,
                1 => 1,
                i => fib(i - 1) + fib(i - 2),
            }
        }
        let mut actual = Spreadsheet::new();
        actual.set_val(&(0, 0), lit(0))?;
        actual.set_val(&(1, 0), lit(1))?;
        for i in 2..=10 {
            actual.set_val(&(i, 0), sum((i - 1, 0), (i - 2, 0)))?;
        }
        for i in 0..=10 {
            assert_eq!(actual.get(&(i, 0))?, fib(i));
        }
        // 2 depends on 0,1.
        // but 1 does not depend on 0, because it is hard-coded to be 1.
        let cell2 = HashSet::from([(2, 0)]);
        assert_eq!(*actual.rdeps.get(&(0, 0)).unwrap(), cell2);

        for i in 1usize..=8 {
            let rdeps = actual.rdeps.get(&(i, 0));
            assert!(rdeps.is_some());
            let expected = HashSet::from([(i + 1, 0), (i + 2, 0)]);
            assert_eq!(*rdeps.unwrap(), expected);
        }

        actual.set_val(&(2, 0), lit(42))?;
        // Now, 0 and 1 should not have 2 in their rdep set.
        assert_eq!(*actual.rdeps.get(&(0, 0)).unwrap(), HashSet::new());
        assert_eq!(*actual.rdeps.get(&(1, 0)).unwrap(), HashSet::from([(3, 0)]));

        for i in 3..=10 {
            let id = (i, 0);
            actual.get(&id)?;
        }
        Ok(())
    }

    #[test]
    fn dirty_recompute_test() -> Result<()> {
        let fib55 = 139583862445usize;
        let fib57 = 365435296162usize;

        let mut actual = Spreadsheet::new();
        actual.set_val(&(0, 0), lit(0))?;
        actual.set_val(&(1, 0), lit(1))?;
        for i in 2..=60 {
            actual.set_val(&(i, 0), sum((i - 1, 0), (i - 2, 0)))?;
        }

        let val = actual.get(&(57, 0));
        assert!(val.is_ok());
        assert_eq!(val.unwrap(), fib57);

        actual.set_val(&(56, 0), lit(0))?;

        assert_eq!(actual.get(&(57, 0)).unwrap(), fib55); // fib57 = fib55 + fib56 = fib55 + 0
        Ok(())
    }
}
