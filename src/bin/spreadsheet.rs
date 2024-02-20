use anyhow::Result;
use thiserror::Error;

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Display};

type Val = usize;
type Id = (usize, usize); // (row, col)
type CellLookup = HashMap<Id, HashSet<Id>>;

#[derive(Debug, Clone, Copy, PartialEq)]
enum CachedValue {
    Cached(Val),
    Dirty,
    Invalid,
}

#[derive(Debug)]
struct GridCell {
    cache: RefCell<CachedValue>,
    formula: CellFormulas,
}

pub trait CellFormula {
    fn ref_cells(&self) -> Option<HashSet<Id>>;
    fn compute_cell_value(&self, sheet: &Spreadsheet) -> std::result::Result<Val, AccessError>;
}

pub trait CustomCellFormula: CellFormula + Debug {}
impl<T: CellFormula + Debug> CustomCellFormula for T {}

#[derive(Debug)]
pub enum CellFormulas {
    Literal(Val),
    Sum2(Id, Id),
    Custom(Box<dyn CustomCellFormula>),
}

#[derive(Error, Debug)]
pub enum SetError {}

#[derive(Error, Debug)]
pub enum AccessError {
    #[error("Cannot compute through a reference cycle.")]
    CyclicReference,
    #[error("Cell is uninitialized")]
    Uninitialized,
    #[error("Reference to an invalid cell.")]
    InvalidReference,
}

impl CellFormula for CellFormulas {
    fn ref_cells(&self) -> Option<HashSet<Id>> {
        use CellFormulas::*;

        match self {
            Literal(_) => None,
            Sum2(id1, id2) => Some(HashSet::from([*id1, *id2])),
            Custom(b) => b.ref_cells(),
        }
    }
    fn compute_cell_value(&self, sheet: &Spreadsheet) -> std::result::Result<Val, AccessError> {
        use AccessError::*;
        use CellFormulas::*;
        match self {
            Literal(v) => Ok(*v),
            Sum2(id1, id2) => Ok(sheet.get(id1).map_err(|_| InvalidReference)?
                + sheet.get(id2).map_err(|_| InvalidReference)?),
            Custom(b) => b.compute_cell_value(sheet),
        }
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

NOTE: The solution here slightly generalizes the problem statement. It handles invalid and cyclic references.
Additionally, it has support for arbitrary user-defined formulas, not just summing two numbers.

*/
pub struct Spreadsheet {
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

    pub fn get(&self, id: &Id) -> std::result::Result<Val, AccessError> {
        let cell_val = self.vals.get(id).ok_or(AccessError::Uninitialized)?;

        match *cell_val.cache.borrow() {
            CachedValue::Cached(v) => return Ok(v),
            CachedValue::Invalid => return Err(AccessError::CyclicReference),
            _ => {}
        };
        let computed = cell_val.formula.compute_cell_value(self);
        match computed {
            Ok(v) => (*cell_val.cache.borrow_mut()) = CachedValue::Cached(v),
            _ => {} // TODO: Can also cache errors
        };
        return computed;
    }

    pub fn set_val(&mut self, id: &Id, formula: CellFormulas) -> std::result::Result<(), SetError> {
        self.max_row = self.max_row.max(id.0);
        self.max_col = self.max_col.max(id.1);
        /*
        1.Update the spreadsheet with the new rdeps that will exist after this cell is updated.
        2. Transitively visit all of the rdeps of the cell we're starting from.
        3. Note if we reached back to the starting point during the traversal.
        3a. If we reached the starting point, we just traveled a dep cycle. Mark every cell in the cycle as a CyclicReference.
        3b. If we didn't reach the starting point, the cells are normal. Mark all the rdeps as dirty.
        4. Mark the starting cell as either CyclicReference or Dirty.
        TODO: other cells might still be cyclic
         */
        if let Some(current) = self.vals.get(id) {
            // If the cell we're updating referenced other cells,
            // it no longer necessarily references those same cells now.
            if let Some(rdeps) = current.formula.ref_cells() {
                for rdep in rdeps {
                    self.rdeps.entry(rdep).and_modify(|set| {
                        set.remove(&id);
                    });
                }
            }
        }
        if let Some(rdeps) = formula.ref_cells() {
            for d in rdeps {
                self.rdeps.entry(d).or_default().insert(*id);
            }
        };

        let has_cycle = self.check_for_cycle_and_dirty_rdeps(id);

        let value = match has_cycle {
            true => CachedValue::Invalid,
            false => CachedValue::Dirty,
        };
        self.vals.insert(
            *id,
            GridCell {
                cache: RefCell::new(value),
                formula,
            },
        );
        Ok(())
    }

    // Recursively dirty the rdeps of id. If there is a dependency cycle, return true, else false.
    fn check_for_cycle_and_dirty_rdeps(self: &Self, id: &Id) -> bool {
        let mut seen = HashSet::new();

        // Note: deliberately not add the starting cell id to the list of visited cells.
        // We know we're starting from it. We need to find out if we can also reach
        // it again via travelling through all of its rdeps. Such a state indicates a dep cycle.
        // In that case, the update will be rejected, and the state shouldn't change. Nothing
        // should be marked as dirty.
        self.update_rdeps_recursive(id, &mut seen);

        let update_value = if seen.contains(id) {
            CachedValue::Invalid
        } else {
            CachedValue::Dirty
        };

        if seen.contains(id) {
            let mut invalid: Vec<usize> = seen.iter().map(|(v, _)| *v).collect();
            invalid.sort();
        }

        for id in &seen {
            let cell = self.vals.get(id).unwrap();
            *cell.cache.borrow_mut() = update_value;
        }
        if seen.contains(id) {
            return true;
        }
        return false;
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
                    Err(e) => {
                        use AccessError::*;
                        match e {
                            InvalidReference => write!(f, "{:^12}|", "#REF")?,
                            Uninitialized => write!(f, "{:^12}|", "")?,
                            CyclicReference => write!(f, "{:^12}|", "#CYCLE")?,
                        }
                    }
                };
            }
            write!(f, "\n")?;
        }
        Ok(())
    }
}
fn main() -> Result<()> {
    use CellFormulas::*;
    fn lit(v: Val) -> CellFormulas {
        Literal(v)
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
        spreadsheet.set_val(&(0, i), Sum2((0, i - 1), (0, i - 2)))?;
    }

    spreadsheet.set_val(&(5, 5), Sum2((3, 3), (7, 7)))?;
    spreadsheet.set_val(&(7, 7), Sum2((2, 2), (4, 4)))?;
    spreadsheet.set_val(&(4, 4), Sum2((2, 2), (5, 5)))?;
    spreadsheet.set_val(&(7, 3), Sum2((50, 50), (0, 0)))?;

    println!("{}", spreadsheet);

    spreadsheet.set_val(&(4, 4), Sum2((0, 0), (1, 1)))?;

    println!("{}", spreadsheet);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    fn lit(v: Val) -> CellFormulas {
        CellFormulas::Literal(v)
    }
    fn sum(id1: Id, id2: Id) -> CellFormulas {
        CellFormulas::Sum2(id1, id2)
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
            actual.set_val(&id, CellFormulas::Sum2((i, 1), (i, 2)))?;
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
            // fib(1000) is an enormous number. If we were eager computing it this test would never finish or would panic on overflow.
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
                _ => assert!(false, "cell {:?} wasn't cached, but should have been", id),
            }
        }

        // 9 and 10 transitively depend on 0. So if 0 were to start depending on those, that'd be a cycle.
        actual.set_val(&(0, 0), sum((10, 0), (9, 0)))?;

        assert_eq!(
            *actual.vals.get(&(0, 0)).unwrap().cache.borrow(),
            CachedValue::Invalid
        );
        // 1 is still cached because it doesn't reference anything.
        assert_eq!(
            *actual.vals.get(&(1, 0)).unwrap().cache.borrow(),
            CachedValue::Cached(1)
        );

        for i in 2..=10 {
            let id = (i, 0);
            assert_eq!(
                *actual.vals.get(&id).unwrap().cache.borrow(),
                CachedValue::Invalid
            );
        }

        // Fixing one of two broken refs shouldn't fix the cycle.
        actual.set_val(&(9, 0), lit(42))?;
        // ERROR: this marks the cycle as dirty, but it shouldn't.
        // The cycle is still invalid, but the detection logic only sees that 9 is now fine and doesn't realize that the other nodes are still in a cycle.

        for i in 2..=10 {
            let id = (i, 0);
            assert_eq!(
                *actual.vals.get(&id).unwrap().cache.borrow(),
                CachedValue::Invalid
            );
            match actual.get(&id) {
                Ok(_) => assert!(false, "{:?}: shouldn't be fixed yet", id),
                Err(_) => {}
            };
        }

        // this should fix the cycle. Now make sure everything is valid again.
        actual.set_val(&(10, 0), lit(42))?;
        // actual.set_val(&(0, 0), lit(42))?;
        for i in 0..=10 {
            let id = (i, 0);

            match actual.get(&id) {
                Ok(_) => {}
                Err(e) => assert!(false, "{:?}: Got error {:?}, but shouldn't have", id, e),
            };
        }

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

    #[test]
    fn custom_formula() -> Result<()> {
        #[derive(Debug)]
        struct Square(Id);
        impl CellFormula for Square {
            fn compute_cell_value(
                &self,
                sheet: &Spreadsheet,
            ) -> std::result::Result<Val, AccessError> {
                let v = sheet
                    .get(&self.0)
                    .map_err(|_| AccessError::InvalidReference)?;
                Ok(v * v)
            }

            fn ref_cells(&self) -> Option<HashSet<Id>> {
                let mut set = HashSet::new();
                set.insert(self.0);
                Some(set)
            }
        }
        let mut actual = Spreadsheet::new();

        for i in 0..=10 {
            let row1 = (i, 0);
            let row2 = (i, 1);
            actual.set_val(&row1, lit(i))?;
            actual.set_val(&row2, CellFormulas::Custom(Box::new(Square(row1))))?;
        }
        for i in 0..=10 {
            assert_eq!(actual.get(&(i, 1)).unwrap(), i * i);
        }

        Ok(())
    }
}
