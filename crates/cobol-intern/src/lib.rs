use smol_str::SmolStr;
use std::collections::HashMap;
use std::fmt;

/// An interned, case-insensitive string identifier.
///
/// Internally stores a `u32` index into the [`Interner`]'s storage.
/// All COBOL names are normalized to uppercase at intern time, so
/// comparisons are O(1) index equality checks.
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct Name(u32);

impl Name {
    /// Returns the raw index of this interned name.
    #[inline]
    pub fn raw(self) -> u32 {
        self.0
    }
}

impl fmt::Debug for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Name({})", self.0)
    }
}

/// Owns all interned strings and provides deduplication.
///
/// Strings are normalized to uppercase on insertion so that
/// `intern("foo")` and `intern("FOO")` yield the same [`Name`].
#[derive(Debug, Default)]
pub struct Interner {
    /// Maps uppercase strings to their assigned [`Name`].
    map: HashMap<SmolStr, Name>,
    /// Indexed by [`Name::raw()`]; stores the canonical uppercase form.
    strings: Vec<SmolStr>,
}

impl Interner {
    /// Creates a new, empty interner.
    pub fn new() -> Self {
        Self::default()
    }

    /// Interns a string, normalizing it to uppercase.
    ///
    /// If the uppercase form has already been interned, the existing
    /// [`Name`] is returned. Otherwise a new [`Name`] is allocated.
    pub fn intern(&mut self, s: &str) -> Name {
        let upper: SmolStr = s.to_uppercase().into();
        if let Some(&name) = self.map.get(&upper) {
            return name;
        }
        let idx = self.strings.len() as u32;
        let name = Name(idx);
        self.strings.push(upper.clone());
        self.map.insert(upper, name);
        name
    }

    /// Resolves an interned [`Name`] back to its canonical (uppercase) string.
    ///
    /// # Panics
    ///
    /// Panics if `name` was not produced by this interner.
    pub fn resolve(&self, name: Name) -> &str {
        &self.strings[name.0 as usize]
    }

    /// Returns the number of unique strings currently interned.
    pub fn len(&self) -> usize {
        self.strings.len()
    }

    /// Returns `true` if no strings have been interned.
    pub fn is_empty(&self) -> bool {
        self.strings.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn case_insensitive_interning() {
        let mut interner = Interner::new();
        let a = interner.intern("foo");
        let b = interner.intern("FOO");
        let c = interner.intern("Foo");
        assert_eq!(a, b);
        assert_eq!(b, c);
        assert_eq!(interner.len(), 1);
    }

    #[test]
    fn resolve_returns_uppercase() {
        let mut interner = Interner::new();
        let name = interner.intern("hello-world");
        assert_eq!(interner.resolve(name), "HELLO-WORLD");
    }

    #[test]
    fn different_strings_yield_different_names() {
        let mut interner = Interner::new();
        let a = interner.intern("MOVE");
        let b = interner.intern("ADD");
        assert_ne!(a, b);
        assert_eq!(interner.len(), 2);
    }

    #[test]
    fn empty_interner() {
        let interner = Interner::new();
        assert!(interner.is_empty());
        assert_eq!(interner.len(), 0);
    }
}
