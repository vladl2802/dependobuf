pub struct StackList<'a, T> {
    parent: Option<&'a StackList<'a, T>>,
    data: T,
}

impl<'a, T> StackList<'a, T> {
    pub fn new(data: T) -> Self {
        StackList { parent: None, data }
    }

    pub fn append_to(mut self, parent: &'a StackList<'a, T>) -> Self {
        assert!(self.parent.is_none(), "must not re-append");
        self.parent = Some(parent);
        self
    }

    pub fn iter(&'a self) -> Iter<'a, T> {
        Iter { node: Some(self) }
    }

    pub fn get(&self) -> &T {
        &self.data
    }

    pub fn get_mut(&mut self) -> &mut T {
        &mut self.data
    }
}

struct Iter<'a, T> {
    node: Option<&'a StackList<'a, T>>,
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        let value = self.node.map(|node| &node.data);
        self.node = self.node.map(|node| node.parent).flatten();
        value
    }
}

mod tests {
    use std::ops::Add;

    use super::StackList;

    #[test]
    fn basic() {
        type IntList<'a> = StackList<'a, i32>;

        let mut s0 = IntList::new(0);
        let mut s1 = IntList::new(1).append_to(&s0);
        let mut s2 = IntList::new(2).append_to(&s1);

        assert_eq!(s2.get().clone(), 2);
        s2.get_mut().add(1);
        assert_eq!(s2.get().clone(), 3);

        assert_eq!(s1.get().clone(), 1);
        s1.get_mut().add(10);
        assert_eq!(s1.get().clone(), 11);

        assert_eq!(s0.get().clone(), 0);
        s0.get_mut().add(100);
        assert_eq!(s0.get().clone(), 100);
    }
}
