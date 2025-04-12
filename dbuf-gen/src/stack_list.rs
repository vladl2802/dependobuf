pub enum StackList<'a, T> {
    Begin,
    Node {
        parent: &'a StackList<'a, T>,
        data: T,
    },
}

impl<'a, T> StackList<'a, T> {
    pub fn append(data: T, parent: &'a StackList<'a, T>) -> Self {
        StackList::Node { parent, data }
    }

    pub fn begin() -> Self {
        StackList::Begin
    }

    pub fn iter(&'a self) -> Iter<'a, T> {
        Iter { node: self }
    }
}

struct Iter<'a, T> {
    node: &'a StackList<'a, T>,
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        match &self.node {
            StackList::Begin => None,
            StackList::Node { parent, data } => {
                self.node = parent;
                Some(data)
            }
        }
    }
}
