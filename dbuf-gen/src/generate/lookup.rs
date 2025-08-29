#[derive(Debug)]
pub enum NavigateResult<Key, Result> {
    GoBack,
    GoNext(Key),
    Stop(Result),
}

pub trait Accessor<State> {
    fn state(&self) -> State;
}

pub(super) trait Walker<Key> {
    fn walk_next(&mut self, key: &Key) -> bool;

    fn walk_back(&mut self) -> bool;
}

pub trait CursorBase<Key, State>: Walker<Key> + Accessor<State> {
    fn clone_boxed(&self) -> Box<dyn CursorBase<Key, State> + '_>;
}

pub trait Cursor<Key, State: Clone>: Sized + CursorBase<Key, State> {
    fn next(mut self, key: &Key) -> Option<Self> {
        self.walk_next(key).then_some(self)
    }

    fn back(mut self) -> Option<Self> {
        self.walk_back().then_some(self)
    }

    fn map<CResult>(self, f: impl FnOnce(Self) -> CResult) -> CResult {
        f(self)
    }

    fn with_state<SAdd>(self, state: SAdd) -> StatefulCursor<SAdd, Self> {
        StatefulCursor {
            state,
            cursor: self,
        }
    }

    fn map_with_state<SAdd: Clone, SResult: Clone>(
        self,
        state: SAdd,
        f: impl FnOnce(State, SAdd) -> SResult,
    ) -> StatefulCursor<SResult, Self>
    where
        Self: Clone,
    {
        self.map(move |cursor| StatefulCursor {
            state: f(cursor.state(), state),
            cursor,
        })
    }

    fn navigate<O, N: Navigator<Key, Self, O>>(
        mut self,
        mut navigator: N,
    ) -> Option<StatefulCursor<O, Self>> {
        let result = loop {
            match navigator.decide(&mut self) {
                NavigateResult::GoBack => self = self.back()?,
                NavigateResult::GoNext(id) => self = self.next(&id)?,
                NavigateResult::Stop(result) => break result,
            };
        };

        Some(self.with_state(result))
    }
}

impl<K, S: Clone, C: Sized + CursorBase<K, S>> Cursor<K, S> for C {}

pub trait Navigator<Key, Cursor: Walker<Key>, Result> {
    fn decide(&mut self, cursor: &Cursor) -> NavigateResult<Key, Result>;
}

impl<K, C, Result, F> Navigator<K, C, Result> for F
where
    F: FnMut(&C) -> NavigateResult<K, Result>,
    C: Walker<K>,
{
    fn decide(&mut self, cursor: &C) -> NavigateResult<K, Result> {
        self(cursor)
    }
}

#[derive(Clone, Debug)]
pub struct StatefulCursor<State, Cursor> {
    state: State,
    cursor: Cursor,
}

impl<S, C> StatefulCursor<S, C> {
    pub fn cursor(self) -> C {
        self.cursor
    }
}

impl<S: Clone, C> StatefulCursor<S, C> {
    pub fn into_parts(self) -> (S, C) {
        (self.state, self.cursor)
    }

    pub fn into_state(self) -> S {
        self.state
    }

    pub fn into_cursor(self) -> C {
        self.cursor
    }

    pub fn map_parts<K, SResult, CResult>(
        self,
        f: impl FnOnce(S, C) -> (SResult, CResult),
    ) -> StatefulCursor<SResult, CResult>
    where
        Self: Cursor<K, S>,
    {
        self.map(|cursor| {
            let (state, cursor) = cursor.into_parts();
            let (state, cursor) = f(state, cursor);
            StatefulCursor { state, cursor }
        })
    }

    pub fn map_state<K, SResult>(self, f: impl FnOnce(S) -> SResult) -> StatefulCursor<SResult, C>
    where
        Self: Cursor<K, S>,
    {
        self.map_parts(|state, cursor| (f(state), cursor))
    }

    pub fn map_cursor<K, CResult>(self, f: impl FnOnce(C) -> CResult) -> StatefulCursor<S, CResult>
    where
        Self: Cursor<K, S>,
    {
        self.map_parts(|state, cursor| (state, f(cursor)))
    }
}

impl<SOutter, SInner, C> StatefulCursor<SOutter, StatefulCursor<SInner, C>> {
    pub fn flatten(self) -> StatefulCursor<(SOutter, SInner), C> {
        StatefulCursor {
            state: (self.state, self.cursor.state),
            cursor: self.cursor.cursor,
        }
    }

    pub fn flatten_with<SResult>(
        self,
        f: impl FnOnce(SOutter, SInner) -> SResult,
    ) -> StatefulCursor<SResult, C> {
        StatefulCursor {
            state: f(self.state, self.cursor.state),
            cursor: self.cursor.cursor,
        }
    }
}

impl<K, S, C: Walker<K>> Walker<K> for StatefulCursor<S, C> {
    fn walk_next(&mut self, key: &K) -> bool {
        self.cursor.walk_next(key)
    }

    fn walk_back(&mut self) -> bool {
        self.cursor.walk_back()
    }
}

impl<S: Clone, C> Accessor<S> for StatefulCursor<S, C> {
    fn state(&self) -> S {
        self.state.clone()
    }
}

impl<K, S: Clone, C: Walker<K> + Clone> CursorBase<K, S> for StatefulCursor<S, C> {
    fn clone_boxed(&self) -> Box<dyn CursorBase<K, S> + '_> {
        Box::new(self.clone())
    }
}

// #[derive(Debug)]
pub struct DynCursor<Key, State> {
    cursor: Box<dyn CursorBase<Key, State>>,
}

impl<K, S> Walker<K> for DynCursor<K, S> {
    fn walk_next(&mut self, key: &K) -> bool {
        self.cursor.walk_next(key)
    }

    fn walk_back(&mut self) -> bool {
        self.cursor.walk_back()
    }
}
