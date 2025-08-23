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

    fn map<SResult, CResult>(
        self,
        f: impl FnOnce(Self, State) -> (CResult, SResult),
    ) -> StatefulCursor<SResult, CResult> {
        let state = self.state().clone();
        let (cursor, state) = f(self, state);
        StatefulCursor { state, cursor }
    }

    fn map_state<SResult>(self, f: impl FnOnce(State) -> SResult) -> StatefulCursor<SResult, Self> {
        self.map(|cursor, state| (cursor, f(state)))
    }

    fn map_cursor<CResult>(
        self,
        f: impl FnOnce(Self) -> CResult,
    ) -> StatefulCursor<State, CResult> {
        self.map(|cursor, state| (f(cursor), state))
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
        let additional = state;
        self.map_state(move |state| f(state, additional))
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

impl<SOutter, SInner, C> StatefulCursor<SOutter, StatefulCursor<SInner, C>> {
    fn flatten(self) -> StatefulCursor<(SOutter, SInner), C> {
        StatefulCursor {
            state: (self.state, self.cursor.state),
            cursor: self.cursor.cursor,
        }
    }

    fn flatten_with<SResult>(
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
