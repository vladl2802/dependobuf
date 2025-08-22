#[derive(Debug)]
pub enum NavigateResult<Key, Result> {
    GoBack,
    GoNext(Key),
    Stop(Result),
}

pub trait Accessor<State> {
    fn state(self) -> State;
}

pub trait Walker<Key> {
    fn next(&mut self, key: &Key) -> bool;

    fn back(&mut self) -> bool;
}

pub trait CursorBase<Key, State>: Walker<Key> + Accessor<State> {}

pub trait Cursor<Key, State>: Sized + CursorBase<Key, State> {
    fn map<SResult, CResult>(
        self,
        f: impl FnOnce(Self, State) -> (CResult, SResult),
    ) -> StatefulCursor<SResult, CResult> {
        let (state, cursor) = f(self.state, self.cursor);
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

    fn map_with_state<SAdd, SResult>(
        self,
        state: SAdd,
        f: impl FnOnce(State, SAdd) -> SResult,
    ) -> StatefulCursor<SResult, Self> {
        self.with_state(state)
            .map(|cursor, state| f(cursor.state, state))
    }

    fn navigate<O, N: Navigator<Key, Self, O>>(
        mut self,
        mut navigator: N,
    ) -> Option<StatefulCursor<O, Self>> {
        let result = loop {
            match navigator.decide(self) {
                NavigateResult::GoBack => self.back().then(())?,
                NavigateResult::GoNext(id) => self.next(&id).then(())?,
                NavigateResult::Stop(result) => break result,
            }
        };

        Some(self.with_state(result))
    }
}

impl<K, S, C: CursorBase<K, S> + Sized> Cursor<K, S> for C {}

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
    fn next(&mut self, key: &K) -> bool {
        self.cursor.next(key)
    }

    fn back(&mut self) -> bool {
        self.cursor.back()
    }
}

impl<S, C> Accessor<S> for StatefulCursor<S, C> {
    fn state(self) -> S {
        self.state
    }
}

impl<K, S, C: Walker<K>> CursorBase<K, S> for StatefulCursor<S, C> {}

#[derive(Clone, Debug)]
pub struct DynCursor<Key> {
    cursor: Box<dyn Walker<Key>>,
}

impl<K> Walker<K> for DynCursor<K> {
    fn next(&mut self, key: &K) -> bool {
        self.cursor.next(key)
    }

    fn back(&mut self) -> bool {
        self.cursor.back()
    }
}
