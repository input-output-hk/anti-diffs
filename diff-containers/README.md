# diff-containers

This package implements difference datatypes for container types. Currently,
there is only one such implemented difference datatype for `Data.Map.Strict`.

## Differences on `Data.Map`s

A difference datatype for maps is defined and implemented in the
`Data.Map.Diff.Strict` module. A key property of our definition of diffs is that
diffs can not only be combined/summed, they can also be inverted and summed
together, which facilitates *subtraction* of diffs. Our definition of diffs is
not only a `Semigroup` and a `Monoid`, it is also a `Group`.

The goals for our definition of diffs are twofold:
1. Provide definitions that adhere to the type class laws for `Semigroup`,
   `Monoid` and `Group`.
2. Applying diffs to maps should have a nice semantics.

An overview of the sections in this document:

* Section [1) High-level overview](#high-level-overview) provides a high-level
  overview of diffs, how to create them, and how to apply them.
* Section [2) A first attempt at implementing
  diffs](#2-a-first-attempt-at-implementing-diffs) describes a straightforward
  way to implement diffs, and where this goes wrong if we try to define a
  `Group` instance.
* Section [3) Implementing diffs with
  inverses](#3-implementing-diffs-with-inverses) describes how we implement
  diffs to facilitate a `Group` instance.
* Section [4) Applying diffs with inverses: positivity and
  normality](#4-applying-diffs-with-inverses-positivity-and-normality) describes
  how applying diffs can fail, but which can be solved by guaranteeing the
  positivity and normality properties.

### 1) High-level overview

For each of the key-value pairs in a map, the difference type will keep track of
whether a value is to be inserted or deleted:

```haskell
newtype Diff k v = Diff {- omitted -}

data DiffEntry v =
    Insert !v
  | Delete !v
  {- omitted for now -}
```

One method of constructing a diff is to compute the difference between two maps.
Other methods include creating diffs from maps and lists directly:

```haskell
diff :: (Ord k, Eq v) => Map k v -> Map k v -> Diff k v

fromMap :: Map k (DiffEntry v) -> Diff k v
fromList :: Ord k => [(k, DiffEntry v)] -> Diff k v
```

Diffs can be applied to maps to obtain new maps. For diffs constructed using
`diff`, the following `property` also holds:

```haskell
unsafeApplyDiff :: Ord k => Map k v -> Diff k v -> Map k v

property :: Map k v -> Map k v -> Bool
property = unsafeApplyDiff m1 (diff m1 m2) == m2
```

### 2) A first attempt at implementing diffs

A straightfoward to represent differences on maps is to keep track of the most
recent change for each key in the map.

```haskell
type Diff_ k v = Diff_ (Map k (Delta_ v))
data Delta_ = Insert_ !v | Delete_ !v
```

Diffs can then be summed (or combined/`mappend`ed) through a
`Semigroup`/`Monoid` instance that behaves like a pair-wise `Last` monoid.

```haskell
   fromList [(1, Insert_ 54), (2, DDelete 42)                ]
<> fromList [                 (2, Insert_ 17), (3, DDelete 1)]
== fromList [(1, Insert_ 54), (2, Insert_ 17), (3, DDelete 1)]
```

What if we want to remove diffs, i.e. through subtraction provided by a `Group`
instance? In the current definition, there is no way to recover information
about previous changes, since we only have the last/most recent change. In
particular, we would like to be able to have the following:

```haskell
   fromList [(1, Insert_ 54), (2, DDelete 42)                ]
<> fromList [                 (2, Insert_ 17), (3, DDelete 1)]
~~ fromList [                 (2, Insert_ 17), (3, DDelete 1)]
== fromList [(1, Insert_ 54), (2, DDelete 42)                ]
```

The current definition of diffs is not sufficient, however.

### 3) Diffs with inverses

The trick is to store a *history* of deltas for each key. When we subtract diffs
from one another, we remove deltas from histories. How exactly deltas are
removed, we will touch upon later. For now, these are the (simplified) types
that make up a diff:

```haskell
-- Note: this is a simplified representation. In reality, we enforce statically
-- that diff histories in a @'Diff'@ are non-empty.
newtype Diff k v = Diff (Map k (DiffHistory v))

newtype DiffHistory v = DiffHistory { getDiffHistory :: Seq (DiffEntry v) }

data DiffEntry v =
    Insert !v
  | Delete !v
  {- omitted for now -}
```

The way in which subtraction is facilitated is by implementing the `invert`
method in a `Group` instance. As a consequence, not only do we have to add
inverses for diffs, diff histories and diff entries, we must also define how
these inverses are handled by summing.

First, we start by adding inverse elements to the definition of `DiffEntry`. One
may think `Insert` and `Delete` are already inverses of one another, and that
would be correct in terms of adhering to the type class laws, but it would have
adverse effects on the semantics of applying diffs to maps. Consider what would
happen if we were to consider `Insert` and `Delete` inverses in the following
example. Ignore diff histories for now:

```haskell
d1 = fromList [(0, Delete 17)]
d2 = fromList [(0, Insert 42)]
d3 = d1 <> d2 = fromList []
```

Applying `d3` to a map would behave like the identity function. Applying `d1`
and `d2` consecutively would not behave like the identity function, as it would
map `0` to `42`. Through some abuse of the definition of distributivity, we can
say that applying diffs does not distribute over the summing of diffs.

Instead, we add explicit inverse constructors for both `Insert` and `Delete`. We
do not make `Semigroup`/`Monoid`/`Group` instances for diff entries, however. We
shall see that we only need these 4 constructors in order to provide the
instances for diff histories.

```haskell
data DiffEntry v =
    Insert !v
  | Delete !v
  | UnsafeAntiInsert !v -- ^ Unsafe!
  | UnsafeAntiDelete !v -- ^ Unsafe!
```

Inverse diff entries will *cancel out* when we sum diff histories. For example:

```haskell
h1 = fromList [Insert 1, UnsafeAntiDelete 2, UnsafeAntiInsert 3]
h2 = fromList [Delete 2, Insert 3, Insert 3]
h3 = h1 <> h2 = [Insert 1, Delete 2, Insert 3]
```

How do we invert diff histories? The type class laws that a `Group` instance
should adhere to give us a clue:

```haskell
-- left inverse
a <> invert a == mempty
-- right inverse
invert a <> a == mempty
```

Given that inverse diff entries cancel each other out, inverting a diff history should invert all diff entries and reverse their order. See this example:

```haskell
h        = fromList [Insert 1, Delete 2, Delete 3, Insert 4]
invert h = fromList [UnsafeAntiInsert 4, UnsafeAntiDelete 3, UnsafeAntiDelete 2, UnsafeAntiInsert 1]

invert h <> h == mempty
h <> invert h == mempty
```

Summing of diffs follows from pairwise summing the inner diff histories.
Inverting diffs follows from inverting each inner diff history.

### 4) Applying diffs with inverses: positivity and normality

It is an unfortunate side effect of facilitating a `Group` instance for diffs
that we can get into situations where applying diffs will fail or produce wrong
results due to diffs containing internally unresolved sums and subtractions. The
responsibility of downstream code is to ensure that diffs that are applied are
both *positive* and *normal*. If that is the case, then applying diffs will
never go wrong.

* Positivity: a positive diff contains only `Insert`s and `Delete`s.
* Normality: a normal diff has resolved all sums and subtrations internally,
  i.e., there are no consecutive diff entries in a diff history that can still
  be cancelled out.

Import to note: a positive diff is by definition also a normal diff. Normality
is a weaker property than positivity. However, normality is a sufficient
precondition for some definitions to work as expected (see the next paragraph),
in which cases positivity would be an unnecessarily strong requirement.

Normality is also a precondition for the type class laws to hold. This is an
optimisation: we could normalise diff histories exhaustively in each sum or
inversion operation, but that could be costly.

If positivity and normality are guaranteed, then `applyDiff` will always return a `Right` result, and `unsafeApplyDiff` will never throw an error.

```haskell
applyDiff :: Ord k => Map k v -> Diff k v -> Either () (Map k v)
unsafeApplyDiff :: Ord k => Map k v -> Diff k v -> Map k v
```

A number of definitions in the Haskell modules are annotated with PRECONDITION,
INVARIANT and POSTCONDITION. Use these and the type class laws for `Semigroup`,
`Monoid` and `Group` (which hold given preconditions) to ensure that applied
diffs are always both positive and normal. This should not be too complex

* Diffing maps creates diffs that are both positive and normal.
* Summing preverses normality and positivity.
* Empty diffs are both normal and positive.
* Inversion preserves normality.
* Subtraction preserves positivity if we only subtract diffs that we know are in
  a sum. For example:

  ```haskell
  (d1 <> ... <> di <> dj <> ... <> dn) <> invert (dj <> ... <> dn) == d1 <> ... <> di
  invert (d1 <> ... <> di) <> (d1 <> ... <> di <> dj <> ... <> dn) == dj <> ... <> dn
  ```

It is important not to forget that these sum operations are not commutative.
Summing/subtracting on the left is not the same as summing/subtracting on the
right.