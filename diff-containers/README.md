# diff-containers

This package implements difference datatypes for container types. Currently,
there is only one such implemented difference datatype for `Data.Map.Strict`.

## Differences on `Data.Map`s

A difference datatype for maps is defined and implemented in the
`Data.Map.Diff.Strict` module. A key property of our definition of diffs is that
diffs can not only be combined/summed, they can also be subtracted. Our
definition of diffs is not only a
[semigroup](https://en.wikipedia.org/wiki/Semigroup#Definition) and a
[monoid](https://en.wikipedia.org/wiki/Monoid#Definition), it is also a
[cancellative semigroup](https://en.wikipedia.org/wiki/Cancellative_semigroup).
Note that diffs are not a commutative semigroup.

### Simple overview

A difference type keeps track of whether a value is to be inserted or deleted at
a specific key. At the same time, we also keep track of the history of changes
for specific keys using a diff history, the right-most element of the history
being the latest change. As we will see later, this is a prerequisite for diffs
to be a cancellative semigroup.

```haskell
newtype Diff k v = Diff (Map k (NEDiffHistory v))
newtype NEDiffHistory v = NEDiffHistory { getNEDiffHistory :: NESeq (DiffEntry v) }
data DiffEntry v =
      Insert !v
    | Delete !v
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
applyDiff :: Ord k => Map k v -> Diff k v -> Map k v

property :: Map k v -> Map k v -> Bool
property = applyDiff m1 (diff m1 m2) == m2
```

### Diffs are cancellative monoids

A straightfoward way to represent differences on maps is to keep track of the
most recent change for each key in the map.

```haskell
type Diff_ k v = Diff_ (Map k (Delta_ v))
data Delta_ =
    I !v -- ^ Insert
  | D !v -- ^ Delete
```

Diffs can then be summed through a `Semigroup`/`Monoid` instance that behaves
like a pair-wise `Last` monoid.

```haskell
   Diff (fromList [(1, I 54), (2, D 42)          ])
<> Diff (fromList [           (2, I 17), (3, D 1)])
== Diff (fromList [(1, I 54), (2, I 17), (3, D 1)])
```

What if we want to undo changes in a diff, like undoing the most recent `I 17`
change to have `D 42` once again? In this simple definition, there is no way to
recover information about previous changes, since we only track the last/most
recent change, so we are stuck.

The trick is to store a *history* of deltas (`DiffEntry`) for each key. When we
undo changes, we remove deltas from histories. The way in which this
"subtraction" of deltas is facilitated is by defining an instance for the
`LeftReductive`/`LeftCancellative` and `RightReductive`/`RightCancellative`
classes by implementing the `stripPrefix` and `stripSuffix` functions. Consider
this simplified example of stripping prefixes/suffixes for diff histories
(printed as lists).

```haskell
h1 = [D 42]
h2 = [I 17]
h3 = stripSuffix h2 (h1 <> h2) == Just h1
h4 = stripPrefix h1 (h1 <> h2) == Just h2
```

Stripping prefixes/suffixes of diffs follows from pairwise stripping
prefixes/suffixes of diff histories. As such, diffs have `LeftReductive`,
`LeftCancellative`, `RightReductive` and `RightCancellative` instances as well.
See this example:

```haskell
let a = Diff (fromList [(1, [I 54]), (2, [D 42])            ])
    b = Diff (fromList [             (2, [I 17]), (3, [D 1])])
    c = Diff (fromList [             (2, [I 17])            ])
    d = Diff (fromList [(1, [I 54]), (2, [D 42]), (3, [D 1])])
in  a <> b `stripSuffix` c == d
```