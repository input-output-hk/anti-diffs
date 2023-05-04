# fingertree-rm

This package implements a variant of a finger tree that keeps track of a *root
measure*, which we call *root-measured finger trees*. Currently, there is only
one implementation for strict finger trees.

## Finger trees with root measures

A possible way to represent a sequence of monoidal sums of any monoid `v` is by
means of a finger tree, where the leaves contain elements of type `a` that can
be measured as `v`, and the intermediate nodes contain the cumulative sums of
the leaves.

```haskell
data StrictFingerTree v a = {- omitted -}

class Monoid v ⇒ Measured v a | a → v where
  measure :: a -> v
```

The root of the tree contains the cumulative sum of all the monoidal measures in
the finger tree.

```haskell
instance Measured v a => Measured v (FingerTree v a) where
  {- omitted -}
```

Because this representation stores intermediate sums in its nodes, we are
essentially "caching" intermediate sums, which allows for relatively fast
reconstruction of intermediate sums from cached intermediate sums as the
structure of the finger tree changes.

A problem with this representation is that computing the intermediate sums can
incur a non-neglible cost. Take `cons` and `tail` operations as an example. Both
run in amortised constant time, though a specific `cons` or `tail` operation
could run either in logarithmic or constant time.
* In the case that either operation runs in logarithmic time, a logarithmic
  number of intermediate sums would have to be re-computed. If we want to
  quickly compute the new total sum from the previous total sum, then it might
  have been faster to *sum* the added element with the previous total sum in the
  case of `cons`, and to *subtract* the removed element from the previous total
  sum in the case of `tail`.
* In the case that either operation runs in constant time, summing or
  subtracting a single element is just as fast as recomputing a single
  intermediate sum (in this case, the total sum).

This is where root-measured finger trees come in. They behave just like
normal finger trees, but they now also keep track of a top-level root measure in
addition to *internal* measures that are stored at each node.

```haskell
-- | A @StrictFingerTree@ with elements of type @a@, an internal measure of type
-- @vi@, and a root measure of type @vr@.
data StrictFingerTree vr vi a = {- omitted -}
```

It is clear from the `cons`/`tail` example above that we should be able to both
sum and subtract root measures. So, we require a root measure to be a
cancellative monoid, which provides a means for subtraction through the
`stripPrefix` and `stripSuffix` functions.

```haskell
class (LeftCancellative v, RightCancellative v, Monoid v)
   => RootMeasured v a | a -> v where
  measureRoot :: a -> v

-- | All @'StrictFingerTree'@s are root measured.
instance RootMeasured vr a => RootMeasured vr (StrictFingerTree vr vi a) where
  {- omitted -}
```

### Word of warning

Root-measured finger trees are asymptotically slower than regular finger trees
in some cases: for functions like `split`, the asymptotic time complexity is now
linear instead of (amortised) logarithmic. Whether root-measured finger trees
are more performant than regular finger trees in practice relies on intimate
knowledge of the workload that the root-measured finger tree will be under.
