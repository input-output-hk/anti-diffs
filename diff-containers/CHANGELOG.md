## next version

### Non-breaking changes

* Add `empty` function.

### Patch

* Fix laziness in the `Functor` instance for `Diff`.
* Make buildable with `ghc-9.4` and `ghc-9.6`.

## 1.0.1.0 — 2023-05-11

* Add `numInserts` and `numDeletes` to count a diff's number of inserts and
  deletes (respectively)

## 1.0.0.0 — 2023-05-09

### Breaking changes

* Make diffs a cancellative monoid instead of a group. Amongst others, this
  change removes the need for preconditions and invariants relating to normality
  and positivity, which greatly simplifies the code and makes it more ergonomic,
  while still allowing a (more restrictive) type of diff subtraction. Changes
  include:
  * Remove the `Group` instance for diffs.
  * Add class instances for reductive and cancellative semigroup classes.
  * Remove predicates related to normality and positivity.
  * Remove unsafe constructors for `DiffEntry`.
  * Make `applyDiff` and `applyDiffForKeys` total.
  * Remove `unsafeApplyDiff` and `unsafeApplyDiffForKeys`.

## 0.1.0.0 — 2023-02-16

First release
