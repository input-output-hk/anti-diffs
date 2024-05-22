## 1.2.0.0 — 2024-05-24

### Breaking

- Remove value from `Delta(Delete)`.

## 1.1.1.1 — 2024-03-22

### Patch

* Make it build with `ghc-9.8`.

## 1.1.1.0 — 2023-08-01

### Non-breaking changes

* Define `mapMaybeDiff` in the same spirit as `Maybe.mapMaybe`.

## 1.1.0.0 — 2023-07-14

### Breaking changes

* Rename `DiffEntry` type to `Delta`. As a result, `foldMapDiffEntry` is renamed
  to `foldMapDelta`, and `traverseDiffEntryWithKey_` is renamed to
  `traverseDeltaWithKey_`.`

### Non-breaking changes

* Add `empty` function.
* Internally, rename `NEDiffHistory` to `DeltaHistory` and remove `DiffHistory`
  to avoid duplication.

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
