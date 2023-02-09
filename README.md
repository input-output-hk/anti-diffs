[![Haskell CI](https://img.shields.io/github/actions/workflow/status/input-output-hk/anti-diffs/haskell.yml?label=Build&style=for-the-badge)](https://github.com/input-output-hk/anti-diffs/actions/workflows/haskell.yml)
[![handbook](https://img.shields.io/badge/policy-Cardano%20Engineering%20Handbook-informational?style=for-the-badge)](https://input-output-hk.github.io/cardano-engineering-handbook)

# anti-diffs

Packages for performant sequences of `Data.Map` differences.

We follow processes and guidelines as established by the Consensus team at IOG.
A nice starting point for reading `ouroboros-consensus` documentation is
[ouroboros-network/ouroboros-consensus/README.md](
https://github.com/input-output-hk/ouroboros-network/blob/master/ouroboros-consensus/README.md).


## Usage

Below is a simple example of defining a sequence of differences using
root-measured finger trees and diffs. For more in-depth information about
root-measured finger trees and diffs, read the following two READMEs. It is
recommended to read them in order, because the documentation of `fingertree-rm`
will justify choices and goals for `diff-containers`.

* See [fingertree-rm/README.md](./fingertree-rm/README.md)
* See [diff-containers/README.md](./diff-containers/README.md)

```haskell
import qualified Data.Map.Diff.Strict as Diff
import qualified Data.FingerTree.RootMeasured.Strict as FTRM

-- | A sequence of differences as a root-measured finger tree, storing the total
-- sum of diffs at the root, and the length of sub-sequences in each node of the
-- finger tree tree.
type DiffSeq k v = FTRM.StrictFingerTree (Diff k v) (Sum Int) (Diff k v)

instance RootMeasured (Diff k v) (Diff k v) where
  measureRoot = id

instance Measured (Diff k v) (Sum Int) where
  measure = const 1
```

