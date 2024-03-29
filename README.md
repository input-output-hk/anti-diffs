[![handbook](https://img.shields.io/badge/policy-Cardano%20Engineering%20Handbook-informational)](https://input-output-hk.github.io/cardano-engineering-handbook)
[![Haskell CI](https://img.shields.io/github/actions/workflow/status/input-output-hk/anti-diffs/haskell.yml?label=Build)](https://github.com/input-output-hk/anti-diffs/actions/workflows/haskell.yml)
[![Documentation CI](https://img.shields.io/github/actions/workflow/status/input-output-hk/anti-diffs/documentation.yml?label=Documentation%20build)](https://github.com/input-output-hk/anti-diffs/actions/workflows/documentation.yml)
[![Haddocks](https://img.shields.io/badge/documentation-Haddocks-purple)](https://input-output-hk.github.io/anti-diffs/)

# anti-diffs

Packages for sequences of `Data.Map` differences.

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

