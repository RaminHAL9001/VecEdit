# VecEdit

This is a small library that provides some **unabashedly stateful**
monadic function types for editing mutable vectors. The main use case
is rapid prototyping of mutable vector algorithms in a GHCI
session.

Lenses allow arbitrary updates to certain elements of the state, using
Lens functions like `modifying` or `assign`.

## Breif overview of useful APIs

### `VecEdit.Vector.Editor`

Provides the monadic data type `Editor`, polymorphic over a vector
element type, and a vector type (e.g. mutable or immutable, boxed or
unboxed). Also instantiates `MonadRandom` with a `TFGen`, a random
number generator provided by the `tf-random` package.

- initializing vectors:

  - `newCurrentBuffer`
  - `withBuffer`
  - `fillWith`
  - `fillRandom`
  - `mapBuffer`
  - `thawBuffer`
  - `freezeBuffer`

- resizing vectors:

  - `growBufferWithCursor`

- a cursor into individual indicies

  - `cursor` -- a lens to a stateful index value
  - `getCurrentElem`
  - `putCurrentElem`

- slicing ranges elements

  - `currentRange` -- a lens to a stateful `{index,length}` value
  - `canonicalRange`
  - `sliceRange`
  - `sliceFromEnd`

- folding

  - `foldOverBuffer`
  - `foldOverRange`
  - `withSubRange`

- printing content to `STDOUT`

  - `printBuffer`

### `VecEdit.Vector.Editor.GapBuffer`

  Provides the `GapBuffer` monad, which lifts the above `Editor`
  monad. All `Editor` monad functions can be used, but `GapBuffer`
  provides features for using a Vector as a sort of text editor
  buffer:
  
  - moving elements from one end of a buffer to the other keeping a
    gap of uninitialized elements in the middle, and

    - `beforeCursor` and `afterCursor`
    - `getGapSize` and `countDefined`
    - `shiftCursor`
    - `atCursor`
    - `stepCursor`
  
  - growing the buffer

    - `ensureFreeSpace`
    - `defaultGrowFunction`

  - allowing `O(1)` insertion into the middle of the vector, to the
    lower or upper side of the gap.

    - `pushItem` and `pullItem`
    - `popItem`

  - slicing and copying the elements into a contiguous vector without
    a gap
  
    - `getBuffer3SliceInRange`
    - `fuseBuffer`

### `VecEdit.Table`

The "VecEdit.Table" module, which provides a very small set of
SQL-like APIs for modifying a vector. This module should be imported
qualified. Rows all have text labels which need not be unique, and
auto-incrementing ID numbers.

- `Row` is the data type used to store rows in the table.

- `Table` is the data type containing the table buffer.

- `EditTable` is the state monad for working with a mutable vector
  as though it were a database.

  - `new` -- creates a table
  - `list` -- display the full content of a table
  - `insert` -- insert an labeled element into the table
  - `select1` -- search for a matching row, return first match

    - `byRowId` -- select by auto-incrementing ID number
    - `byRowLabel` -- select by label
    - `byRowValue` -- select by the row data

  - `update1` -- use `select1` to find a row and modify it
  - `remove1` -- use `select1` to find a row and delete it
