# Higher-order functions for dealing with lists

These functions were inspired by underscore.js.

## Usage

``` r
pluck(key)

pluck_apply(key, xss)

pick(keys)

pick_apply(keys, xss)
```

## Arguments

- key:

  the name of a value in a list

- xss:

  a list of lists

- keys:

  a character vector of names in a list

## Value

`pluck()` returns an unnamed value and `pluck_apply()` returns a list of
unnamed values. `pick()` returns a simplified version of the original
list. `pick_apply()` returns a list of simplified lists.

## Details

- `pluck()`: Pluck a named value from a list

- `pick()`: Simplify a list by picking out allowed names

The simple versions of `pluck()` and `pick()` are curried functions,
meaning that they return a function which can be applied to a list. See
the syntax in the usage section.
