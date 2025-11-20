# Add two sparse_numeric vectors

Elementwise addition of two sparse numeric vectors of the same length.

## Usage

``` r
sparse_add(x, y, ...)

# S4 method for class 'sparse_numeric,sparse_numeric'
sparse_add(x, y)
```

## Arguments

- x:

  A `sparse_numeric` vector.

- y:

  A `sparse_numeric` vector.

- ...:

  Ignored; included for method compatibility.

## Value

A `sparse_numeric` vector representing `x + y`.
