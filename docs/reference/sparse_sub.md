# Subtract two sparse_numeric vectors

Elementwise subtraction of two sparse numeric vectors of the same
length.

## Usage

``` r
sparse_sub(x, y, ...)

# S4 method for class 'sparse_numeric,sparse_numeric'
sparse_sub(x, y)
```

## Arguments

- x:

  A `sparse_numeric` vector.

- y:

  A `sparse_numeric` vector.

- ...:

  Ignored; included for method compatibility.

## Value

A `sparse_numeric` vector representing `x - y`.
