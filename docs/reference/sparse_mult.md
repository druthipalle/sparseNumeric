# Elementwise multiplication of sparse_numeric vectors

Multiplies corresponding non-zero positions in two sparse vectors.

## Usage

``` r
sparse_mult(x, y, ...)

# S4 method for class 'sparse_numeric,sparse_numeric'
sparse_mult(x, y)
```

## Arguments

- x:

  A `sparse_numeric` vector.

- y:

  A `sparse_numeric` vector.

- ...:

  Ignored; included for method compatibility.

## Value

A `sparse_numeric` vector representing `x * y`.
