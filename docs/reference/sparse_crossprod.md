# Dot product of sparse_numeric vectors

Computes the inner (cross) product of two sparse_numeric vectors.

## Usage

``` r
sparse_crossprod(x, y, ...)

# S4 method for class 'sparse_numeric,sparse_numeric'
sparse_crossprod(x, y)
```

## Arguments

- x:

  A `sparse_numeric` vector.

- y:

  A `sparse_numeric` vector.

- ...:

  Ignored; included for method compatibility.

## Value

A numeric scalar equal to `sum(x * y)`.
