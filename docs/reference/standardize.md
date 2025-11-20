# Standardize a sparse_numeric vector

Standardizes the full vector (including zeros) to have mean 0 and
standard deviation 1. Since almost all standardized elements are
typically non-zero, this method returns a dense numeric vector.

## Usage

``` r
standardize(x, ...)

# S4 method for class 'sparse_numeric'
standardize(x, ...)
```

## Arguments

- x:

  A `sparse_numeric` object.

- ...:

  Ignored.

## Value

A numeric vector of the same length as `x`.
