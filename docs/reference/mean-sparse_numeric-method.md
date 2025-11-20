# Mean of a sparse_numeric vector

Computes the arithmetic mean of a sparse numeric vector, including the
implicit zeros (i.e., treating missing positions as 0).

## Usage

``` r
# S4 method for class 'sparse_numeric'
mean(x, ...)
```

## Arguments

- x:

  A `sparse_numeric` vector.

- ...:

  Ignored; included for compatibility with the generic.

## Value

A numeric scalar giving the mean of the full (dense) vector.
