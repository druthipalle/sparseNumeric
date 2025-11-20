# Sparse numeric vector S4 class

Represents a sparse numeric vector, storing only non-zero values and
their positions along with the full vector length.

## Slots

- `value`:

  Numeric vector of non-zero values.

- `pos`:

  Integer vector of positions of non-zero values (1-based, sorted,
  unique).

- `length`:

  Single non-negative integer giving the full length.
