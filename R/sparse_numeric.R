#' Core infrastructure for sparseNumeric
#'
#' This file defines the sparse_numeric S4 class and its associated methods.
#'
#' @keywords internal
#' @noRd
#' @importFrom methods setClass setMethod setGeneric new as validObject show
#' @importFrom stats setNames
#' @importFrom graphics par points legend
NULL


#' Sparse numeric vector S4 class
#'
#' Represents a sparse numeric vector, storing only non-zero values and
#' their positions along with the full vector length.
#'
#' @slot value Numeric vector of non-zero values.
#' @slot pos Integer vector of positions of non-zero values (1-based, sorted, unique).
#' @slot length Single non-negative integer giving the full length.
#'
#' @export
setClass(
  Class = "sparse_numeric",
  slots = c(
    value  = "numeric",
    pos    = "integer",
    length = "integer"
  )
)

# Validity ---------------------------------------------------------------

setValidity("sparse_numeric", function(object) {
  errs <- character()

  len <- object@length
  pos <- object@pos
  val <- object@value

  # length slot
  if (length(len) != 1L || !is.integer(len) || is.na(len) || len < 0L)
    errs <- c(errs, "`length` must be a single non-negative integer")

  # type checks
  if (!is.integer(pos))
    errs <- c(errs, "`pos` must be an integer vector")
  if (!is.numeric(val))
    errs <- c(errs, "`value` must be a numeric vector")

  # matching lengths
  if (length(pos) != length(val))
    errs <- c(errs, "`pos` and `value` must have the same length")

  # bounds & sorting
  if (length(pos) > 0L) {
    if (any(pos < 1L))
      errs <- c(errs, "`pos` values must be >= 1")
    if (any(pos > len))
      errs <- c(errs, "`pos` cannot exceed `length`")
    if (is.unsorted(pos, strictly = TRUE))
      errs <- c(errs, "`pos` must be strictly increasing (sorted, no duplicates)")
  }

  # non-zero values
  if (any(val == 0))
    errs <- c(errs, "`value` cannot contain zeros")

  # final
  if (length(errs)) errs else TRUE
})

# Helpers ----------------------------------------------------------------

`%||%` <- function(a, b) {
  if (length(a) == 0L || is.na(a)) b else a
}

.check_same_length <- function(x, y) {
  if (x@length != y@length)
    stop("Sparse vectors must have the same `length`.")
}

# Coercion ---------------------------------------------------------------

# numeric -> sparse_numeric
setAs("numeric", "sparse_numeric", function(from) {
  nz <- which(from != 0)
  vals <- from[nz]

  new("sparse_numeric",
      value  = as.numeric(vals),
      pos    = as.integer(nz),
      length = as.integer(length(from)))
})

# sparse_numeric -> numeric
setAs("sparse_numeric", "numeric", function(from) {
  dense <- numeric(from@length)
  if (length(from@pos) > 0L)
    dense[from@pos] <- from@value
  dense
})

#' Construct a sparse_numeric vector from a numeric vector
#'
#' @param x A numeric vector.
#'
#' @return An object of class \code{sparse_numeric}.
#' @export
sparse_numeric <- function(x) {
  as(x, "sparse_numeric")
}

#' Length of a sparse_numeric vector
#'
#' @param x A \code{sparse_numeric} object.
#'
#' @return Integer length of the underlying full vector.
#' @export
setMethod("length", "sparse_numeric", function(x) x@length)

# Arithmetic generics ----------------------------------------------------

#' Add two sparse_numeric vectors
#'
#' Elementwise addition of two sparse numeric vectors of the same length.
#'
#' @param x A \code{sparse_numeric} vector.
#' @param y A \code{sparse_numeric} vector.
#' @param ... Ignored; included for method compatibility.
#'
#' @return A \code{sparse_numeric} vector representing \code{x + y}.
#' @export
setGeneric("sparse_add", function(x, y, ...) standardGeneric("sparse_add"))

#' Subtract two sparse_numeric vectors
#'
#' Elementwise subtraction of two sparse numeric vectors of the same length.
#'
#' @param x A \code{sparse_numeric} vector.
#' @param y A \code{sparse_numeric} vector.
#' @param ... Ignored; included for method compatibility.
#'
#' @return A \code{sparse_numeric} vector representing \code{x - y}.
#' @export
setGeneric("sparse_sub", function(x, y, ...) standardGeneric("sparse_sub"))

#' Elementwise multiplication of sparse_numeric vectors
#'
#' Multiplies corresponding non-zero positions in two sparse vectors.
#'
#' @param x A \code{sparse_numeric} vector.
#' @param y A \code{sparse_numeric} vector.
#' @param ... Ignored; included for method compatibility.
#'
#' @return A \code{sparse_numeric} vector representing \code{x * y}.
#' @export
setGeneric("sparse_mult", function(x, y, ...) standardGeneric("sparse_mult"))

#' Dot product of sparse_numeric vectors
#'
#' Computes the inner (cross) product of two sparse_numeric vectors.
#'
#' @param x A \code{sparse_numeric} vector.
#' @param y A \code{sparse_numeric} vector.
#' @param ... Ignored; included for method compatibility.
#'
#' @return A numeric scalar equal to \code{sum(x * y)}.
#' @export
setGeneric("sparse_crossprod", function(x, y, ...) standardGeneric("sparse_crossprod"))

# Arithmetic methods -----------------------------------------------------

#' @rdname sparse_add
#' @export
setMethod("sparse_add", c("sparse_numeric", "sparse_numeric"),
          function(x, y) {
            .check_same_length(x, y)
            pos_all <- sort(unique(c(x@pos, y@pos)))
            val_x <- setNames(x@value, x@pos)
            val_y <- setNames(y@value, y@pos)
            vals <- vapply(
              pos_all,
              function(p) {
                px <- val_x[as.character(p)] %||% 0
                py <- val_y[as.character(p)] %||% 0
                px + py
              },
              numeric(1L)
            )
            nz <- which(vals != 0)
            new("sparse_numeric",
                value  = as.numeric(vals[nz]),
                pos    = as.integer(pos_all[nz]),
                length = x@length)
          })

#' @rdname sparse_sub
#' @export
setMethod("sparse_sub", c("sparse_numeric", "sparse_numeric"),
          function(x, y) {
            .check_same_length(x, y)
            pos_all <- sort(unique(c(x@pos, y@pos)))
            val_x <- setNames(x@value, x@pos)
            val_y <- setNames(y@value, y@pos)
            vals <- vapply(
              pos_all,
              function(p) {
                px <- val_x[as.character(p)] %||% 0
                py <- val_y[as.character(p)] %||% 0
                px - py
              },
              numeric(1L)
            )
            nz <- which(vals != 0)
            new("sparse_numeric",
                value  = as.numeric(vals[nz]),
                pos    = as.integer(pos_all[nz]),
                length = x@length)
          })

#' @rdname sparse_mult
#' @export
setMethod("sparse_mult", c("sparse_numeric", "sparse_numeric"),
          function(x, y) {
            .check_same_length(x, y)
            common <- intersect(x@pos, y@pos)
            if (length(common) == 0L) {
              return(new("sparse_numeric",
                         value = numeric(),
                         pos = integer(),
                         length = x@length))
            }
            val_x <- setNames(x@value, x@pos)
            val_y <- setNames(y@value, y@pos)
            vals <- as.numeric(
              val_x[as.character(common)] *
                val_y[as.character(common)]
            )
            new("sparse_numeric",
                value  = vals,
                pos    = as.integer(common),
                length = x@length)
          })

#' @rdname sparse_crossprod
#' @export
setMethod("sparse_crossprod", c("sparse_numeric", "sparse_numeric"),
          function(x, y) {
            .check_same_length(x, y)
            common <- intersect(x@pos, y@pos)
            if (length(common) == 0L) return(0)
            val_x <- setNames(x@value, x@pos)
            val_y <- setNames(y@value, y@pos)
            sum(val_x[as.character(common)] * val_y[as.character(common)])
          })

#' Arithmetic operators for sparse_numeric vectors
#'
#' Elementwise addition, subtraction, and multiplication using the
#' standard \code{+}, \code{-}, and \code{*} operators.
#'
#' @param e1,e2 \code{sparse_numeric} vectors.
#'
#' @name sparse_arith_ops
#' @rdname sparse_arith_ops
NULL

#' @rdname sparse_arith_ops
#' @export
setMethod("+", c("sparse_numeric", "sparse_numeric"),
          function(e1, e2) sparse_add(e1, e2))

#' @rdname sparse_arith_ops
#' @export
setMethod("-", c("sparse_numeric", "sparse_numeric"),
          function(e1, e2) sparse_sub(e1, e2))

#' @rdname sparse_arith_ops
#' @export
setMethod("*", c("sparse_numeric", "sparse_numeric"),
          function(e1, e2) sparse_mult(e1, e2))

# Display and plotting ---------------------------------------------------

#' Show method for sparse_numeric
#'
#' @param object A \code{sparse_numeric} object.
#' @export
setMethod("show", "sparse_numeric", function(object) {
  cat("sparse_numeric vector of length", object@length, "\n")
  if (length(object@pos) == 0L) {
    cat("  (all elements are zero)\n")
  } else {
    df <- data.frame(pos = object@pos, value = object@value)
    print(df, row.names = FALSE)
  }
})

#' Plot two sparse_numeric vectors
#'
#' @param x,y \code{sparse_numeric} vectors of the same length.
#' @param ... Additional arguments (ignored).
#' @export
setMethod(
  "plot",
  signature(x = "sparse_numeric", y = "sparse_numeric"),
  function(x, y, ...) {
    .check_same_length(x, y)
    par(mfrow = c(1, 1))
    all_vals <- c(x@value, y@value)
    plot(
      x@pos, x@value,
      pch = 16,
      ylim = range(all_vals),
      xlab = "Index Position",
      ylab = "Value",
      main = "Non-zero elements of two sparse vectors",
      ...
    )
    points(y@pos, y@value, pch = 17)
    legend(
      "topright",
      legend = c("x", "y"),
      pch = c(16, 17)
    )
  }
)

# Sum, mean, norm, standardize -------------------------------------------

#' Sum of a sparse_numeric vector
#'
#' @param x A \code{sparse_numeric} object.
#' @param ... Ignored.
#' @param na.rm Logical; if TRUE, remove NA values in the stored entries.
#'
#' @return Numeric scalar giving the sum of all elements (zeros included).
#' @export
setMethod("sum", "sparse_numeric", function(x, ..., na.rm = FALSE) {
  if (na.rm) sum(x@value, na.rm = TRUE) else sum(x@value)
})

#' Mean of a sparse_numeric vector
#'
#' Computes the arithmetic mean of a sparse numeric vector, including the
#' implicit zeros (i.e., treating missing positions as 0).
#'
#' @param x A \code{sparse_numeric} vector.
#' @param ... Ignored; included for compatibility with the generic.
#'
#' @return A numeric scalar giving the mean of the full (dense) vector.
#' @export
setMethod("mean", "sparse_numeric", function(x, ...) {
  if (x@length == 0L) {
    return(NA_real_)
  }
  sum(x@value) / as.numeric(x@length)
})

#' Euclidean norm of a sparse_numeric vector
#'
#' Computes the Euclidean (L2) norm, i.e. \eqn{sqrt(sum(x_i^2))}.
#'
#' @param x A \code{sparse_numeric} object.
#' @param ... Ignored.
#'
#' @return Numeric scalar norm.
#' @export
setGeneric("norm", function(x, ...) standardGeneric("norm"))

#' @rdname norm
#' @export
setMethod("norm", "sparse_numeric", function(x, ...) {
  sqrt(sum(x@value^2))
})

#' Standardize a sparse_numeric vector
#'
#' Standardizes the full vector (including zeros) to have mean 0 and
#' standard deviation 1. Since almost all standardized elements are
#' typically non-zero, this method returns a dense numeric vector.
#'
#' @inheritParams norm
#'
#' @return A numeric vector of the same length as \code{x}.
#' @export
setGeneric("standardize", function(x, ...) standardGeneric("standardize"))

#' @rdname standardize
#' @export
setMethod("standardize", "sparse_numeric", function(x, ...) {
  n <- as.numeric(x@length)
  if (n <= 1L)
    return(numeric(n))

  # sum and sum of squares over non-zero entries (zeros contribute 0)
  sum_all    <- sum(x@value)
  sum_sq_all <- sum(x@value^2)

  mean_all <- sum_all / n
  # sample variance using identity: sum((x - mean)^2) = sum(x^2) - n*mean^2
  var_num <- sum_sq_all - n * mean_all^2
  var_all <- var_num / (n - 1)

  if (!is.finite(var_all) || var_all <= 0)
    return(rep(0, n))

  sd_all <- sqrt(var_all)

  dense <- numeric(n)
  if (length(x@pos) > 0L) {
    dense[x@pos] <- x@value
  }

  (dense - mean_all) / sd_all
})
