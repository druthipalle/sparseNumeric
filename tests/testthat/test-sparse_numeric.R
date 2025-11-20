## tests/testthat/test_sparse_numeric.R

test_that("validity method exists", {
  validity_method <- getValidity(getClassDef("sparse_numeric"))
  expect_false(is.null(validity_method))
})

test_that("valid object passes validity", {
  x <- new("sparse_numeric",
           value = c(1, 2, 3, 1),
           pos = c(1L, 2L, 3L, 5L),
           length = 5L)
  expect_true(validObject(x))
})

test_that("invalid object fails validity (wrong length)", {
  x <- new("sparse_numeric",
           value = c(1, 2, 3, 1),
           pos = c(1L, 2L, 3L, 5L),
           length = 5L)
  x@length <- 2L
  expect_error(validObject(x))
})

test_that("coercion numeric -> sparse_numeric returns correct class", {
  x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
  expect_s4_class(x, "sparse_numeric")
})

test_that("coercion sparse_numeric -> numeric gives original dense vector", {
  v <- c(0, 0, 3, 0, 5)
  s <- as(v, "sparse_numeric")
  dense <- as(s, "numeric")
  expect_equal(dense, v)
})

test_that("show method exists", {
  expect_no_error(getMethod("show", "sparse_numeric"))
})

test_that("plot method exists", {
  expect_no_error(getMethod("plot", c("sparse_numeric", "sparse_numeric")))
})

test_that("+, -, * methods exist for sparse_numeric", {
  expect_no_error(getMethod("+", c("sparse_numeric", "sparse_numeric")))
  expect_no_error(getMethod("-", c("sparse_numeric", "sparse_numeric")))
  expect_no_error(getMethod("*", c("sparse_numeric", "sparse_numeric")))
})

## Generics existence

test_that("sparse_add generic exists", {
  expect_true(isGeneric("sparse_add"))
})

test_that("sparse_mult generic exists", {
  expect_true(isGeneric("sparse_mult"))
})

test_that("sparse_sub generic exists", {
  expect_true(isGeneric("sparse_sub"))
})

test_that("sparse_crossprod generic exists", {
  expect_true(isGeneric("sparse_crossprod"))
})

test_that("sparse_add has at least two arguments", {
  expect_true(length(formals(sparse_add)) >= 2L)
})

test_that("sparse_mult has at least two arguments", {
  expect_true(length(formals(sparse_mult)) >= 2L)
})

test_that("sparse_sub has at least two arguments", {
  expect_true(length(formals(sparse_sub)) >= 2L)
})

test_that("sparse_crossprod has at least two arguments", {
  expect_true(length(formals(sparse_crossprod)) >= 2L)
})

## Behavior of arithmetic operations

test_that("sparse_add returns sparse_numeric", {
  x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
  y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
  z <- sparse_add(x, y)
  expect_s4_class(z, "sparse_numeric")
})

test_that("sparse_add basic example matches expected result", {
  result <- as(c(1, 1, 0, 1, 6), "sparse_numeric")
  x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
  y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
  expect_equal(sparse_add(x, y), result)
})

test_that("sparse_add matches dense addition", {
  v1 <- c(1, 3, 4, 1, 2)
  v2 <- c(1, 1, 2, 9, 10)
  x <- as(v1, "sparse_numeric")
  y <- as(v2, "sparse_numeric")
  s <- sparse_add(x, y)
  expect_equal(as(s, "numeric"), v1 + v2)
})

test_that("sparse_add throws error on different lengths", {
  x <- as(rep(0, 10), "sparse_numeric")
  y <- as(rep(0, 9), "sparse_numeric")
  expect_error(sparse_add(x, y))
})

test_that("sparse_sub and sparse_mult behave like dense versions", {
  v1 <- c(0, 2, 0, 4)
  v2 <- c(1, 0, 3, 4)
  x <- as(v1, "sparse_numeric")
  y <- as(v2, "sparse_numeric")

  s_sub <- sparse_sub(x, y)
  s_mult <- sparse_mult(x, y)

  expect_equal(as(s_sub, "numeric"), v1 - v2)
  expect_equal(as(s_mult, "numeric"), v1 * v2)
})

test_that("sparse_crossprod matches dot product", {
  v1 <- c(0, 2, 0, 4)
  v2 <- c(1, 0, 3, 4)
  x <- as(v1, "sparse_numeric")
  y <- as(v2, "sparse_numeric")
  dp <- sparse_crossprod(x, y)
  expect_equal(dp, sum(v1 * v2))
})

## sum, mean, norm, standardize

test_that("sum(sparse_numeric) equals sum of dense vector", {
  v <- c(0, 0, 3, 0, 5)
  s <- as(v, "sparse_numeric")
  expect_equal(sum(s), sum(v))
})

test_that("mean(sparse_numeric) equals mean of dense vector", {
  v <- c(0, 0, 3, 0, 5)
  s <- as(v, "sparse_numeric")
  expect_equal(mean(s), mean(v))
})

test_that("mean works for all-zero sparse vector", {
  v <- rep(0, 10)
  s <- as(v, "sparse_numeric")
  expect_equal(mean(s), 0)
})

test_that("norm(sparse_numeric) equals Euclidean norm of dense vector", {
  v <- c(0, 3, 4)
  s <- as(v, "sparse_numeric")
  # assuming norm(x) = sqrt(sum(x^2))
  expect_equal(norm(s), sqrt(sum(v^2)))
})

test_that("norm of all-zero vector is zero", {
  v <- rep(0, 5)
  s <- as(v, "sparse_numeric")
  expect_equal(norm(s), 0)
})

test_that("standardize(sparse_numeric) gives mean ~ 0 and sd ~ 1", {
  v <- c(1, 2, 3, 4, 5)
  s <- as(v, "sparse_numeric")

  s_std <- standardize(s)
  v_std <- as(s_std, "numeric")

  expect_equal(round(mean(v_std), 7), 0)      # allow numeric tolerance
  expect_equal(round(sd(v_std), 7), 1)
})

test_that("standardize handles constant vector gracefully", {
  v <- rep(3, 5)
  s <- as(v, "sparse_numeric")

  # your implementation might return all zeros if sd == 0,
  # or throw an error. Accept either but don't silently give NaNs.
  expect_no_error({
    s_std <- standardize(s)
    v_std <- as(s_std, "numeric")
    expect_false(any(is.nan(v_std)))
    expect_false(any(is.infinite(v_std)))
  })
})

## length method, edge cases

test_that("length(sparse_numeric) matches stored length slot", {
  v <- c(0, 0, 3, 0, 5, 0)
  s <- as(v, "sparse_numeric")
  expect_equal(length(s), length(v))
})

test_that("sparse_numeric can represent all-zero vector", {
  v <- rep(0, 10)
  s <- as(v, "sparse_numeric")
  expect_equal(length(s@pos), 0L)
  expect_equal(length(s@value), 0L)
  expect_equal(as(s, "numeric"), v)
})
