## HW5_testscript.R

test_that("check validity method exists", {
  expect_false({
    validity_method <- getValidity(getClassDef("sparse_numeric"))
    is.null(validity_method)
  })
})

test_that("check validity method", {
  expect_true({
    x <- new("sparse_numeric",
             value = c(1, 2, 3, 1),
             pos = c(1L, 2L, 3L, 5L),
             length = 5L)
    validObject(x)
  })
})

test_that("check validity method 2", {
  expect_error({
    x <- new("sparse_numeric",
             value = c(1, 2, 3, 1),
             pos = c(1L, 2L, 3L, 5L),
             length = 5L)
    x@length <- 2L
    validObject(x)
  })
})

test_that("show prints expected lines", {
  x <- as(c(0, 1), "sparse_numeric")

  out <- capture.output(show(x))

  expect_equal(
    out,
    c(
      "Vector has value 0 at position 1",
      "Vector has value 1 at position 2"
    )
  )
})

test_that("check coercion return class", {
  expect_s4_class({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
  }, "sparse_numeric")
})

test_that("check for show method", {
  expect_no_error({
    getMethod("show", "sparse_numeric")
  })
})

test_that("check for plot method", {
  expect_no_error({
    getMethod("plot", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("plot runs without error for compatible sparse_numeric vectors", {
  x <- as(c(0, 1, 0, 3),  "sparse_numeric")
  y <- as(c(2, 0, 4, -1), "sparse_numeric")

  expect_silent(plot(x, y))
})

test_that("plot fails if mismatched lengths", {
  expect_error({
    x <- as(rep(0, 10), "sparse_numeric")
    y <- as(rep(0, 9), "sparse_numeric")

    plot(x, y)
  })
})

test_that("check for + method", {
  expect_no_error({
    getMethod("+", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("`+` for sparse_numeric matches dense addition", {
  x <- c(0, 1, 0, 3)
  y <- c(2, 0, 4, -1)

  sx <- as(x, "sparse_numeric")
  sy <- as(y, "sparse_numeric")

  s_sum <- sx + sy
  expect_identical(as(s_sum, "numeric"), x + y)
})


test_that("check for - method", {
  expect_no_error({
    getMethod("-", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("`-` for sparse_numeric matches dense subtraction", {
  x <- c(0, 1, 0, 3)
  y <- c(2, 0, 4, -1)

  sx <- as(x, "sparse_numeric")
  sy <- as(y, "sparse_numeric")

  s_sub <- sx - sy
  expect_identical(as(s_sub, "numeric"), x - y)
})

test_that("check for * method", {
  expect_no_error({
    getMethod("*", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("`*` for sparse_numeric matches multiplication", {
  x <- c(0, 1, 0, 3)
  y <- c(2, 0, 4, -1)

  sx <- as(x, "sparse_numeric")
  sy <- as(y, "sparse_numeric")

  s_mult <- sx * sy
  expect_identical(s_mult, sparse_mult(sx, sy))
})

test_that("`/` for sparse_numeric matches division", {
  x <- c(0, 0, 0, 4)
  y <- c(0, 0, 0, 2)

  sx <- as(x, "sparse_numeric")
  sy <- as(y, "sparse_numeric")

  s_div <- sx / sy
  expect_identical(s_div, sparse_div(sx, sy))
})

test_that("sparse add generic", {
  expect_true(isGeneric("sparse_add"))})
test_that("sparse mult generic", {
  expect_true(isGeneric("sparse_mult"))})
test_that("sparse sub generic", {
  expect_true(isGeneric("sparse_sub"))})
test_that("sparse crossprod generic", {
  expect_true(isGeneric("sparse_crossprod"))})

test_that("sparse add formals", {
  expect_true(length(formals(sparse_add)) >= 2L)
})

test_that("sparse mult formals", {
  expect_true(length(formals(sparse_mult)) >= 2L)
})

test_that("sparse sub formals", {
  expect_true(length(formals(sparse_sub)) >= 2L)
})

test_that("sparse crossprod formals", {
  expect_true(length(formals(sparse_crossprod)) >= 2L)
})

test_that("check returned class for add", {
  expect_s4_class({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
    sparse_add(x, y)
  }, "sparse_numeric")
})

test_that("sparse_add", {
  result <- as(c(1, 1, 0, 1, 6), "sparse_numeric")
  expect_equal({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
    sparse_add(x, y)
  }, result)
})

test_that("sparse add dense", {
  result <- as(c(2, 4, 6, 10, 12), "sparse_numeric")
  expect_equal({
    x <- as(c(1, 3, 4, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 2, 9, 10), "sparse_numeric")
    sparse_add(x, y)
  }, result)
})

test_that("all zero wrong length", {
  expect_error({
    x <- as(rep(0, 10), "sparse_numeric")
    y <- as(rep(0, 9), "sparse_numeric")
    sparse_add(x, y)
  })
})

test_that("check returned class for add", {
  expect_s4_class({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
    sparse_add(x, y)
  }, "sparse_numeric")
})

test_that("sparse_sub", {
  result <- as(c(-1, -1, 0, 1, -2), "sparse_numeric")
  expect_equal({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
    sparse_sub(x, y)
  }, result)
})

test_that("sparse sub dense", {
  result <- as(c(0, 2, 2, -8, -8), "sparse_numeric")
  expect_equal({
    x <- as(c(1, 3, 4, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 2, 9, 10), "sparse_numeric")
    sparse_sub(x, y)
  }, result)
})

test_that("all zero wrong length", {
  expect_error({
    x <- as(rep(0, 10), "sparse_numeric")
    y <- as(rep(0, 9), "sparse_numeric")
    sparse_sub(x, y)
  })
})

test_that("check returned class for subtraction", {
  expect_s4_class({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
    sparse_sub(x, y)
  }, "sparse_numeric")
})

test_that("sparse_mult", {
  result <- as(c(0, 0, 0, 0, 8), "sparse_numeric")
  expect_equal({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
    sparse_mult(x, y)
  }, result)
})

test_that("sparse mult dense", {
  result <- as(c(1, 3, 8, 9, 20), "sparse_numeric")
  expect_equal({
    x <- as(c(1, 3, 4, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 2, 9, 10), "sparse_numeric")
    sparse_mult(x, y)
  }, result)
})

test_that("all zero wrong length", {
  expect_error({
    x <- as(rep(0, 10), "sparse_numeric")
    y <- as(rep(0, 9), "sparse_numeric")
    sparse_mult(x, y)
  })
})

test_that("sparse_cross_mult", {
  result <- 18
  expect_equal({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
    sparse_crossprod(x, y)
  }, result)
})

test_that("sparse cross mult dense", {
  result <- 23
  expect_equal({
    x <- as(c(1, 0, 0, 0, 0), "sparse_numeric")
    y <- as(c(1, 1, 2, 9, 10), "sparse_numeric")
    sparse_crossprod(x, y)
  }, result)
})

test_that("all zero wrong length", {
  expect_error({
    x <- as(rep(0, 10), "sparse_numeric")
    y <- as(rep(0, 9), "sparse_numeric")
    sparse_crossprod(x, y)
  })
})

test_that("sparse_div", {
  result <- as(c(0, 0, 0, 0, 2), "sparse_numeric")
  expect_equal({
    x <- as(c(0, 0, 0, 1, 4), "sparse_numeric")
    y <- as(c(1, 1, 0, 0, 2), "sparse_numeric")
    sparse_div(x, y)
  }, result)
})


test_that("all zero wrong length", {
  expect_error({
    x <- as(rep(0, 10), "sparse_numeric")
    y <- as(rep(0, 9), "sparse_numeric")
    sparse_div(x, y)
  })
})

test_that("mean for sparse_numeric matches dense mean", {
  x <- c(0, 0, 2, 0, -5)
  sx <- as(x, "sparse_numeric")

  expect_equal(mean(sx), mean(x))
})

test_that("norm for sparse_numeric matches dense L2 norm", {
  x <- c(0, 3, -4, 0)
  sx <- as(x, "sparse_numeric")

  expect_equal(norm(sx), sqrt(sum(x^2)))
})

test_that("standardize gives mean 0 and sd 1", {
  x <- c(0, 0, 2, 0, -5)
  sx <- as(x, "sparse_numeric")

  sz <- standardize(sx)
  z  <- as(sz, "numeric")

  expect_equal(mean(z), 0, tolerance = 1e-8)
  expect_equal(sd(z), 1, tolerance = 1e-8)
})

test_that("operations fail on length mismatch", {
  x <- as(c(1, 2, 3), "sparse_numeric")
  y <- as(c(1, 2), "sparse_numeric")

  expect_error(sparse_add(x, y), "length mismatch")
  expect_error(sparse_sub(x, y), "length mismatch")
  expect_error(sparse_mult(x, y), "length mismatch")
  expect_error(sparse_div(x, y), "length mismatch")
})

