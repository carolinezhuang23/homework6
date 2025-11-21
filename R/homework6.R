
#' Sparse Numeric Vector
#'
#' An S4 class that represents a sparse numeric vector. A sparse numeric vector
#' is a vector where majority elements are zero.
#'
#' @slot value A numeric vector of nonzero values
#' @slot pos An integer vector of the positions of nonzero values
#' @slot length An integer representing the total length of vector, w/non zeros
#'
#' @importFrom  methods new
#' @exportClass sparse_numeric

setClass(
  Class = "sparse_numeric",
  slots = c(
    value = "numeric",
    pos = "integer",
    length = "integer"
  )
)

# validation
setValidity(
  Class = "sparse_numeric",
  method = function(object){
    if(anyNA(object@value))
      return("Values must not contain NA values.")
    if(anyNA(object@pos) || any(object@pos < 1L))
      return("Can not have negative or NA position.")
    if(length(object@value) != length(object@pos))
      return("Length of values must be equal to length of positions.")
    if(length(object@length) != 1L || is.na(object@length) || object@length < 1)
      return("Length must be one single positive integer.")
    if(any(object@pos > object@length))
      return("Position must not be greater than length of sparse vector.")
    TRUE
  }
)

# coerce sparse_vector --> numeric
setAs(
  from = "sparse_numeric",
  to = "numeric",
  def = function(from){
    n <- as.integer(from@length)
    out <- numeric(n)
    if(length(from@pos))
      out[from@pos] <- from@value
  }
)

# coerce numeric --> sparse_vector
setAs(
  from = "numeric",
  to = "sparse_numeric",
  def = function(from){
    n <- as.integer(length(from))
    values <- c()
    positions <- c()
    for(i in seq_len(n)){
      if(from[i] != 0){
        values <- append(values, from[i])
        positions <- append(positions, i)
      }
    }
    new_vector <- new("sparse_numeric",
                      value = values,
                      pos = as.integer(positions),
                      length = n)
    return(new_vector)
  }
)

#' Show a sparse_numeric object
#'
#' Displays the content of a \code{sparse_numeric} vector, printing both
#' non-zero and zero elements as well as their position.
#'
#' @param object a \code{sparse_numeric} vector
#'
#' @return Invisibly returns a \code{sparse_numeric} vector
setMethod("show", "sparse_numeric",
          definition = function(object){
            j <- 1L
            pos <- object@pos
            val <- object@value
            for(i in seq_len(object@length)){
              if(j <= length(pos) && i == pos[j]){
                cat(sprintf("Vector has value %g at position %d\n", val[j], pos[j]))
                j <- j + 1L
              }
              else {
                cat(sprintf("Vector has value 0 at position %d\n", i))
              }
            }
          })

#' Plot overlapping non-zero entries of two sparse_numeric vectors
#'
#' Creates a scatter plot of the overlapping non-zero entries of two
#' \code{sparse_numeric} vectors of the same length.
#'
#' @param x A \code{sparse_numeric} object.
#' @param y A \code{sparse_numeric} object.
#' @param ... Additional arguments (currently ignored).
#'
#' @return A \code{ggplot} object is printed to the active graphics device.
#' @export
setMethod("plot",
          signature = c(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            if (x@length != y@length)
              stop("length mismatch in plot.sparse_numeric")

            idx <- intersect(x@pos, y@pos)
            xv <- x@value[match(idx, x@pos)]
            yv <- y@value[match(idx, y@pos)]

            df <- data.frame(x = xv, y = yv, pos = idx)
            p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
              ggplot2::geom_point() +
              ggplot2::labs(
                title = "Overlapping non-zero entries",
                x = "x value (at overlapping positions)",
                y = "y value (at overlapping positions)"
              )
            print(p)
          }
)

#' Add two sparse numeric vectors together
#'
#' Adds two \code{sparse_numeric} vectors of the same length element-wise,
#' returning a new \code{sparse_numeric} object containing the non-zero elements
#' of the sum
#'
#' @param x a \code{sparse_numeric} vector
#' @param y a \code{sparse_numeric} vector
#' @param ... ignored.
#' @param e1 A \code{sparse_numeric} object (left-hand operand for `+`).
#' @param e2 A \code{sparse_numeric} object (right-hand operand for `+`).
#'
#' @return A \code{sparse_numeric} vector representing x + y
#' @export
setGeneric(
  "sparse_add",
  function(x, y, ...) standardGeneric("sparse_add")
)

#' @rdname sparse_add
#' @export
setMethod(
  "sparse_add",
  signature = c(x = "sparse_numeric", y = "sparse_numeric"),
  function(x, y){
    if(x@length != y@length){
      stop("length mismatch")
    }
    x_pos <- x@pos
    y_pos <- y@pos
    x_value <- x@value
    y_value <- y@value
    values <- c()
    positions <- c()
    i <- 1L; j <- 1L

    while(i <= length(x_pos) || j <= length(y_pos)){
      if(j > length(y_pos) || (i <= length(x_pos) && x_pos[i] < y_pos[j])){
        values <- append(values, x_value[i])
        positions <- append(positions, x_pos[i])
        i <- i + 1L
      } else if(i > length(x_pos) || y_pos[j] < x_pos[i]){
        values <- append(values, y_value[j])
        positions <- append(positions, y_pos[j])
        j <- j + 1L
      } else{
        s <- x_value[i] + y_value[j]
        if(s != 0){
          values <- append(values, s)
          positions <- append(positions, x_pos[i])
        }
        i <- i + 1L
        j <- j + 1L
      }
    }
    new_vector <- new("sparse_numeric",
                      value = values,
                      pos = as.integer(positions),
                      length = as.integer(x@length))
    return(new_vector)
  }
)

#' @rdname sparse_add
#' @export
setMethod("+",
          signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2){
            return(sparse_add(e1, e2))
          }
)

#' Subtracts two sparse numeric vectors
#'
#' Subtracts two \code{sparse_numeric} vectors of the same length element-wise,
#' returning a new \code{sparse_numeric} object containing the non-zero elements
#' of the subtraction
#'
#' @param x a \code{sparse_numeric} vector
#' @param y a \code{sparse_numeric} vector
#' @param ... ignored.
#'
#' @param e1 A \code{sparse_numeric} object (left-hand operand for `-`).
#' @param e2 A \code{sparse_numeric} object (right-hand operand for `-`).
#'
#' @return A \code{sparse_numeric} vector representing x - y
#' @export
setGeneric(
  "sparse_sub",
  function(x, y, ...) standardGeneric("sparse_sub")
)
#' @rdname sparse_sub
#' @export
setMethod(
  "sparse_sub",
  signature = c(x = "sparse_numeric", y = "sparse_numeric"),
  function(x, y){
    if(x@length != y@length){
      stop("length mismatch")
    }
    x_pos <- x@pos
    y_pos <- y@pos
    x_value <- x@value
    y_value <- y@value
    values <- numeric(0)
    positions <- integer(0)
    i <- 1L; j <- 1L

    while(i <= length(x_pos) || j <= length(y_pos)){
      if(j > length(y_pos) || (i <= length(x_pos) && x_pos[i] < y_pos[j])){
        values <- append(values, x_value[i])
        positions <- append(positions, x_pos[i])
        i <- i + 1L
      } else if(i > length(x_pos) || y_pos[j] < x_pos[i]){
        values <- append(values, -y_value[j])
        positions <- append(positions, y_pos[j])
        j <- j + 1L
      } else{
        s <- x_value[i] - y_value[j]
        if(s != 0){
          values <- append(values, s)
          positions <- append(positions, x_pos[i])
        }
        i <- i + 1L
        j <- j + 1L
      }
    }
    new_vector <- new("sparse_numeric",
                      value = values,
                      pos = as.integer(positions),
                      length = as.integer(x@length))
    return(new_vector)
  }
)
#' @rdname sparse_sub
#' @export
setMethod("-",
          signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2){
            return(sparse_sub(e1, e2))
          }
)

#' Multiplies two sparse numeric vectors together
#'
#' Multiplies two \code{sparse_numeric} vectors of the same length element-wise,
#' returning a new \code{sparse_numeric} object containing the non-zero elements
#' of the product
#'
#' @param x a \code{sparse_numeric} vector
#' @param y a \code{sparse_numeric} vector
#' @param ... ignored.
#'
#' @param e1 A \code{sparse_numeric} object (left-hand operand for `*`).
#' @param e2 A \code{sparse_numeric} object (right-hand operand for `*`).
#'
#' @return A \code{sparse_numeric} vector representing x * y (element-wise)
#' @export
setGeneric(
  "sparse_mult",
  function(x, y, ...) standardGeneric("sparse_mult")
)
#' @rdname sparse_mult
#' @export
setMethod(
  "sparse_mult",
  signature = c(x = "sparse_numeric", y = "sparse_numeric"),
  function(x, y){
    if(x@length != y@length){
      stop("length mismatch")
    }
    x_pos <- x@pos
    y_pos <- y@pos
    x_value <- x@value
    y_value <- y@value
    values <- c()
    positions <- c()
    i <- 1L; j <- 1L

    while(i <= length(x_pos) && j <= length(y_pos)){
      if(x_pos[i] < y_pos[j]){
        i <- i + 1L
      }
      else if(y_pos[j] < x_pos[i]){
        j <- j + 1L
      }
      else{
        prod <- x_value[i] * y_value[j]
        if(prod != 0){
          values <- append(values, x_value[i] * y_value[j])
          positions <- append(positions, x_pos[i])
        }
        i <- i + 1L
        j <- j + 1L
      }
    }
    new_vector <- new("sparse_numeric",
                      value = values,
                      pos = as.integer(positions),
                      length = as.integer(x@length))
    return(new_vector)
  }
)
#' @rdname sparse_mult
#' @export
setMethod("*",
          signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2){
            return(sparse_mult(e1, e2))
          }
)

#' Cross multiplies two sparse numeric vectors together
#'
#' Computes the sum of all pairwise products between the non-zero entries of
#' two \code{sparse_numeric} vectors (a simple cross-product).
#'
#' @param x a \code{sparse_numeric} vector
#' @param y a \code{sparse_numeric} vector
#' @param ... ignored.
#'
#' @return A numeric scalar representing cross product of x and y
#' @export
setGeneric(
  "sparse_crossprod",
  function(x, y, ...) standardGeneric("sparse_crossprod")
)
#' @rdname sparse_crossprod
#' @export
setMethod(
  "sparse_crossprod",
  signature = c(x = "sparse_numeric", y = "sparse_numeric"),
  function(x, y){
    if(x@length != y@length){
      stop("length mismatch")
    }
    x_value <- x@value
    y_value <- y@value
    values <- c()
    i <- 1L

    totals <- 0
    while(i <= length(x_value)){
      xv <- x_value[i]
      j <- 1L
      while(j <= length(y_value)){
        yv <- y_value[j]
        sum <- xv * yv
        totals <- totals + sum
        j <- j + 1L
      }
      i <- i + 1L
    }
    return(as.numeric(totals))
  }
)

#' Divide two sparse_numeric vectors
#'
#' Computes the element-wise integer division of two \code{sparse_numeric}
#' vectors of the same length, storing non-zero results.
#'
#' @param x A \code{sparse_numeric} object (numerator).
#' @param y A \code{sparse_numeric} object (denominator).
#' @param ... Ignored.
#'
#' @param e1 A \code{sparse_numeric} object (left-hand operand for `\`).
#' @param e2 A \code{sparse_numeric} object (right-hand operand for `\`)
#'
#' @return A \code{sparse_numeric} object representing \code{floor(x / y)} at
#'   positions where both vectors are non-zero.
#' @export
setGeneric(
  "sparse_div",
  function(x, y, ...) standardGeneric("sparse_div")
)
#' @rdname sparse_div
#' @export
setMethod(
  "sparse_div",
  signature = c(x = "sparse_numeric", y = "sparse_numeric"),
  function(x, y){
    if(x@length != y@length){
      stop("length mismatch")
    }
    x_pos <- x@pos
    y_pos <- y@pos
    x_value <- x@value
    y_value <- y@value
    values <- c()
    positions <- c()
    i <- 1L; j <- 1L

    while(i <= length(x_pos) || j <= length(y_pos)){
      if(x_pos[i] < y_pos[j]){
        i <- i + 1L
      }
      else if(y_pos[j] < x_pos[i]){
        j <- j + 1L
      }
      else{
        values <- append(values, as.integer(floor(x_value[i] / y_value[j])))
        positions <- append(positions, x_pos[i])
        i <- i + 1L
        j <- j + 1L
      }
    }
    new_vector <- new("sparse_numeric",
                      value = values,
                      pos = as.integer(positions),
                      length = as.integer(x@length))
    return(new_vector)
  }
)
#' @rdname sparse_div
#' @export
setMethod("/",
          signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2){
            return(sparse_div(e1, e2))
          }
)

#' Mean of a sparse_numeric vector
#'
#' Computes the mean of a \code{sparse_numeric} vector, counting the stored
#' non-zeros
#'
#' @param x A \code{sparse_numeric}
#' @param ... Ignored
#'
#' @return A numeric scalar giving mean of vector.
#' @export
setMethod(
  "mean",
  signature(x = "sparse_numeric"),
  function(x, ...){
    n <- as.integer(x@length)
    if(n == 0L){
      return(NA_real_)
    }
    (sum(x@value) / n)
  }
)

#' Euclidean norm of sparse_numeric vector
#'
#' Computes norm of a \code{sparse_numeric} object, defined as square root of
#' the sum of squared elements
#'
#' @param x A \code{sparse_numeric} object.
#' @param ... Ignored.
#'
#' @return A numeric scalar giving Euclidean norm
#' @export
setGeneric("norm",
           function(x, ...) standardGeneric("norm"))
#' @rdname norm
#' @export
setMethod(
  "norm",
  signature(x = "sparse_numeric"),
  function(x, ...){
    sqrt(sum(x@value^2))
  }
)

# sd helper
.sparse_sd <- function(x){
  mean_x <- mean(x)
  k <- length(x@pos)
  n <- as.integer(x@length)

  ss_nonzero <- sum((x@value - mean_x)^2)
  ss_zero <- (n - k) * (mean_x^2)

  sqrt((ss_nonzero + ss_zero) / (n - 1L))
}

#' Standardize a sparse_numeric vector
#'
#' Centers and scales a \code{sparse_numeric} vector by subtracting its mean
#' and dividing by its standard deviation
#'
#' @param x a \code{sparse_numeric} object.
#' @param ... ignored.
#'
#' @return A new \code{sparse_numeric} object containing standardized values
#' @export
setGeneric("standardize",
           function(x, ...) standardGeneric("standardize"))
#' @rdname standardize
#' @export
setMethod(
  "standardize",
  signature(x = "sparse_numeric"),
  function(x, ...){
    mu <- mean(x)
    sd <- .sparse_sd(x)
    if(is.na(sd) || sd == 0){
      stop("cannot standardize: standard deviation is zero or undefined")
    }
    k <- length(x@pos)
    n <- x@length

    nz_pos <- x@pos
    nz_vals <- (x@value - mu) / sd

    all_pos <- seq_len(x@length)
    zero_pos <- setdiff(all_pos, nz_pos)

    zero_val <- (-mu)/sd
    zero_vals <- rep(zero_val, length(zero_pos))
    new_pos  <- c(nz_pos, zero_pos)
    new_vals <- c(nz_vals, zero_vals)

    new("sparse_numeric",
        value = new_vals,
        pos = new_pos,
        length = as.integer(n))
  }
)
