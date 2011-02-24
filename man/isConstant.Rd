\name{isConstant}

\alias{isConstant}
\alias{isConstant,integer-method}
\alias{isConstant,numeric-method}
\alias{isConstant,array-method}

\title{Test if an atomic vector or array is constant}

\description{
  Generic function to test if an atomic vector or array is constant or not.
  Currently only methods for vectors or arrays of type integer or double
  are implemented.
}

\usage{
isConstant(x)
}

\arguments{
  \item{x}{
    An atomic vector or array.
  }
}

\details{
  Vectors of length 0 or 1 are always considered to be constant.
}

\value{
  A single logical i.e. \code{TRUE}, \code{FALSE} or \code{NA}.
}

\author{H. Pages}

\seealso{
  \code{\link{duplicated}},
  \code{\link{unique}},
  \code{\link{all.equal}},
  \code{\link{NA}},
  \code{\link{is.finite}}
}

\examples{
  ## ---------------------------------------------------------------------
  ## A. METHOD FOR integer VECTORS
  ## ---------------------------------------------------------------------

  ## On a vector with no NAs:
  stopifnot(isConstant(rep(-29L, 10000)))

  ## On a vector with NAs:
  stopifnot(!isConstant(c(0L, NA, -29L)))
  stopifnot(is.na(isConstant(c(-29L, -29L, NA))))
 
  ## On a vector of length <= 1:
  stopifnot(isConstant(NA_integer_))


  ## ---------------------------------------------------------------------
  ## B. METHOD FOR numeric VECTORS
  ## ---------------------------------------------------------------------
  ## This method does its best to handle rounding errors and special
  ## values NA, NaN, Inf and -Inf in a way that "makes sense".
  ## Below we only illustrate handling of rounding errors.

  ## Here values in 'x' are "conceptually" the same:
  x <- c(11/3,
         2/3 + 4/3 + 5/3,
         50 + 11/3 - 50,
         7.00001 - 1000003/300000)
  ## However, due to machine rounding errors, they are not *strictly*
  ## equal:
  duplicated(x)
  unique(x)
  ## only *nearly* equal:
  all.equal(x, rep(11/3, 4))  # TRUE

  ## 'isConstant(x)' uses 'all.equal()' internally to decide whether
  ## the values in 'x' are all the same or not:
  stopifnot(isConstant(x))

  ## This is not perfect though:
  isConstant((x - 11/3) * 1e8)  # FALSE on Intel Pentium paltforms
                                # (but this is highly machine dependent!)
}

\keyword{utilities}
