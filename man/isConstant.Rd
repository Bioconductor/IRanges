\name{isConstant}

\alias{isConstant}
\alias{isConstant,integer-method}
\alias{isConstant,numeric-method}

\title{Test if a vector is constant}

\description{
  Generic function to test if a vector is constant or not.
  Currently only methods for integer and numeric vectors are implemented.
}

\usage{
isConstant(x)
}

\arguments{
  \item{x}{
    A vector.
  }
}

\details{
  Vectors of length 0 or 1 are always considered to be constant.
}

\value{
  \code{TRUE} or \code{FALSE}.
}

\examples{
  x <- c(11/3, 2/3 + 4/3 + 5/3, 7.00001 - 1000003/300000)
  isConstant(x)

  ## Note that looking at the length of unique(x) to determine whether
  ## x is constant or not would not work:
  unique(x)
}

\keyword{utilities}
