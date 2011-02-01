\name{endoapply}

\alias{endoapply}
\alias{endoapply,list-method}
\alias{endoapply,data.frame-method}

\alias{mendoapply}
\alias{mendoapply,list-method}
\alias{mendoapply,data.frame-method}

\title{Endomorphisms via application of a function over an object's elements}

\description{Performs the endomorphic equivalents of
  \code{\link[base]{lapply}} and \code{\link[base]{mapply}} by returning
  objects of the same class as the inputs rather than a list.
}

\usage{
endoapply(X, FUN, \dots)

mendoapply(FUN, \dots, MoreArgs = NULL)
}

\arguments{
  \item{X}{a list, data.frame or List object.}
  \item{FUN}{the function to be applied to each element of \code{X}
    (for \code{endoapply}) or for the elements in \code{\dots}
    (for \code{mendoapply}).}
  \item{\dots}{For \code{endoapply}, optional arguments to \code{FUN}.
    For \code{mendoapply}, a set of list, data.frame or List
    objects to compute over.}
  \item{MoreArgs}{a list of other arguments to \code{FUN}.}
}

\value{
  \code{endoapply} returns an object of the same class as \code{X},
  each element of which is the result of applying \code{FUN} to the
  corresponding element of \code{X}.

  \code{mendoapply} returns an object of the same class as the first
  object specified in \code{\dots}, each element of which is the result
  of applying \code{FUN} to the corresponding elements of \code{\dots}.
}

\seealso{
  \code{\link[base]{lapply}}, \code{\link[base]{mapply}}
}

\examples{
  a <- data.frame(x = 1:10, y = rnorm(10))
  b <- data.frame(x = 1:10, y = rnorm(10))

  endoapply(a, function(x) (x - mean(x))/sd(x))
  mendoapply(function(e1, e2) (e1 - mean(e1)) * (e2 - mean(e2)), a, b)
}

\keyword{manip}
\keyword{utilities}
