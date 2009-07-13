\name{endomorph}

\alias{endomorph}
\alias{endomorph,list-method}
\alias{endomorph,data.frame-method}

\alias{mendomorph}
\alias{mendomorph,list-method}
\alias{mendomorph,data.frame-method}

\title{Endomorphisms via application of a function over an object's elements}

\description{Performs the endomorphic equivalents of
  \code{\link[base]{lapply}} and \code{\link[base]{mapply}} by returning
  objects of the same class as the inputs rather than a list.
}

\usage{
  endomorph(X, FUN, \dots)

  mendomorph(FUN, \dots, MoreArgs = NULL)
}

\arguments{
  \item{X}{a list, data.frame or Sequence object.}
  \item{FUN}{the function to be applied to each element of \code{X}
    (for \code{endomorph}) or for the elements in \code{\dots}
    (for \code{mendomorph}).}
  \item{\dots}{For \code{endomorph}, optional arguments to \code{FUN}.
    For \code{mendomorph}, a set of list, data.frame or Sequence
    objects to compute over.}
  \item{MoreArgs}{a list of other arguments to \code{FUN}.}
}

\value{
  \code{endomorph} returns an object of the same class as \code{X},
  each element of which is the result of applying \code{FUN} to the
  corresponding element of \code{X}.

  \code{mendomorph} returns an object of the same class as the first
  object specified in \code{\dots}, each element of which is the result
  of applying \code{FUN} to the corresponding elements of \code{\dots}.
}

\seealso{
  \code{\link[base]{lapply}}, \code{\link[base]{mapply}}
}

\examples{
  a <- data.frame(x = 1:10, y = rnorm(10))
  b <- data.frame(x = 1:10, y = rnorm(10))

  endomorph(a, function(x) (x - mean(x))/sd(x))
  mendomorph(function(e1, e2) (e1 - mean(e1)) * (e2 - mean(e2)), a, b)
}

\keyword{manip}
\keyword{utilities}
