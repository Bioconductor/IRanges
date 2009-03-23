\name{IRanges-setops}

\alias{IRanges-setops}

\alias{union,IRanges,IRanges-method}
\alias{intersect,IRanges,IRanges-method}
\alias{setdiff,IRanges,IRanges-method}
\alias{punion}
\alias{punion,IRanges,IRanges-method}
\alias{pintersect}
\alias{pintersect,IRanges,IRanges-method}
\alias{psetdiff}
\alias{psetdiff,IRanges,IRanges-method}
\alias{pgap}
\alias{pgap,IRanges,IRanges-method}

\title{Set operations on IRanges objects}

\description{
  Performs set operations on \link{IRanges} objects.
}

\usage{
  ## Vector-wise operations:
  \S4method{union}{IRanges,IRanges}(x, y)
  \S4method{intersect}{IRanges,IRanges}(x, y)
  \S4method{setdiff}{IRanges,IRanges}(x, y)

  ## Element-wise (aka "parallel") operations:
  punion(x, y, ...)
  pintersect(x, y, ...)
  psetdiff(x, y, ...)
  pgap(x, y, ...)
}

\arguments{
  \item{x, y}{
    \link{IRanges} objects.
  }
  \item{...}{
    Further arguments to be passed to or from other methods.
    For example, the \code{fill.gap} argument can be passed to the
    \code{punion} method for \link{IRanges} objects (see below).
  }
}

\details{
  The \code{union}, \code{intersect} and \code{setdiff} methods
  for \link{IRanges} objects return a "normal" \link{IRanges} instance
  representing the union, intersection and (asymmetric!) difference
  of the sets of integers represented by \code{x} and \code{y}.

  \code{punion}, \code{pintersect} and \code{psetdiff}
  are generic functions that perform the element-wise (aka "parallel")
  union, intersection and (asymmetric!) difference of \code{x}
  and \code{y}.
  Methods for \link{IRanges} objects are defined. For these methods,
  \code{x} and \code{y} must have the same length (i.e. same number
  of ranges) and they return an \link{IRanges} instance of the
  same length as \code{x} and \code{y} where each range represents
  the union/intersection/difference of the corresponding ranges in
  \code{x} and \code{y}.

  Note that the union or difference of 2 ranges cannot always
  be represented by a single range so \code{punion} and \code{psetdiff}
  cannot always return their result in an \link{IRanges} instance of
  the same length as the input. This happens to \code{punion} when
  there is a gap between the 2 ranges to combine. In that case,
  the user can use the \code{fill.gap} argument to enforce the union
  by filling the gap. This happens to \code{psetdiff} when a range
  in \code{y} has its end points strictly inside the corresponding
  range in \code{x}. In that case, \code{psetdiff} will simply fail.

  \code{pgap} returns the gap between each element in \code{x} and
  its corresponding element in \code{y}, in parallel fashion, returning
  an \code{IRanges} instance. If two ranges overlap, the gap is empty.
}

\author{H. Pages and M. Lawrence}

\seealso{
  \link{Ranges-class},
  \link{IRanges-class},
  \link{IRanges-utils}
}

\examples{
  x <- IRanges(c(1, 5, -2, 0, 14), c(10, 9, 3, 11, 17))
  y <- IRanges(c(14, 0, -5, 6, 18), c(20, 2, 2, 8, 20)) 

  ## Vector-wise operations:
  union(x, y)
  intersect(x, y)
  setdiff(x, y)
  setdiff(y, x)

  ## Element-wise (aka "parallel") operations:
  try(punion(x, y))
  punion(x[3:5], y[3:5])
  punion(x, y, fill.gap=TRUE)
  pintersect(x, y)
  psetdiff(y, x)
  try(psetdiff(x, y))
  start(x)[4] <- -99
  end(y)[4] <- 99
  psetdiff(x, y)
  pgap(x, y)
}

\keyword{utilities}
