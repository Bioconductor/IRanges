\name{IRanges-setops}

\alias{IRanges-setops}

\alias{gaps}
\alias{gaps,IRanges-method}
\alias{gaps,Ranges-method}
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
gaps(x, start=NA, end=NA)
\S4method{union}{IRanges,IRanges}(x, y,...)
\S4method{intersect}{IRanges,IRanges}(x, y,...)
\S4method{setdiff}{IRanges,IRanges}(x, y,...)

## Element-wise (aka "parallel") operations:
\S4method{punion}{IRanges,IRanges}(x, y, fill.gap=FALSE, ...)
\S4method{pintersect}{IRanges,IRanges}(x, y, resolve.empty=c("none", "max.start", "start.x"), ...)
\S4method{psetdiff}{IRanges,IRanges}(x, y, ...)
\S4method{pgap}{IRanges,IRanges}(x, y, ...)
}

\arguments{
  \item{x, y}{
    \link{IRanges} objects.
  }
  \item{start, end}{
    A single integer or \code{NA}. Use these arguments to specify the
    interval of reference i.e. which interval the returned gaps
    should be relative to.
  }
  \item{fill.gap}{
    Logical indicating whether or not to force a union by using the rule
    \code{start = min(start(x), start(y)), end = max(end(x), end(y))}.
  }
  \item{resolve.empty}{
    One of \code{"none"}, \code{"max.start"}, or \code{"start.x"} denoting
    how to handle ambiguous empty ranges formed by intersections.
    \code{"none"} - throw an error if an ambiguous empty range is formed,
    \code{"max.start"} - associate the maximum start value with any
    ambiguous empty range, and \code{"start.x"} - associate the start value
    of \code{x} with any ambiguous empty range. (See Details section
    below for the definition of an ambiguous range.)
  }
  \item{...}{
    Further arguments to be passed to or from other methods.
  }
}

\details{
  \code{gaps} returns the "normal" \link{IRanges} object (of the same
  class as \code{x}) representing the set of integers that remain
  after the set of integers represented by \code{x} has been removed
  from the interval specified by the \code{start} and \code{end} arguments.

  The \code{union}, \code{intersect} and \code{setdiff} methods
  for \link{IRanges} objects return a "normal" \link{IRanges}
  object (of the same class as \code{x}) representing the union,
  intersection and (asymmetric!) difference of the sets of integers
  represented by \code{x} and \code{y}.

  \code{punion}, \code{pintersect}, \code{psetdiff} and \code{pgap}
  are generic functions that compute the element-wise (aka "parallel")
  union, intersection, (asymmetric!) difference and gap between
  each element in \code{x} and its corresponding element in \code{y}.
  Methods for \link{IRanges} objects are defined. For these methods,
  \code{x} and \code{y} must have the same length (i.e. same number
  of ranges) and they return an \link{IRanges} instance of the
  same length as \code{x} and \code{y} where each range represents
  the union/intersection/difference/gap of/between the corresponding
  ranges in \code{x} and \code{y}.

  By default, \code{pintersect} will throw an error when an "ambiguous
  empty range" is formed. An ambiguous empty range can occur three
  different ways:  1) when corresponding non-empty ranges elements \code{x}
  and \code{y} have an empty intersection, 2) if the position of an empty
  range element does not fall within the corresponding limits of a non-empty
  range element, or 3) if two corresponding empty range elements do not have
  the same position. For example if empty range element [22,21] is intersected
  with non-empty range element [1,10], an error will be produced; but if
  it is intersected with the range [22,28], it will produce [22,21].
  As mentioned in the Arguments section above, this behavior can be
  changed using the \code{resolve.empty} argument.
}

\author{H. Pages and M. Lawrence}

\seealso{
  \code{pintersect} is similar to \code{\link{narrow}}, except the
  end points are absolute, not relative. \code{pintersect} is also
  similar to \code{\link{restrict}}, except ranges outside of the
  restriction become empty and are not discarded.

  \link[base:sets]{union}, \link[base:sets]{intersect}, \link[base:sets]{setdiff},
  \link{Ranges-class},
  \link{Ranges-utils},
  \link{IRanges-class},
  \link{IRanges-utils}
}

\examples{
  x0 <- IRanges(start=c(-2, 6, 9, -4, 1, 0, -6, 10),
                width=c( 5, 0, 6,  1, 4, 3,  2,  3))
  gaps(x0)
  gaps(x0, start=-6, end=20)  # Regions of the -6:20 range that are not masked by 'x0'.

  x <- IRanges(c(1, 5, -2, 0, 14), c(10, 9, 3, 11, 17))
  y <- Views(as(4:-17, "XInteger"), start=c(14, 0, -5, 6, 18), end=c(20, 2, 2, 8, 20))

  ## Vector-wise operations:
  union(x, y)
  union(y, x)

  intersect(x, y)
  intersect(y, x)

  setdiff(x, y)
  setdiff(y, x)

  ## Element-wise (aka "parallel") operations:
  try(punion(x, y))
  punion(x[3:5], y[3:5])
  punion(x, y, fill.gap=TRUE)
  try(pintersect(x, y))
  pintersect(x[3:4], y[3:4])
  pintersect(x, y, resolve.empty="max.start")
  psetdiff(y, x)
  try(psetdiff(x, y))
  start(x)[4] <- -99
  end(y)[4] <- 99
  psetdiff(x, y)
  pgap(x, y)
}

\keyword{utilities}
