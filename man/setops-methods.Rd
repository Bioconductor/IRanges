\name{setops-methods}

\alias{setops-methods}

\alias{union}
\alias{union,Ranges,Ranges-method}
\alias{union,RangesList,RangesList-method}
\alias{union,CompressedIRangesList,CompressedIRangesList-method}
\alias{union,Pairs,missing-method}

\alias{intersect}
\alias{intersect,Ranges,Ranges-method}
\alias{intersect,RangesList,RangesList-method}
\alias{intersect,CompressedIRangesList,CompressedIRangesList-method}
\alias{intersect,Pairs,missing-method}

\alias{setdiff}
\alias{setdiff,Ranges,Ranges-method}
\alias{setdiff,RangesList,RangesList-method}
\alias{setdiff,CompressedIRangesList,CompressedIRangesList-method}
\alias{setdiff,Pairs,missing-method}

\alias{punion}
\alias{punion,Ranges,Ranges-method}
\alias{punion,Pairs,missing-method}

\alias{pintersect}
\alias{pintersect,Ranges,Ranges-method}
\alias{pintersect,Pairs,missing-method}

\alias{psetdiff}
\alias{psetdiff,Ranges,Ranges-method}
\alias{psetdiff,Pairs,missing-method}

\alias{pgap}
\alias{pgap,Ranges,Ranges-method}

\title{Set operations on Ranges and RangesList objects}

\description{
  Performs set operations on \link{Ranges} and \link{RangesList} objects.
}

\usage{
## Vector-wise set operations
## --------------------------

\S4method{union}{Ranges,Ranges}(x, y)
\S4method{union}{Pairs,missing}(x, y, ...)

\S4method{intersect}{Ranges,Ranges}(x, y)
\S4method{intersect}{Pairs,missing}(x, y, ...)

\S4method{setdiff}{Ranges,Ranges}(x, y)
\S4method{setdiff}{Pairs,missing}(x, y, ...)

## Element-wise (aka "parallel") set operations
## --------------------------------------------

\S4method{punion}{Ranges,Ranges}(x, y, fill.gap=FALSE)
\S4method{punion}{Pairs,missing}(x, y, ...)

\S4method{pintersect}{Ranges,Ranges}(x, y, resolve.empty=c("none", "max.start", "start.x"))
\S4method{pintersect}{Pairs,missing}(x, y, ...)

\S4method{psetdiff}{Ranges,Ranges}(x, y)
\S4method{psetdiff}{Pairs,missing}(x, y, ...)

\S4method{pgap}{Ranges,Ranges}(x, y)
}

\arguments{
  \item{x, y}{
    Objects representing ranges.
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
    The methods for \link[S4Vectors]{Pairs} objects pass any extra argument
    to the internal call to \code{punion(first(x), last(x), ...)},
    \code{pintersect(first(x), last(x), ...)}, etc...
  }
}

\details{
  The \code{union}, \code{intersect} and \code{setdiff} methods
  for \link{Ranges} objects return a "normal" \link{Ranges}
  object representing the union, intersection and (asymmetric!)
  difference of the sets of integers represented by \code{x} and
  \code{y}.

  \code{punion}, \code{pintersect}, \code{psetdiff} and \code{pgap}
  are generic functions that compute the element-wise (aka "parallel")
  union, intersection, (asymmetric!) difference and gap between
  each element in \code{x} and its corresponding element in \code{y}.
  Methods for \link{Ranges} objects are defined. For these methods,
  \code{x} and \code{y} must have the same length (i.e. same number
  of ranges). They return a \link{Ranges} object \emph{parallel}
  to \code{x} and \code{y} i.e. where the i-th range corresponds
  to the i-th range in \code{x} and in\code{y}) and represents
  the union/intersection/difference/gap of/between the corresponding
  \code{x[i]} and \code{y[i]}.

  If \code{x} is a \code{\link[S4Vectors]{Pairs}} object, then \code{y}
  should be missing, and the operation is performed between the members
  of each pair.

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

\value{
  On \link{Ranges} objects, \code{union}, \code{intersect}, and \code{setdiff}
  return an \link{IRanges} \emph{instance} that is guaranteed to be
  \emph{normal} (see \code{\link{isNormal}}) but is NOT
  promoted to \link{NormalIRanges}.

  On \link{Ranges} objects, \code{punion}, \code{pintersect}, \code{psetdiff},
  and \code{pgap} return an object of the same class and length as their first
  argument.
}

\author{H. Pagès and M. Lawrence}

\seealso{
  \itemize{
    \item \code{pintersect} is similar to \code{\link{narrow}}, except the
          end points are absolute, not relative. \code{pintersect} is also
          similar to \code{\link{restrict}}, except ranges outside of the
          restriction become empty and are not discarded.

    \item \link[GenomicRanges]{setops-methods} in the \pkg{GenomicRanges}
          package for set operations on genomic ranges.

    \item \link{findOverlaps-methods} for finding/counting overlapping ranges.

    \item \link{intra-range-methods} and
          \link{inter-range-methods} for intra range and
          inter range transformations.

    \item \link{Ranges} and \link{RangesList} objects. In particular,
          \emph{normality} of a \link{Ranges} object is discussed in
          the man page for \link{Ranges} objects.

    \item \code{\link[S4Vectors]{mendoapply}} in the \pkg{S4Vectors} package.
  }
}

\examples{
x <- IRanges(c(1, 5, -2, 0, 14), c(10, 9, 3, 11, 17))
subject <- Rle(1:-3, 6:2)
y <- Views(subject, start=c(14, 0, -5, 6, 18), end=c(20, 2, 2, 8, 20))

## Vector-wise operations:
union(x, ranges(y))
union(ranges(y), x)

intersect(x, ranges(y))
intersect(ranges(y), x)

setdiff(x, ranges(y))
setdiff(ranges(y), x)

## Element-wise (aka "parallel") operations:
try(punion(x, ranges(y)))
punion(x[3:5], ranges(y)[3:5])
punion(x, ranges(y), fill.gap=TRUE)
try(pintersect(x, ranges(y)))
pintersect(x[3:4], ranges(y)[3:4])
pintersect(x, ranges(y), resolve.empty="max.start")
psetdiff(ranges(y), x)
try(psetdiff(x, ranges(y)))
start(x)[4] <- -99
end(y)[4] <- 99
psetdiff(x, ranges(y))
pgap(x, ranges(y))

## On RangesList objects:
irl1 <- IRangesList(a=IRanges(c(1,2),c(4,3)), b=IRanges(c(4,6),c(10,7)))
irl2 <- IRangesList(c=IRanges(c(0,2),c(4,5)), a=IRanges(c(4,5),c(6,7)))
union(irl1, irl2)
intersect(irl1, irl2)
setdiff(irl1, irl2)
}

\keyword{utilities}
