\name{disjoin}

\alias{disjoin}
\alias{disjointBins}
\alias{disjoin,Ranges-method}
\alias{disjointBins,Ranges-method}

\title{Making Ranges disjoint}

\description{
  Functions for making \code{\linkS4class{Ranges}} disjoint,
  where no ranges overlap another.
}

\usage{
  disjoin(x, ...)
  disjointBins(x, ...)
}

\arguments{
  \item{x}{The \code{\linkS4class{Ranges}} instance, possibly
    overlapping intervals.}
  \item{...}{Additional arguments for methods}
}

\details{
  
  The \code{disjoin} method returns a disjoint
  \code{\linkS4class{Ranges}}, by finding the union of the end points in
  \code{x}. In other words, the result consists of a range for every
  interval, of maximal length, over which the set of overlapping ranges
  in \code{x} is the same and at least of size 1.

  \code{disjointBins} segregates \code{x} into a set of bins so that the
  ranges in each bin are disjoint. Lower-indexed bins are filled
  first. The method returns an integer vector indicating the bin index
  for each range.
  
}

\author{M. Lawrence}

\seealso{
  \code{\link{reduce}} for making normal ranges, a subset
  of disjoint ranges, where there must be a gap of length >= 1 between
  each range.
}

\examples{
  ir <- IRanges(c(1, 1, 4, 10), c(6, 3, 8, 10))
  disjoin(ir) # IRanges(c(1, 4, 7, 10), c(3, 6, 8, 10))

  disjointBins(IRanges(1, 5)) # 1L
  disjointBins(IRanges(c(3, 1, 10), c(5, 12, 13))) # c(2L, 1L, 2L)
}

\keyword{utilities}
