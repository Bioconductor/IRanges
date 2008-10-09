\name{coverage}
\alias{coverage}
\alias{coverage,IRanges-method}
\alias{coverage,MaskCollection-method}

\title{Coverage across a set of ranges}

\description{
  Counts the number of times a position is represented in a set of ranges. 
}

\usage{
  coverage(x, start=NA, end=NA, \dots)
  \S4method{coverage}{IRanges}(x, start=NA, end=NA, weight=1L)
}

\arguments{
  \item{x}{
    An \link{IRanges} or \link{MaskCollection} object.
  }
  \item{start}{
    A single integer specifying the position in \code{x} where to start the
    extraction of the coverage.
  }
  \item{end}{
    A single integer specifying the position in \code{x} where to end the
    extraction of the coverage.
  }
  \item{weight}{
    An integer vector specifying how much each element in \code{x} counts.
  }
  \item{\dots}{
    Further arguments to be passed to or from other methods.
  }
}

\value{
  An \link{XRleInteger} object representing the coverage of \code{x} in the
  interval specified by the \code{start} and \code{end} arguments. An integer
  value called the "coverage" can be associated to each position in \code{x},
  indicating how many times this position is covered by the ranges stored in
  \code{x}. Note that the positions in the returned \link{XInteger} object are
  to be interpreted as relative to the interval specified by the \code{start}
  and \code{end} arguments.
}

\seealso{
  \link{XRleInteger-class},
  \link{IRanges-class},
  \link{MaskCollection-class}
}

\examples{
  x <- IRanges(start=c(-2L, 6L, 9L, -4L, 1L, 0L, -6L, 10L),
               width=c( 5L, 0L, 6L,  1L, 4L, 3L,  2L,  3L))
  coverage(x, start=-6, end=20)  # 'start' and 'end' must be specified for
                                 # an IRanges object.
  coverage(shift(x, 2), start=-6, end=20)
  coverage(restrict(x, 1, 10), start=-6, end=20)
  coverage(reduce(x), start=-6, end=20)
  coverage(gaps(x, start=-6, end=20), start=-6, end=20)

  mask1 <- Mask(mask.width=29, start=c(11, 25, 28), width=c(5, 2, 2))
  mask2 <- Mask(mask.width=29, start=c(3, 10, 27), width=c(5, 8, 1))
  mask3 <- Mask(mask.width=29, start=c(7, 12), width=c(2, 4))
  mymasks <- append(append(mask1, mask2), mask3)
  coverage(mymasks)
}

\keyword{methods}
