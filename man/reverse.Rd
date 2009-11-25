\name{reverse}

\alias{reverse}
\alias{reverse,IRanges-method}
\alias{reverse,NormalIRanges-method}
\alias{reverse,SharedRaw-method}
\alias{reverse,SharedVector_Pool-method}
\alias{reverse,MaskCollection-method}


\title{Reverse an IRanges object}

\description{
  This operation is not intended to be used directly.
}

\usage{
  reverse(x, \dots)
}

\arguments{
  \item{x}{
    An IRanges object.
  }
  \item{\dots}{
    Additional arguments to be passed to or from methods.
  }
}

\value{
  An object of the same class and length as the original object.
}

\examples{
  ## WARNING: The examples here are not relevant anymore and will be
  ## updated soon.
  x <- IRanges(start=c(-2L, 6L, 9L, -4L, 1L, 0L, -6L, 10L),
               width=c( 5L, 0L, 6L,  1L, 4L, 3L,  2L,  3L))
  reverse(x, start=-6, end=20)

  ## When omitted, 'start' and 'end' are considered to be 'min(start(x))'
  ## and 'max(end(x))', respectively:
  reverse(x)

  reverse(shift(x, 2), start=-6, end=20)
  reverse(restrict(x, 1, 10), start=-6, end=20)
  reverse(reduce(x), start=-6, end=20)
  reverse(gaps(x, start=-6, end=20), start=-6, end=20)

  mask1 <- Mask(mask.width=29, start=c(11, 25, 28), width=c(5, 2, 2))
  mask2 <- Mask(mask.width=29, start=c(3, 10, 27), width=c(5, 8, 1))
  mask3 <- Mask(mask.width=29, start=c(7, 12), width=c(2, 4))
  mymasks <- append(append(mask1, mask2), mask3)
  reverse(mymasks)
}

\keyword{methods}
\keyword{manip}
