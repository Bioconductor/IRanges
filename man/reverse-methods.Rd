\name{reverse}

\alias{reverse}
\alias{reverse,character-method}
\alias{reverse,IRanges-method}
\alias{reverse,NormalIRanges-method}
\alias{reverse,Views-method}
\alias{reverse,MaskCollection-method}


\title{reverse}

\description{
  A generic function for reversing vector-like or list-like objects.
  This man page describes methods for reversing a character vector,
  a \link{Views} object, or a \link{MaskCollection} object.
  Note that \code{reverse} is similar to but not the same as
  \code{\link[base]{rev}}.
}

\usage{
reverse(x, ...)
}

\arguments{
  \item{x}{
    A vector-like or list-like object.
  }
  \item{...}{
    Additional arguments to be passed to or from methods.
  }
}

\details{
  On a character vector or a \link{Views} object, \code{reverse} reverses
  each element individually, without modifying the top-level order of the
  elements. More precisely, each individual string of a character vector
  is reversed.
}

\value{
  An object of the same class and length as the original object.
}

\seealso{
  \link[XVector]{reverse-methods},
  \link{Views-class},
  \link{MaskCollection-class},
  \code{\link{endoapply}},
  \code{\link[base]{rev}}
}

\examples{
## On a character vector:
reverse(c("Hi!", "How are you?"))
rev(c("Hi!", "How are you?"))

## On a Views object:
v <- successiveViews(Rle(c(-0.5, 12.3, 4.88), 4:2), 1:4)
v
reverse(v)
rev(v)

## On a MaskCollection object:
mask1 <- Mask(mask.width=29, start=c(11, 25, 28), width=c(5, 2, 2))
mask2 <- Mask(mask.width=29, start=c(3, 10, 27), width=c(5, 8, 1))
mask3 <- Mask(mask.width=29, start=c(7, 12), width=c(2, 4))
mymasks <- append(append(mask1, mask2), mask3)
reverse(mymasks)
}

\keyword{methods}
\keyword{manip}
