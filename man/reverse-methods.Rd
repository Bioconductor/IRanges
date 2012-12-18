\name{reverse}

\alias{reverse}
\alias{reverse,character-method}
\alias{reverse,IRanges-method}
\alias{reverse,NormalIRanges-method}
\alias{reverse,SharedRaw-method}
\alias{reverse,SharedVector_Pool-method}
\alias{reverse,Views-method}
\alias{reverse,MaskCollection-method}
\alias{reverse,XVector-method}
\alias{rev,XVector-method}
\alias{reverse,XVectorList-method}


\title{reverse}

\description{
  A generic function with methods for reversing a character vector,
  a \link{Views} object, an \link{XVector} object, an \link{XVectorList}
  object, or a \link{MaskCollection} object.
  Note that \code{reverse} is similar to but not the same as
  \code{\link[base]{rev}}.
}

\usage{
reverse(x, \dots)
}

\arguments{
  \item{x}{
    A character vector, a \link{Views} object, an \link{XVector} object,
    an \link{XVectorList} object, or a \link{MaskCollection} object.
  }
  \item{\dots}{
    Additional arguments to be passed to or from methods.
  }
}

\details{
  On an \link{XVector} object, \code{reverse} and \code{\link[base]{rev}}
  are equivalent, i.e. they both reverse the order of their elements.

  On a character vector, a \link{Views} object, or an \link{XVectorList}
  object, \code{reverse} reverses each element individually, without
  modifying the top-level order of the elements.
  In particular, on \link{XVectorList} object \code{x}, \code{reverse} is
  equivalent to \code{endoapply(x, rev)}.
}

\value{
  An object of the same class and length as the original object.
}

\seealso{
  \link{Views-class},
  \link{XVector-class},
  \link{XVectorList-class},
  \link{MaskCollection-class},
  \code{\link{endoapply}},
  \code{\link[base]{rev}}
}

\examples{
## On a character vector:
reverse(c("Hi!", "How are you?"))

## On a Views object:
v <- successiveViews(as(12:-2, "XInteger"), 1:5)
v
reverse(v)

## On an XVectorList object:
\dontrun{
  library(Biostrings)
  library(drosophila2probe)
  probes <- DNAStringSet(drosophila2probe)
  reverse(probes)
}

## On a MaskCollection object:
mask1 <- Mask(mask.width=29, start=c(11, 25, 28), width=c(5, 2, 2))
mask2 <- Mask(mask.width=29, start=c(3, 10, 27), width=c(5, 8, 1))
mask3 <- Mask(mask.width=29, start=c(7, 12), width=c(2, 4))
mymasks <- append(append(mask1, mask2), mask3)
reverse(mymasks)
}

\keyword{methods}
\keyword{manip}
