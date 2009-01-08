\name{Views-utils}
\alias{Views-utils}

\alias{slice}
\alias{slice,integer-method}
\alias{slice,Rle-method}
\alias{slice,XInteger-method}
\alias{slice,XRleInteger-method}

\alias{viewApply}
\alias{viewApply,Views-method}
\alias{viewApply,RleViews-method}
\alias{viewMins}
\alias{viewMins,RleViews-method}
\alias{viewMins,XIntegerViews-method}
\alias{viewMins,XRleIntegerViews-method}
\alias{viewMaxs}
\alias{viewMaxs,RleViews-method}
\alias{viewMaxs,XIntegerViews-method}
\alias{viewMaxs,XRleIntegerViews-method}
\alias{viewSums}
\alias{viewSums,RleViews-method}
\alias{viewSums,XIntegerViews-method}
\alias{viewSums,XRleIntegerViews-method}
\alias{viewWhichMins}
\alias{viewWhichMins,RleViews-method}
\alias{viewWhichMins,XIntegerViews-method}
\alias{viewWhichMins,XRleIntegerViews-method}
\alias{viewWhichMaxs}
\alias{viewWhichMaxs,RleViews-method}
\alias{viewWhichMaxs,XIntegerViews-method}
\alias{viewWhichMaxs,XRleIntegerViews-method}


\title{Utility functions and numeric summary of Views of numerics}

\description{
  The \code{slice} function creates a \link{Views} object that contains the
  indices where the data are within the specified bounds.

  The \code{viewMins}, \code{viewMaxs}, \code{viewSums} functions calculate
  the minimums, maximums, and sums on views respectively.
}

\usage{
  viewApply(X, FUN, ..., simplify = TRUE)

  slice(x, lower=-Inf, upper=Inf, ...)
  \S4method{slice}{Rle}(x, lower=-Inf, upper=Inf,
                        includeLower=TRUE, includeUpper=TRUE)

  viewMins(x, na.rm=FALSE)
  viewMaxs(x, na.rm=FALSE)
  viewSums(x, na.rm=FALSE)

  viewWhichMins(x, na.rm=FALSE)
  viewWhichMaxs(x, na.rm=FALSE)
}

\arguments{
  \item{X}{
    A Views object.
  }
  \item{FUN}{
    The function to be applied to each view in \code{X}.
  }
  \item{\dots}{
    Additional arguments to be passed on.
  }
  \item{simplify}{
    A logical value specifying whether or not the result should be simplified
    to a vector or matrix if possible.
  }
  \item{x}{
    An \link{Rle}, \link{XRleInteger}, \link{XInteger} object or an integer
    vector for \code{slice}.

    An \link{RleViews}, \link{XRleIntegerViews}, \link{XIntegerViews} object for
    \code{viewMins}, \code{viewMaxs}, and \code{viewSums}.

    An \link{Rle}, \link{XRleIntegerViews}, \link{XIntegerViews} object for
    \code{viewWhichMins} and \code{viewWhichMaxs}.
  }
  \item{lower, upper}{
    The lower and upper bounds for the slice.
  }
  \item{includeLower, includeUpper}{
    Logical indicating whether or not the specified boundary is open or closed.
  }
  \item{na.rm}{
    Logical indicating whether or not to include missing values in the results.
  }
}

\details{
  The \code{slice} function creates views on \link{XRleInteger} or
  \link{XInteger} objects where the data are within the specified bounds. This
  is useful for finding areas of absolute maxima (peaks), absolute minima
  (troughs), or fluxuations within a specified limits.

  The \code{viewMins}, \code{viewMaxs}, \code{viewSums}, \code{viewWhichMins},
  and \code{viewWhichMaxs} functions provide efficient methods for calculating
  the specified numeric summary by performing the looping in compiled code.
}

\value{
  An \link{XRleIntegerViews} or \link{XIntegerViews} object for \code{slice}.

  A vector of \code{length(x)} containing the numeric summaries for the views.
}

\author{P. Aboyoun}

\seealso{
  \link{RleViews-class}, \link{XRleIntegerViews-class}, \link{XIntegerViews-class},
  \code{\link{which.min}}, \code{\link{colSums}}
}

\examples{
  ## Views derived from vector
  vec <- as.integer(c(19, 5, 0, 8, 5))
  slice(vec, lower=5, upper=8)

  set.seed(0)
  vec <- sample(24)
  vecViews <- slice(vec, lower=4, upper=16)
  vecViews
  viewApply(vecViews, function(x) diff(as.integer(x)))
  viewMins(vecViews)
  viewMaxs(vecViews)
  viewSums(vecViews)
  viewWhichMins(vecViews)
  viewWhichMaxs(vecViews)

  ## Views derived from coverage
  x <- IRanges(start=c(1L, 9L, 4L, 1L, 5L, 10L),
               width=c(5L, 6L, 3L, 4L, 3L,  3L))
  coverage(x, start=1, end=15)
}

\keyword{methods}
\keyword{algebra}
\keyword{arith}
