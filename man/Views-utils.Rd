\name{Views-utils}
\alias{Views-utils}

\alias{slice}
\alias{slice,integer-method}
\alias{slice,numeric-method}
\alias{slice,Rle-method}
\alias{slice,RleList-method}
\alias{slice,XInteger-method}
\alias{slice,XDouble-method}

\alias{viewApply}
\alias{viewApply,Views-method}
\alias{viewApply,RleViews-method}
\alias{viewApply,RleViewsList-method}
\alias{viewMins}
\alias{viewMins,RleViews-method}
\alias{viewMins,RleViewsList-method}
\alias{viewMins,XIntegerViews-method}
\alias{viewMins,XDoubleViews-method}
\alias{viewMaxs}
\alias{viewMaxs,RleViews-method}
\alias{viewMaxs,RleViewsList-method}
\alias{viewMaxs,XIntegerViews-method}
\alias{viewMaxs,XDoubleViews-method}
\alias{viewSums}
\alias{viewSums,RleViews-method}
\alias{viewSums,RleViewsList-method}
\alias{viewSums,XIntegerViews-method}
\alias{viewSums,XDoubleViews-method}
\alias{viewMeans}
\alias{viewMeans,RleViews-method}
\alias{viewMeans,RleViewsList-method}
\alias{viewMeans,XIntegerViews-method}
\alias{viewMeans,XDoubleViews-method}
\alias{viewWhichMins}
\alias{viewWhichMins,RleViews-method}
\alias{viewWhichMins,RleViewsList-method}
\alias{viewWhichMins,XIntegerViews-method}
\alias{viewWhichMins,XDoubleViews-method}
\alias{viewWhichMaxs}
\alias{viewWhichMaxs,RleViews-method}
\alias{viewWhichMaxs,RleViewsList-method}
\alias{viewWhichMaxs,XIntegerViews-method}
\alias{viewWhichMaxs,XDoubleViews-method}
\alias{viewRangeMins}
\alias{viewRangeMins,RleViews-method}
\alias{viewRangeMins,RleViewsList-method}
\alias{viewRangeMaxs}
\alias{viewRangeMaxs,RleViews-method}
\alias{viewRangeMaxs,RleViewsList-method}

\alias{Summary,Views-method}
\alias{mean,Views-method}
\alias{max,Views-method}
\alias{min,Views-method}
\alias{sum,Views-method}
\alias{which.min,Views-method}
\alias{which.max,Views-method}

\title{Utility functions and numeric summary of Views of numerics}

\description{
  The \code{slice} function creates a \linkS4class{Views} object that contains
  the indices where the data are within the specified bounds.

  The \code{viewMins}, \code{viewMaxs}, \code{viewSums}, \code{viewMeans}
  functions calculate the minima, maxima, sums, and means on views respectively.
}

\usage{
viewApply(X, FUN, ..., simplify = TRUE)

slice(x, lower=-Inf, upper=Inf, ...)
\S4method{slice}{Rle}(x, lower=-Inf, upper=Inf,
                      includeLower=TRUE, includeUpper=TRUE,
                      rangesOnly = FALSE)

viewMins(x, na.rm=FALSE)
\S4method{min}{Views}(x, ..., na.rm = FALSE)
viewMaxs(x, na.rm=FALSE)
\S4method{max}{Views}(x, ..., na.rm = FALSE)
viewSums(x, na.rm=FALSE)
\S4method{sum}{Views}(x, ..., na.rm = FALSE)
viewMeans(x, na.rm=FALSE)
\S4method{mean}{Views}(x, ...)

viewWhichMins(x, na.rm=FALSE)
\S4method{which.min}{Views}(x)
viewWhichMaxs(x, na.rm=FALSE)
\S4method{which.max}{Views}(x)

viewRangeMins(x, na.rm=FALSE)
viewRangeMaxs(x, na.rm=FALSE)
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
    An \linkS4class{Rle}, \linkS4class{RleList}, \linkS4class{XInteger} object
    or an integer vector for \code{slice}.

    An \linkS4class{RleViews}, \linkS4class{RleViewsList},
    \linkS4class{XIntegerViews} or \linkS4class{XDoubleViews} object
    for \code{viewMins}, \code{viewMaxs}, \code{viewSums}, and \code{viewMeans}.

    An \linkS4class{Rle}, \linkS4class{RleViewsList},
    \linkS4class{XIntegerViews} or \linkS4class{XDoubleViews} object
    for \code{viewWhichMins} and \code{viewWhichMaxs}.
  }
  \item{lower, upper}{
    The lower and upper bounds for the slice.
  }
  \item{includeLower, includeUpper}{
    Logical indicating whether or not the specified boundary is open or closed.
  }
  \item{rangesOnly}{
    For \linkS4class{Rle} and \linkS4class{RleList} objects, a logical
    indicating whether or not to drop the original data from the output.
  }
  \item{na.rm}{
    Logical indicating whether or not to include missing values in the results.
  }
}

\details{
  The \code{slice} function is useful for finding areas of absolute maxima
  (peaks), absolute minima (troughs), or fluctuations within specified limits.

  The \code{viewMins}, \code{viewMaxs}, \code{viewSums}, and \code{viewMeans}
  functions provide efficient methods for calculating the specified numeric
  summary by performing the looping in compiled code.

  The \code{viewWhichMins}, \code{viewWhichMaxs}, \code{viewRangeMins}, and
  \code{viewRangeMaxs} functions provide efficient methods for finding the
  locations of the minima and maxima.

  For convenience, methods for \code{min}, \code{max}, \code{sum},
  \code{mean}, \code{which.min} and \code{which.max} are provided as
  wrappers around the corresponding \code{view*} functions (which might
  be deprecated at some point).
}

\value{
  An \linkS4class{RleViews} object for \linkS4class{Rle} or an
  \linkS4class{RleViewsList} for \linkS4class{RleList} containing the
  views when using \code{slice} with \code{rangesOnly = FALSE}.
  An \linkS4class{IRanges} object for \linkS4class{Rle} or a
  \linkS4class{CompressedIRangesList} for \linkS4class{RleList} containing
  the ranges when using \code{slice} with \code{rangesOnly = TRUE}.
  An \linkS4class{XIntegerViews} for an \linkS4class{XInteger} object
  when using \code{slice}.
  An \linkS4class{XDoubleViews} for an \linkS4class{XDouble} object
  when using \code{slice}.

  A vector of \code{length(x)} for \linkS4class{RleViews},
  \linkS4class{XIntegerViews} and \linkS4class{XDoubleViews} objects,
  or a \linkS4class{SimpleList} object of \code{length(x)} for
  \linkS4class{RleViewsList} objects containing the numeric summaries
  for the views for \code{viewMins}, \code{viewMaxs},
  \code{viewSums}, \code{viewMeans}, \code{viewWhichMins}, and
  \code{viewWhichMaxs}.

  An \linkS4class{IRanges} object for \linkS4class{RleViews} objects or a
  \linkS4class{SimpleIRangesList} for \linkS4class{RleViewsList} objects
  containing the location ranges for \code{viewRangeMins} and
  \code{viewRangeMaxs}.
}

\author{P. Aboyoun}

\seealso{
  \link{RleViews-class}, \link{RleViewsList-class}, \link{XIntegerViews-class},
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
  viewMeans(vecViews)
  viewWhichMins(vecViews)
  viewWhichMaxs(vecViews)

  ## Views derived from coverage
  x <- IRanges(start=c(1L, 9L, 4L, 1L, 5L, 10L),
               width=c(5L, 6L, 3L, 4L, 3L,  3L))
  slice(coverage(x), lower=2)
  slice(coverage(x), lower=2, rangesOnly = TRUE)
}

\keyword{methods}
\keyword{algebra}
\keyword{arith}
