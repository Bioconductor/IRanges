\name{view-summarization-methods}
\alias{view-summarization-methods}

\alias{slice}
\alias{slice,Rle-method}
\alias{slice,RleList-method}

\alias{viewApply}
\alias{viewApply,Views-method}
\alias{viewApply,RleViews-method}
\alias{viewApply,RleViewsList-method}
\alias{viewMins}
\alias{viewMins,RleViews-method}
\alias{viewMins,RleViewsList-method}
\alias{viewMaxs}
\alias{viewMaxs,RleViews-method}
\alias{viewMaxs,RleViewsList-method}
\alias{viewSums}
\alias{viewSums,RleViews-method}
\alias{viewSums,RleViewsList-method}
\alias{viewMeans}
\alias{viewMeans,RleViews-method}
\alias{viewMeans,RleViewsList-method}
\alias{viewWhichMins}
\alias{viewWhichMins,RleViews-method}
\alias{viewWhichMins,RleViewsList-method}
\alias{viewWhichMaxs}
\alias{viewWhichMaxs,RleViews-method}
\alias{viewWhichMaxs,RleViewsList-method}
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


\title{Summarize views on a vector-like object with numeric values}

\description{
  The \code{slice} function creates a \link{Views} object that contains
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
    An \link{Rle} or \link{RleList} object for \code{slice}.

    An \link{RleViews} or \link{RleViewsList} object for the \code{view*}
    functions.
  }
  \item{lower, upper}{
    The lower and upper bounds for the slice.
  }
  \item{includeLower, includeUpper}{
    Logical indicating whether or not the specified boundary is open or closed.
  }
  \item{rangesOnly}{
    A logical indicating whether or not to drop the original data from the
    output.
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
}

\value{
  For \code{slice}: The method for \link{Rle} objects returns an
  \link{RleViews} object if \code{rangesOnly=FALSE} or an \link{IRanges}
  object if \code{rangesOnly=TRUE}.
  The method for \link{RleList} objects returns an \link{RleViewsList}
  object if \code{rangesOnly=FALSE} or an \link{IRangesList} object
  if \code{rangesOnly=TRUE}.

  For all the \code{view*} functions (except \code{viewRangeMins} and
  \code{viewRangeMaxs}): A numeric vector of the length of \code{x}
  if \code{x} is an \link{RleViews} object, or a \link{List} object of
  the length of \code{x} if it's an \link{RleViewsList} object.

  For \code{viewRangeMins} and \code{viewRangeMaxs}: An \link{IRanges}
  object if \code{x} is an \link{RleViews} object, or an \link{IRangesList}
  object if it's an \link{RleViewsList} object.
}

\note{
  For convenience, methods for \code{min}, \code{max}, \code{sum},
  \code{mean}, \code{which.min} and \code{which.max} are provided as
  wrappers around the corresponding \code{view*} functions (which might
  be deprecated at some point).
}

\author{P. Aboyoun}

\seealso{
  \link[XVector]{view-summarization-methods},
  \link{Rle-class}, \link{RleViews-class}, \link{RleViewsList-class},
  \code{\link{which.min}}, \code{\link{colSums}}
}

\examples{
## Views derived from coverage
x <- IRanges(start=c(1L, 9L, 4L, 1L, 5L, 10L),
             width=c(5L, 6L, 3L, 4L, 3L,  3L))
cvg <- coverage(x)
cvg_views <- slice(cvg, lower=2)
slice(cvg, lower=2, rangesOnly=TRUE)

viewApply(cvg_views, diff)

viewMins(cvg_views)
viewMaxs(cvg_views)

viewSums(cvg_views)
viewMeans(cvg_views)

viewWhichMins(cvg_views)
viewWhichMaxs(cvg_views)

viewRangeMins(cvg_views)
viewRangeMaxs(cvg_views)
}

\keyword{methods}
\keyword{arith}
