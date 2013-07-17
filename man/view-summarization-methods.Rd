\name{view-summarization-methods}

\alias{view-summarization-methods}

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
  \code{viewApply} applies a function on each view of a \link{Views} or
  \link{ViewsList} object.

  \code{viewMins}, \code{viewMaxs}, \code{viewSums}, \code{viewMeans}
  calculate respectively the minima, maxima, sums, and means of the views
  in a \link{Views} or \link{ViewsList} object.
}

\usage{
viewApply(X, FUN, ..., simplify = TRUE)

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
  \item{...}{
    Additional arguments to be passed on.
  }
  \item{simplify}{
    A logical value specifying whether or not the result should be simplified
    to a vector or matrix if possible.
  }
  \item{x}{
    An \link{RleViews} or \link{RleViewsList} object.
  }
  \item{na.rm}{
    Logical indicating whether or not to include missing values in the results.
  }
}

\details{
  The \code{viewMins}, \code{viewMaxs}, \code{viewSums}, and \code{viewMeans}
  functions provide efficient methods for calculating the specified numeric
  summary by performing the looping in compiled code.

  The \code{viewWhichMins}, \code{viewWhichMaxs}, \code{viewRangeMins}, and
  \code{viewRangeMaxs} functions provide efficient methods for finding the
  locations of the minima and maxima.
}

\value{
  For all the functions in this man page (except \code{viewRangeMins} and
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
  \itemize{
    \item The \code{\link{slice}} function for slicing an \link{Rle} or
          \link{RleList} object.

    \item \link[XVector]{view-summarization-methods} in the \pkg{XVector}
          package for more view summarization methods.

    \item The \link{RleViews} and \link{RleViewsList} classes.

    \item The \code{\link{which.min}} and \code{\link{colSums}} functions.
  }
}

\examples{
## Views derived from coverage
x <- IRanges(start=c(1L, 9L, 4L, 1L, 5L, 10L),
             width=c(5L, 6L, 3L, 4L, 3L,  3L))
cvg <- coverage(x)
cvg_views <- slice(cvg, lower=2)

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
