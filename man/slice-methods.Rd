\name{slice-methods}

\alias{slice-methods}

\alias{slice}
\alias{slice,ANY-method}
\alias{slice,Rle-method}
\alias{slice,RleList-method}


\title{Slice a vector-like or list-like object}

\description{
  \code{slice} is a generic function that creates views on a vector-like
  or list-like object that contain the elements that are within the
  specified bounds.
}

\usage{
slice(x, lower=-Inf, upper=Inf, ...)

\S4method{slice}{Rle}(x, lower=-Inf, upper=Inf,
      includeLower=TRUE, includeUpper=TRUE, rangesOnly=FALSE)

\S4method{slice}{RleList}(x, lower=-Inf, upper=Inf,
      includeLower=TRUE, includeUpper=TRUE, rangesOnly=FALSE)
}

\arguments{
  \item{x}{
    An \link{Rle} or \link{RleList} object, or any object coercible to
    an Rle object.
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
  \item{...}{
    Additional arguments to be passed to specific methods.
  }
}

\details{
  \code{slice} is useful for finding areas of absolute maxima (peaks),
  absolute minima (troughs), or fluctuations within specified limits.
  One or more view summarization methods can be used on the result of
  \code{slice}. See \code{?`link{view-summarization-methods}`}
}

\value{
  The method for \link{Rle} objects returns an \link{RleViews} object
  if \code{rangesOnly=FALSE} or an \link{IRanges} object if
  \code{rangesOnly=TRUE}.

  The method for \link{RleList} objects returns an \link{RleViewsList} object
  if \code{rangesOnly=FALSE} or an \link{IRangesList} object if
  \code{rangesOnly=TRUE}.
}

\author{P. Aboyoun}

\seealso{
  \itemize{
    \item \link{view-summarization-methods} for summarizing the views
          returned by \code{slice}.

    \item \link[XVector]{slice-methods} in the \pkg{XVector} package
          for more \code{slice} methods.

    \item \code{\link{coverage}} for computing the coverage across a set
          of ranges.

    \item The \link{Rle}, \link{RleList}, \link{RleViews}, and
          \link{RleViewsList} classes.
  }
}

\examples{
## Views derived from coverage
x <- IRanges(start=c(1L, 9L, 4L, 1L, 5L, 10L),
             width=c(5L, 6L, 3L, 4L, 3L,  3L))
cvg <- coverage(x)
slice(cvg, lower=2)
slice(cvg, lower=2, rangesOnly=TRUE)
}

\keyword{methods}
