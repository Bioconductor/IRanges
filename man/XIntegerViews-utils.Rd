\name{XIntegerViews-utils}
\alias{XIntegerViews-utils}

\alias{slice}
\alias{slice,integer-method}
\alias{slice,XInteger-method}

\alias{viewMins}
\alias{viewMins,XIntegerViews-method}
\alias{viewMaxs}
\alias{viewMaxs,XIntegerViews-method}
\alias{viewSums}
\alias{viewSums,XIntegerViews-method}


\title{XIntegerViews utility functions and numeric summary of views}

\description{
  A \code{slice} function for creating an XIntegerViews object.

  A set of functions to calculate mins, maxs, and sums on views.
}

\usage{
  slice(x, lower=-Inf, upper=Inf, includeLower=TRUE, includeUpper=TRUE)

  viewMins(x, na.rm=FALSE)
  viewMaxs(x, na.rm=FALSE)
  viewSums(x, na.rm=FALSE)
}

\arguments{
  \item{x}{
    An \link{XInteger} object or an integer vector for \code{slice}.

    An \link{XIntegerViews} object for \code{viewMins}, \code{viewMaxs} and \code{viewSums}.
  }
  \item{lower, upper}{
    The lower and upper bounds for the slice.
  }
  \item{includeLower, includeUpper}{
    Logical values that indicate whether or not to include the equality in the
    corresponding bound for the slice.
  }
  \item{na.rm}{
    Logical indicating whether or not to include missing values in the results.
  }
}

\details{
  [TODO: some details about \code{slice}]

  Fast looping over views to create numeric summaries.
}

\value{
  An \link{XIntegerViews} object for \code{slice}.

  An integer vector of \code{length(x)} containing the numeric summaries for
  the views.
}

\author{P. Aboyoun}

\seealso{
  \link{XIntegerViews-class}
}

\examples{
  ## Views derived by slicing
  vec <- as.integer(c(19, 5, 0, 8, 5))
  slice(vec, lower=5, upper=8)

  set.seed(0)
  vec <- sample(24)
  vecViews <- slice(vec, lower=4, upper=16)
  vecViews
  viewMins(vecViews)
  viewMaxs(vecViews)
  viewSums(vecViews)
}

\keyword{methods}
\keyword{algebra}
\keyword{arith}
