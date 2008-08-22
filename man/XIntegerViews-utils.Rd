\name{XIntegerViews-utils}

\alias{XIntegerViews-utils}
\alias{viewMins}
\alias{viewMins,XIntegerViews-method}
\alias{viewMaxs}
\alias{viewMaxs,XIntegerViews-method}
\alias{viewSums}
\alias{viewSums,XIntegerViews-method}


\title{Numeric summary of views}

\description{
  A set of functions to calculate mins, maxs, and sums on views.
}

\usage{
  viewMins(x, na.rm=FALSE)
  viewMaxs(x, na.rm=FALSE)
  viewSums(x, na.rm=FALSE)
}

\arguments{
  \item{x}{
    An \link{XIntegerViews} object.
  }
  \item{na.rm}{
    Logical indicating whether or not to include missing values in the results.
  }
}

\details{
  Fast looping over views to create numeric summaries.
}

\value{
  An integer vector of \code{length(x)} containing the numeric summaries for
  the views.
}

\seealso{
  \link{XIntegerViews-class}
}

\examples{
  set.seed(0)
  vec <- sample(24)
  vecViews <- slice(vec, lower = 4, upper = 16)
  vecViews
  viewMins(vecViews)
  viewMaxs(vecViews)
  viewSums(vecViews)
}

\keyword{methods}
\keyword{algebra}
\keyword{arith}
