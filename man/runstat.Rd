\name{runstat}
\alias{runstat}

\alias{runsum}
\alias{runwtsum}
\alias{runq}


\title{Fixed width running window summaries across vector-like objects}

\description{
  The \code{runsum}, \code{runwtsum}, \code{runq} functions calculate
  the sum, weighted sum, and order statistics for fixed width running windows.
}

\usage{
  runsum(x, k)
  runwtsum(x, k, wt)
  runq(x, k, i)
}

\arguments{
  \item{x}{
    The data object.
  }
  \item{k}{
    An integer indicating the fixed width of the running window.
  }
  \item{wt}{
    A numeric vector of length \code{k} that provides the weights to use.
  }
  \item{i}{
    An integer indicating which order statistic to calculate.
  }
}

\details{
  The \code{runsum}, \code{runwtq}, and \code{runq} functions
  provide efficient methods for calculating the specified numeric summary
  by performing the looping in compiled code.
}

\value{
  An Rle of length \code{length(x) - k} containing the numeric summaries.
}

\author{P. Aboyoun}

\seealso{
  \link{Rle-class}, \link{RleList-class}
}

\examples{
  x <- Rle(1:10, 1:10)
  runsum(x, k = 3)
  runwtsum(x, k = 3, wt = c(0.25, 0.5, 0.25))
  runq(x, k = 5, i = 3)
}

\keyword{methods}
\keyword{algebra}
\keyword{arith}
