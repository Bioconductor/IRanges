\name{rollStat}
\alias{rollStat}

\alias{rollSum}
\alias{rollWeightedSum}
\alias{rollQ}


\title{Fixed width rolling window summaries across vector-like objects}

\description{
  The \code{rollSum}, \code{rollWeightedSum}, \code{rollW} functions calculate
  the sum, weighted sum, and order statistics for fixed width rolling windows.
}

\usage{
  rollSum(x, width)
  rollWeightedSum(x, width, weight)
  rollQ(x, width, which)
}

\arguments{
  \item{x}{
    The data object.
  }
  \item{width}{
    An integer indicating the fixed width of the rolling window.
  }
  \item{weight}{
    A numeric vector of length \code{width} that provides the weights to use.
  }
  \item{which}{
    An integer indicating which order statistic to calculate.
  }
}

\details{
  The \code{rollSum}, \code{rollWeigthedSum}, and \code{rollQ} functions
  provide efficient methods for calculating the specified numeric summary
  by performing the looping in compiled code.
}

\value{
  An Rle of length \code{length(x) - width} containing the numeric summaries.
}

\author{P. Aboyoun}

\seealso{
  \link{Rle-class}, \link{RleList-class}
}

\examples{
  x <- Rle(1:10, 1:10)
  rollSum(x, width = 3)
  rollWeightedSum(x, width = 3, weight = c(0.25, 0.5, 0.25))
  rollQ(x, width = 5, which = 3)
}

\keyword{methods}
\keyword{algebra}
\keyword{arith}
