\name{runstat}
\alias{runstat}

\alias{runsum}
\alias{runmean}
\alias{runwtsum}
\alias{runq}


\title{Fixed width running window summaries across vector-like objects}

\description{
  The \code{runsum}, \code{runmean}, \code{runwtsum}, \code{runq} functions
  calculate the sum, mean, weighted sum, and order statistics for fixed width
  running windows.
}

\usage{
runsum(x, k, endrule = c("drop", "constant"))
runmean(x, k, endrule = c("drop", "constant"))
runwtsum(x, k, wt, endrule = c("drop", "constant"))
runq(x, k, i, endrule = c("drop", "constant"))
}

\arguments{
  \item{x}{
    The data object.
  }
  \item{k}{
    An integer indicating the fixed width of the running window. Must be
    odd when \code{endrule == "constant"}.
  }
  \item{wt}{
    A numeric vector of length \code{k} that provides the weights to use.
  }
  \item{i}{
    An integer indicating which order statistic to calculate.
  }
  \item{endrule}{
    A character string indicating how the values at the beginning
    and the end (of the data) should be treated.
    \describe{
      \item{\code{"drop"}}{do not extend the running statistics to
        be the same length as the underlying vectors;}
      \item{\code{"constant"}}{copies running statistic to the
        first values and analogously for the last ones making the
        smoothed ends \emph{constant};}
    }
  }
}

\details{
  The \code{runsum}, \code{runmean}, \code{runwtsum}, and \code{runq}
  functions provide efficient methods for calculating the specified
  numeric summary by performing the looping in compiled code.
}

\value{
  An object of the same class as \code{x}.
}

\author{P. Aboyoun}

\seealso{
  \code{\link[stats]{runmed}}, \link{Rle-class}, \link{RleList-class}
}

\examples{
  x <- Rle(1:10, 1:10)
  runsum(x, k = 3)
  runsum(x, k = 3, endrule = "constant")
  runmean(x, k = 3)
  runwtsum(x, k = 3, wt = c(0.25, 0.5, 0.25))
  runq(x, k = 5, i = 3, endrule = "constant")
}

\keyword{methods}
\keyword{algebra}
\keyword{arith}
