\name{RangedData-utils}
\alias{RangedData-utils}
\alias{range,RangedData-method}

\title{RangedData utility functions}

\description{
  Utility functions for manipulating \code{\linkS4class{RangedData}} objects.
}

\usage{
\S4method{range}{RangedData}(x, ..., na.rm = FALSE)
}

\arguments{
  \item{x}{A \code{RangedData} object}
  \item{...}{Additional \code{RangedData} objects}
  \item{na.rm}{Ignored}
}

\value{
  \code{range} returns a \code{RangesList} resulting from calling
  \code{\link[=range,RangesList-method]{range}(ranges(x))}, i.e. the
  bounds of the ranges in each space.
}

\author{Michael Lawrence}

\seealso{
  \link{RangedData}, \link{DataTable-class}, \link{DataTable-eval}
}

\examples{
  ranges <- IRanges(c(1,2,3),c(4,5,6))
  score <- c(10L, 2L, NA)
  rd <- RangedData(ranges, score)

  range(rd)
  rd2 <- RangedData(IRanges(c(5,2,0), c(6,3,1)))
  range(rd, rd2)
}

\keyword{utilities}
