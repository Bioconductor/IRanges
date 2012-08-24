\name{RangesList-utils}
\alias{RangesList-utils}

% "merge" and "range"
\alias{merge,RangesList,missing-method}
\alias{merge,missing,RangesList-method}
\alias{merge,RangesList,RangesList-method}

% set operations
\alias{union,RangesList,RangesList-method}
\alias{intersect,RangesList,RangesList-method}
\alias{setdiff,RangesList,RangesList-method}

\title{RangesList utility functions}

\description{
  Utility functions for manipulating \code{\linkS4class{RangesList}}
  objects.
}

\usage{
# Set operations
\S4method{union}{RangesList,RangesList}(x, y)
\S4method{intersect}{RangesList,RangesList}(x, y)
\S4method{setdiff}{RangesList,RangesList}(x, y)
}

\arguments{
  \item{x, y}{A \code{RangesList}}
}

\details{
  The \code{union} method performs elementwise \code{union} operations
  for two \code{RangesList} objects.

  The \code{intersect} method performs elementwise \code{intersect}
  operations for two \code{RangesList} objects.

  The \code{setdiff} method performs elementwise \code{setdiff} operations
  for two \code{RangesList} objects.
}

\author{M. Lawrence, H. Pages, P. Aboyoun}

\seealso{
  \link{intra-range-methods},
  \link{inter-range-methods}, 
  \linkS4class{RangesList}
}

\examples{
  irl1 <- IRangesList(a = IRanges(c(1,2),c(4,3)), b = IRanges(c(4,6),c(10,7)))
  irl2 <- IRangesList(c = IRanges(c(0,2),c(4,5)), a = IRanges(c(4,5),c(6,7)))

  # set operations
  union(irl1, irl2)
  intersect(irl1, irl2)
  setdiff(irl1, irl2)
}

\keyword{utilities}
