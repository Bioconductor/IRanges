\name{RangesList-utils}
\alias{RangesList-utils}
% utilities
\alias{reduce,RangesList-method}
\alias{gaps,RangesList-method}

\title{RangesList utility functions}

\description{
  Utility functions for manipulating \code{\linkS4class{RangesList}}
  objects.
}

\usage{
\S4method{gaps}{RangesList}(x, start=NA, end=NA)
\S4method{reduce}{RangesList}(x, with.inframe.attrib = FALSE)
}

\arguments{
  \item{x}{A \code{RangesList}}
  \item{start}{The start of the range over which to calculate the
    gaps. If \code{NA}, use the minimum start position in the
    \code{Ranges} instance.}
  \item{end}{The end of the range over which to calculate the gaps. If
    \code{NA}, use the maximum end position in the \code{Ranges}
    instance.}
  \item{with.inframe.attrib}{Ignored.}
}

\details{
  The \code{gaps} function takes the complement (the \code{\link{gaps}})
  of each element in the list and returns the result as a
  \code{RangesList}.

  The \code{reduce} method merges (via \code{\link{reduce}}) all of the
  elements into a single \code{Ranges} instance and returns the result
  as a length-one \code{RangesList}.
}

\value{
  A \code{RangesList} instance. For \code{gaps}, length is the same as
  that of \code{x}. For \code{reduce}, length is one.
}

\author{Michael Lawrence, H. Pages}

\seealso{
  \code{\linkS4class{RangesList}}, \code{\link{reduce}}, \code{\link{gaps}}
}

\examples{
  # 'gaps'
  range1 <- IRanges(start=c(1,2,3), end=c(5,2,8))
  range2 <- IRanges(start=c(15,45,20,1), end=c(15,100,80,5))
  collection <- RangesList(one = range1, range2)

  # these two are the same
  RangesList(gaps(range1), gaps(range2))
  gaps(collection)

  # 'reduce'
  range2 <- IRanges(start=c(45,20,1), end=c(100,80,5))
  collection <- RangesList(one = range1, range2)

  # and these two are the same
  reduce(collection)
  RangesList(asNormalIRanges(IRanges(c(1,20), c(8, 100)), force=FALSE))
}

\keyword{utilities}
