\name{seqapply}
\alias{seqapply}
\alias{mseqapply}
\alias{tseqapply}
\alias{seqsplit}
\alias{seqby}

\title{
Apply function and cast to Vector
}
\description{
  The \code{seqapply} family of functions behaves much like the existing
  \code{lapply} family, except the return value is cast to a
  \linkS4class{Vector} subclass. This facilitates constraining
  computation to the \code{Vector} framework across iteration and
  (for \code{seqsplit}) splitting.
}

\usage{
seqapply(X, FUN, ...)
mseqapply(FUN, ..., MoreArgs = NULL, USE.NAMES = TRUE)
tseqapply(X, INDEX, FUN = NULL, ...)
seqsplit(x, f, drop = FALSE)
seqby(data, INDICES, FUN, ...)
}

\arguments{
  \item{X}{
    The object over which to iterate, usually a vector or \code{Vector}
  }
  \item{x}{
    Like \code{X}
  }
  \item{data}{
    Like \code{X}
  }
  \item{FUN}{
    The function that is applied to each element of \code{X}
  }
  \item{MoreArgs}{
    Additional arguments to \code{FUN} that are treated like scalars
  }
  \item{USE.NAMES}{
    Whether the return values should inherit names from one of the arguments
  }
  \item{INDEX}{
    A \code{list} of factors to split \code{X} into subsets, each of
    which is passed in a separate invocation of \code{FUN}
  }
  \item{INDICES}{
    Like \code{INDEX}, except a single factor need not be in a
    \code{list}.
  }
  \item{f}{
    A \code{factor} or \code{list} of factors
  }
  \item{drop}{
    Whether to drop empty elements from the returned list
  }
  \item{\dots}{
    Extra arguments to pass to \code{FUN}
  }
}

\details{
  These functions should be used just like their base equivalent:
  \describe{
    \item{\code{seqapply}}{=> \code{lapply}}
    \item{\code{mseqapply}}{=> \code{mapply}}
    \item{\code{tseqapply}}{=> \code{tapply}}
    \item{\code{seqsplit}}{=> \code{split}}
    \item{\code{seqby}}{=> \code{by}}
  }
  The only difference is that the result is cast to a \code{Vector}
  object. The casting logic simply looks for a common class from which
  all returned values inherit. It then checks for the existence of a
  function of the form \code{ClassList} where \code{Class} is the name
  of the class. If such a function is not found, the search proceeds up
  the hierarchy of classes. An error is thrown when hierarchy is
  exhausted. If \code{ClassList} is found, it is called with the list of
  return values as its only argument, under the assumption that
  a \code{Vector}-derived instance will be constructed.
}

\value{
  A \code{Vector} object
}

\author{
  Michael Lawrence
}

\examples{
starts <- IntegerList(c(1, 5), c(2, 8))
ends <- IntegerList(c(3, 8), c(5, 9))
rangesList <- mseqapply(IRanges, starts, ends)
rangeDataFrame <- stack(rangesList, "space", "ranges")
dataFrameList <- seqsplit(rangeDataFrame, rangeDataFrame$space)
starts <- seqapply(dataFrameList[,"ranges"], start)
}

\keyword{manip}
