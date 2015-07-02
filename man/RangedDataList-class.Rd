\name{RangedDataList-class}
\docType{class}
\alias{RangedDataList-class}
\alias{unlist,RangedDataList-method}
\alias{stack,RangedDataList-method}

% Constructor
\alias{RangedDataList}

\title{Lists of RangedData}
\description{
  IMPORTANT NOTE: Starting with BioC 2.12, the use of \code{RangedData}
  and \code{RangedDataList} objects is discouraged in favor of
  \code{\link[GenomicRanges]{GRanges}} and
  \code{\link[GenomicRanges]{GRangesList}} objects (those classes
  are defined in the \pkg{GenomicRanges} package).

  A formal list of \code{\linkS4class{RangedData}}
  objects. Extends and inherits all its methods from
  \code{\linkS4class{List}}. One use case is to group together
  all of the samples from an experiment generating data on ranges.
}

\section{Constructor}{
  \describe{
    \item{}{\code{RangedDataList(...)}: Concatenates the
      \code{RangedData} objects in \code{...} into a new \code{RangedDataList}.
    }
  }
}

\section{Other methods}{
  \describe{
    \item{}{\code{stack(x, index.var = "name")}: Concantenates the elements
      of \code{x} into a \code{RangedData}, with a column
      named by \code{index.var} that groups the records by their original
      element in \code{x}.
    }
  }
}

\author{Michael Lawrence}

\seealso{
  \code{\linkS4class{RangedData}}, the element type of this
  \code{\linkS4class{List}}.
}
\examples{
  ranges <- IRanges(c(1,2,3),c(4,5,6))
  a <- RangedData(IRanges(c(1,2,3),c(4,5,6)), score = c(10L, 2L, NA))
  b <- RangedData(IRanges(c(1,2,4),c(4,7,5)), score = c(3L, 5L, 7L))
  RangedDataList(sample1 = a, sample2 = b)
}
\keyword{classes}
