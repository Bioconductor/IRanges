\name{IRangesList-class}
\docType{class}

% IRangesList objects:
\alias{class:IRangesList}
\alias{class:CompressedIRangesList}
\alias{class:SimpleIRangesList}
\alias{IRangesList-class}
\alias{CompressedIRangesList-class}
\alias{SimpleIRangesList-class}
\alias{IRangesList}
\alias{CompressedIRangesList}
\alias{SimpleIRangesList}

% accessors
\alias{end,CompressedIRangesList-method}
\alias{width,CompressedIRangesList-method}
\alias{start,CompressedIRangesList-method}

% subset
\alias{[,SimpleIRangesList-method}
\alias{[,CompressedIRangesList-method}

% coercion
\alias{unlist,IRangesList-method}

% general
\alias{max,CompressedNormalIRangesList-method}
\alias{max,SimpleNormalIRangesList-method}
\alias{min,CompressedNormalIRangesList-method}
\alias{min,SimpleNormalIRangesList-method}
\alias{summary,CompressedIRangesList-method}

% setops
\alias{intersect,CompressedIRangesList,CompressedIRangesList-method}
\alias{setdiff,CompressedIRangesList,CompressedIRangesList-method}
\alias{union,CompressedIRangesList,CompressedIRangesList-method}

% NormalIRangesList objects:
\alias{class:NormalIRangesList}
\alias{class:CompressedNormalIRangesList}
\alias{class:SimpleNormalIRangesList}
\alias{NormalIRangesList-class}
\alias{CompressedNormalIRangesList-class}
\alias{SimpleNormalIRangesList-class}
\alias{NormalIRangesList}
\alias{CompressedNormalIRangesList}
\alias{SimpleNormalIRangesList}

\alias{isNormal,CompressedIRangesList-method}
\alias{isNormal,RangesList-method}
\alias{isNormal,SimpleIRangesList-method}
\alias{whichFirstNotNormal,RangesList-method}

\alias{[[,CompressedNormalIRangesList-method}
\alias{unlist,SimpleNormalIRangesList-method}

% coercion
\alias{coerce,CompressedIRangesList,CompressedNormalIRangesList-method}
\alias{coerce,SimpleIRangesList,SimpleNormalIRangesList-method}
\alias{as.list,CompressedNormalIRangesList-method}


\title{List of IRanges and NormalIRanges}
\description{\code{\linkS4class{IRangesList}} and
  \code{\linkS4class{NormalIRangesList}} objects for storing
  \code{\linkS4class{IRanges}} and \code{\linkS4class{NormalIRanges}} objects
  respectively.}

\section{Constructor}{
  \describe{
    \item{}{\code{IRangesList(..., universe = NULL, compress = TRUE)}:
      The \code{...} argument accepts either a comma-separated list of
      \code{IRanges} objects, or a single \code{LogicalList} / logical
      \code{RleList} object, or 2 elements named \code{start} and \code{end}
      each of them being either a list of integer vectors or an IntegerList
      object.
      When \code{IRanges} objects are supplied, each of them becomes an
      element in the new \code{IRangesList}, in the same order, which is
      analogous to the \code{\link{list}} constructor. If \code{compress},
      the internal storage of the data is compressed.
    }
  }
}

\section{Coercion}{
  \describe{
    \item{}{\code{unlist(x)}: Unlists \code{x}, an \code{IRangesList},
      by concatenating all of the ranges into a single \code{IRanges}
      instance. If the length of \code{x} is zero, an empty \code{IRanges}
      is returned.
    }
  }
}

\section{Methods for NormalIRangesList objects}{
  \describe{
    \item{}{
      \code{max(x)}:
      An integer vector containing the maximum values of each of the elements
      of \code{x}.
    }
    \item{}{
      \code{min(x)}:
      An integer vector containing the minimum values of each of the elements
      of \code{x}.
    }
  }
}

\author{ Michael Lawrence }
\seealso{
  \code{\linkS4class{RangesList}}, the parent of this class, for more
  functionality.
}
\examples{
  range1 <- IRanges(start=c(1,2,3), end=c(5,2,8))
  range2 <- IRanges(start=c(15,45,20,1), end=c(15,100,80,5))
  named <- IRangesList(one = range1, two = range2)
  length(named) # 2
  names(named) # "one" and "two"
  named[[1]] # range1
  unnamed <- IRangesList(range1, range2)
  names(unnamed) # NULL

  x <- IRangesList(start=list(c(1,2,3), c(15,45,20,1)),
                   end=list(c(5,2,8), c(15,100,80,5)))
  as.list(x)
}
\keyword{classes}
\keyword{methods}
