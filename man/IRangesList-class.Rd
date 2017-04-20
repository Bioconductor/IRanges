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

% coercion
\alias{coerce,list,CompressedIRangesList-method}
\alias{coerce,list,SimpleIRangesList-method}
\alias{coerce,list,IRangesList-method}

\alias{coerce,List,CompressedIRangesList-method}
\alias{coerce,Ranges,CompressedIRangesList-method}
\alias{coerce,List,SimpleIRangesList-method}
\alias{coerce,RangesList,SimpleIRangesList-method}
\alias{coerce,SimpleRangesList,SimpleIRangesList-method}
\alias{coerce,List,IRangesList-method}

\alias{coerce,CompressedRleList,CompressedIRangesList-method}

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

\alias{isNormal,SimpleIRangesList-method}
\alias{isNormal,CompressedIRangesList-method}

% general
\alias{max,CompressedNormalIRangesList-method}
\alias{max,SimpleNormalIRangesList-method}
\alias{min,CompressedNormalIRangesList-method}
\alias{min,SimpleNormalIRangesList-method}
\alias{summary,CompressedIRangesList-method}

% more coercions
\alias{as.list,CompressedNormalIRangesList-method}
\alias{unlist,SimpleNormalIRangesList-method}

\alias{coerce,RangesList,SimpleNormalIRangesList-method}
\alias{coerce,SimpleIRangesList,SimpleNormalIRangesList-method}
\alias{coerce,NormalIRangesList,CompressedNormalIRangesList-method}
\alias{coerce,CompressedIRangesList,CompressedNormalIRangesList-method}
\alias{coerce,RangesList,CompressedNormalIRangesList-method}
\alias{coerce,RangesList,NormalIRangesList-method}

\alias{coerce,LogicalList,NormalIRangesList-method}
\alias{coerce,LogicalList,CompressedNormalIRangesList-method}
\alias{coerce,LogicalList,SimpleNormalIRangesList-method}
\alias{coerce,RleList,NormalIRangesList-method}
\alias{coerce,RleList,CompressedNormalIRangesList-method}
\alias{coerce,RleList,SimpleNormalIRangesList-method}

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
  In the code snippets below, \code{from} is a \emph{list-like} object.
  \describe{
    \item{}{\code{as(from, "SimpleIRangesList")}: Coerces \code{from},
      to a \code{\linkS4class{SimpleIRangesList}}, requiring
      that all \code{Ranges} elements are coerced to internal
      \code{IRanges} elements. This is a convenient way to ensure that all
      \code{Ranges} have been imported into R (and that there is no
      unwanted overhead when accessing them).
    }
    \item{}{\code{as(from, "CompressedIRangesList")}: Coerces \code{from},
      to a \code{\linkS4class{CompressedIRangesList}}, requiring
      that all \code{Ranges} elements are coerced to internal
      \code{IRanges} elements. This is a convenient way to ensure that all
      \code{Ranges} have been imported into R (and that there is no
      unwanted overhead when accessing them).
    }
    \item{}{\code{as(from, "SimpleNormalIRangesList")}: Coerces \code{from},
      to a \code{\linkS4class{SimpleNormalIRangesList}}, requiring
      that all \code{Ranges} elements are coerced to internal
      \code{NormalIRanges} elements.
    }
    \item{}{\code{as(from, "CompressedNormalIRangesList")}: Coerces
      \code{from}, to a \code{\linkS4class{CompressedNormalIRangesList}},
      requiring that all \code{Ranges} elements are coerced to internal
      \code{NormalIRanges} elements.
    }
  }

  In the code snippet below, \code{x} is an \code{IRangesList} object.
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

  \link{intra-range-methods} and \link{inter-range-methods} for
  intra and inter range transformations of IRangesList objects.

  \link{setops-methods} for set operations on IRangesList objects.
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
