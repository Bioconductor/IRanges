\name{IRangesList-class}
\docType{class}
\alias{IRangesList-class}
\alias{CompressedIRangesList-class}
\alias{SimpleIRangesList-class}

% coercion
\alias{coerce,IRangesList,NormalIRanges-method}
\alias{coerce,IRangesList,list-method} % undocumented
\alias{unlist,IRangesList-method}

\alias{summary,CompressedIRangesList-method}

% constructor
\alias{IRangesList}

\title{List of IRanges}
\description{A \code{\linkS4class{RangesList}} that only stores
  \code{\linkS4class{IRanges}} objects.}

\section{Constructor}{
  \describe{
    \item{}{\code{IRangesList(..., compress = TRUE)}: The \code{...} argument
      accepts either a comma-separated list of \code{IRanges} objects or
      a single \code{LogicalList} / logical \code{RleList} object. When
      the former is supplied, each \code{IRanges} in \code{...} becomes an
      element in the new \code{IRangesList}, in the same order, which is
      analogous to the \code{\link{list}} constructor. If \code{compress},
      the internal storage of the data is compressed.
    }
  }
}

\section{Coercion}{
  \describe{
    \item{}{\code{as(from, "NormalIRanges")}: Merges each of the
      elements into a single \code{\linkS4class{NormalIRanges}} through
      \code{\link{reduce}}.
    }
    \item{}{\code{unlist(x)}: Unlists \code{x}, an \code{IRangesList},
      by concatenating all of the ranges into a single \code{IRanges}
      instance. If the length of \code{x} is zero, an empty \code{IRanges}
      is returned.
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
}
\keyword{classes}
\keyword{methods}
