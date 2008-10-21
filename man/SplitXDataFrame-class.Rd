\name{SplitXDataFrame-class}
\docType{class}
\alias{SplitXDataFrame-class}

% accessors
\alias{dim,SplitXDataFrame-method}
\alias{dimnames,SplitXDataFrame-method}

% constructor
\alias{SplitXDataFrame}

% coercion
\alias{as.data.frame,SplitXDataFrame-method}
\alias{unlist,SplitXDataFrame-method}
\alias{coerce,SplitXDataFrame,XDataFrame-method}

\title{Split XDataFrame}
\description{Represents an \code{\linkS4class{XDataFrame}} split along
  some factor. Internally a list of \code{XDataFrame} instances and
  extends \code{\linkS4class{TypedList}}. Asserts all elements have the
  same number and names of columns.
}
\note{
  The \code{\linkS4class{RangedData}} drove the development of this
  class. It is not clear if it is of general use and might disappear.
}

\section{Accessors}{
  In the following code snippets, \code{x} is a \code{SplitXDataFrame}.
  \describe{
    \item{}{\code{dim(x)}: Get the two element integer vector indicating
      the number of rows and columns over the entire dataset.}
    \item{}{\code{dimnames(x)}: Get the list of two character vectors,
      the first holding the rownames (possibly \code{NULL}) and the second
      the column names.
    }
  }
}

\section{Constructor}{
  \describe{
    \item{}{\code{SplitXDataFrame(...)}: Concatenates the
      \code{XDataFrame} instances in \code{...} into a new
      \code{SplitXDataFrame}. Note that all arguments should have the same
      number and names of columns.}
  }
}

\section{Coercion}{
  In the following code snippets, \code{x} is a \code{SplitXDataFrame}.
  \describe{
    \item{}{\code{as(from, "XDataFrame")}: Coerces a
      \code{SplitXDataFrame} to an \code{XDataFrame} by combining the
      rows of the elements. This essentially unsplits the \code{XDataFrame}.
    }
    \item{}{\code{unlist(x, recursive = TRUE, use.names = TRUE)}:
      Same as above, except specifying \code{use.names} to \code{FALSE}
      drops the row names. \code{recursive} is ignored.
    }
    \item{}{\code{as.data.frame(x, row.names=NULL, optional=FALSE, ...)}:
      Unsplits the \code{XDataFrame} and coerces it to a
      \code{data.frame}, with the rownames specified in
      \code{row.names}. The \code{optional} argument is ignored.
    }
  }
}

\author{ Michael Lawrence }
\seealso{
  \code{\linkS4class{XDataFrame}}, \code{\linkS4class{RangedData}},
  which uses a \code{SplitXDataFrame} to split the data by the spaces.
}
\keyword{methods}
\keyword{classes}
