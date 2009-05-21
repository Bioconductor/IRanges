\name{XDataFrameList-class}
\docType{class}
\alias{XDataFrameList-class}
\alias{SimpleXDataFrameList-class}
\alias{SplitXDataFrameList-class}
\alias{CompressedSplitXDataFrameList-class}
\alias{SimpleSplitXDataFrameList-class}

% accessors
\alias{dim,SimpleXDataFrameList-method}
\alias{dim,CompressedSplitXDataFrameList-method}
\alias{dimnames,SimpleXDataFrameList-method}
\alias{dimnames,CompressedSplitXDataFrameList-method}

% constructor
\alias{XDataFrameList}
\alias{SplitXDataFrameList}

% coercion
\alias{as.data.frame,SplitXDataFrameList-method}
\alias{coerce,SplitXDataFrameList,XDataFrame-method}

\title{List of XDataFrames}
\description{Represents a list of \code{\linkS4class{XDataFrame}} objects.
  The \code{SplitXDataFrameList} class contains the additional restriction
  that all the columns be of the same name and type. Internally it is stored
  as a list of \code{XDataFrame} objects and extends
  \code{\linkS4class{Sequence}}.}

\section{Accessors}{
  In the following code snippets, \code{x} is a \code{XDataFrameList}.
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
    \item{}{\code{XDataFrameList(..., compress = FALSE)}:
      Concatenates the \code{XDataFrame}
      objects in \code{...} into a new \code{XDataFrameList}, optionally
      compressing them, which is only possible if the elements can be
      combined via \code{rbind}, see \code{\linkS4class{Sequence}}.}
    \item{}{\code{SplitXDataFrameList(..., compress = FALSE)}: Concatenates the
      \code{XDataFrame} objects in \code{...} into a new
      \code{SplitXDataFrameList}. Note that all arguments must have the same
      number and names of columns.}
  }
}

\section{Coercion}{
  In the following code snippets, \code{x} is a \code{SplitXDataFrameList}.
  \describe{
    \item{}{\code{as(from, "XDataFrame")}: Coerces a
      \code{XDataFrameList} to an \code{XDataFrame} by combining the
      rows of the elements. This essentially unsplits the \code{XDataFrame}.
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
  which uses a \code{XDataFrameList} to split the data by the spaces.
}
\keyword{methods}
\keyword{classes}
