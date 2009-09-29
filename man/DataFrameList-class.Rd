\name{DataFrameList-class}
\docType{class}
\alias{DataFrameList-class}
\alias{SimpleDataFrameList-class}
\alias{SplitDataFrameList-class}
\alias{CompressedSplitDataFrameList-class}
\alias{SimpleSplitDataFrameList-class}

% accessors
\alias{nrow,DataFrameList-method}
\alias{ncol,DataFrameList-method}
\alias{dim,DataFrameList-method}
\alias{rownames,DataFrameList-method}
\alias{colnames,DataFrameList-method}
\alias{dimnames,DataFrameList-method}
\alias{rownames<-,CompressedSplitDataFrameList-method}
\alias{rownames<-,SimpleDataFrameList-method}
\alias{colnames<-,CompressedSplitDataFrameList-method}
\alias{colnames<-,SimpleDataFrameList-method}
\alias{dimnames<-,DataFrameList-method}

% constructor
\alias{DataFrameList}
\alias{SplitDataFrameList}

% subsetting
\alias{[,SimpleSplitDataFrameList-method}
\alias{[,CompressedSplitDataFrameList-method}
\alias{[<-,SimpleSplitDataFrameList-method}
\alias{[<-,CompressedSplitDataFrameList-method}

% coercion
\alias{as.data.frame,SplitDataFrameList-method}
\alias{coerce,SplitDataFrameList,DataFrame-method}

% show
\alias{show,SplitDataFrameList-method}

\title{List of DataFrames}
\description{Represents a list of \code{\linkS4class{DataFrame}} objects.
  The \code{SplitDataFrameList} class contains the additional restriction
  that all the columns be of the same name and type. Internally it is stored
  as a list of \code{DataFrame} objects and extends
  \code{\linkS4class{Sequence}}.}

\section{Accessors}{
  In the following code snippets, \code{x} is a \code{DataFrameList}.
  \describe{
    \item{}{\code{dim(x)}: Get the two element integer vector indicating
      the number of rows and columns over the entire dataset.}
    \item{}{\code{dimnames(x)}: Get the list of two character vectors,
      the first holding the rownames (possibly \code{NULL}) and the second
      the column names.
    }
  }
}

\section{Subsetting}{
  In the following code snippets, \code{x} is a \code{SplitDataFrameList}. In
  general \code{x} follows the conventions of 
  \code{SimpleList}/\code{CompressedList} with the following addition:
  \describe{
    \item{}{
      \code{x[i,j,drop]}: If matrix subsetting is used, \code{i} selects
      either the list elements or the rows within the list elements as
      determined by the \code{[} method for
      \code{SimpleList}/\code{CompressedList}, code{j} selects the columns,
      and \code{drop} is used when one column is selected and output can
      be coerced into an \code{AtomicList} or \code{RangesList} subclass.
    }
    \item{}{\code{x[i,j] <- value}: If matrix subsetting is used, \code{i}
      selects either the list elements or the rows within the list elements
      as determined by the \code{[<-} method for
      \code{SimpleList}/\code{CompressedList}, \code{j} selects the columns
      and \code{value} is the replacement value for the selected region.
    }
  }
}

\section{Constructor}{
  \describe{
    \item{}{\code{DataFrameList(...)}: Concatenates the \code{DataFrame}
      objects in \code{...} into a new \code{DataFrameList}.}
    \item{}{\code{SplitDataFrameList(..., compress = TRUE)}:
      The \code{...} arguments can either be a set of \code{DataFrame}
      objects with the same number and names of columns or a set of objects
      (e.g. \code{AtomicList}) that represent split columns that will be
      column bound to form \code{DataFrame} objects. If \code{compress = TRUE},
      returns a \code{CompressedSplitDataFrameList}; else returns a
      \code{SimpleSplitDataFrameList}.}
  }
}

\section{Coercion}{
  In the following code snippets, \code{x} is a \code{SplitDataFrameList}.
  \describe{
    \item{}{\code{as(from, "DataFrame")}: Coerces a
      \code{DataFrameList} to an \code{DataFrame} by combining the
      rows of the elements. This essentially unsplits the \code{DataFrame}.
    }
    \item{}{\code{as.data.frame(x, row.names=NULL, optional=FALSE, ...)}:
      Unsplits the \code{DataFrame} and coerces it to a
      \code{data.frame}, with the rownames specified in
      \code{row.names}. The \code{optional} argument is ignored.
    }
  }
}

\author{ Michael Lawrence }
\seealso{
  \code{\linkS4class{DataFrame}}, \code{\linkS4class{RangedData}},
  which uses a \code{DataFrameList} to split the data by the spaces.
}
\keyword{methods}
\keyword{classes}
