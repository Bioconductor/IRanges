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
\alias{ncol,CompressedSplitDataFrameList-method}
\alias{ncol,SimpleSplitDataFrameList-method}
\alias{dim,DataFrameList-method}
\alias{rownames,DataFrameList-method}
\alias{colnames,DataFrameList-method}
\alias{colnames,CompressedSplitDataFrameList-method}
\alias{colnames,SimpleSplitDataFrameList-method}
\alias{dimnames,DataFrameList-method}
\alias{rownames<-,CompressedSplitDataFrameList-method}
\alias{rownames<-,SimpleDataFrameList-method}
\alias{colnames<-,CompressedSplitDataFrameList-method}
\alias{colnames<-,SimpleDataFrameList-method}
\alias{dimnames<-,DataFrameList-method}
\alias{columnMetadata}
\alias{columnMetadata<-}
\alias{columnMetadata,SimpleSplitDataFrameList-method}
\alias{columnMetadata<-,SimpleSplitDataFrameList-method}
\alias{columnMetadata,CompressedSplitDataFrameList-method}
\alias{columnMetadata<-,CompressedSplitDataFrameList-method}


% constructor
\alias{DataFrameList}
\alias{SplitDataFrameList}

% subsetting
\alias{[,SimpleSplitDataFrameList-method}
\alias{[,CompressedSplitDataFrameList-method}
\alias{[<-,SimpleSplitDataFrameList-method}
\alias{[<-,CompressedSplitDataFrameList-method}

% coercion
\alias{as.data.frame,DataFrameList-method}
\alias{coerce,DataFrameList,DataFrame-method}
\alias{coerce,SplitDataFrameList,DataFrame-method}
\alias{coerce,ANY,CompressedSplitDataFrameList-method}
\alias{coerce,ANY,SimpleSplitDataFrameList-method}
\alias{coerce,List,CompressedSplitDataFrameList-method}
\alias{coerce,List,SimpleSplitDataFrameList-method}
\alias{stack,DataFrameList-method}

% splitting and combining
\alias{cbind,DataFrameList-method}
\alias{rbind,DataFrameList-method}

% show
\alias{show,SplitDataFrameList-method}

\title{List of DataFrames}
\description{Represents a list of \code{\linkS4class{DataFrame}} objects.
  The \code{SplitDataFrameList} class contains the additional restriction
  that all the columns be of the same name and type. Internally it is stored
  as a list of \code{DataFrame} objects and extends
  \code{\linkS4class{List}}.}

\section{Accessors}{
  In the following code snippets, \code{x} is a \code{DataFrameList}.
  \describe{
    \item{}{\code{dim(x)}: Get the two element integer vector indicating
      the number of rows and columns over the entire dataset.}
    \item{}{\code{dimnames(x)}: Get the list of two character vectors,
      the first holding the rownames (possibly \code{NULL}) and the second
      the column names.
    }
    \item{}{\code{columnMetadata(x)}: Get the \code{DataFrame} of
      metadata along the columns, i.e., where each column in \code{x} is
      represented by a row in the metadata. The metadata is common
      across all elements of \code{x}. Note that
      calling \code{mcols(x)} returns the metadata on
      the \code{DataFrame} elements of \code{x}.
    }
    \item{}{\code{columnMetadata(x) <- value}: Set the \code{DataFrame}
      of metadata for the columns.
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
      \code{SimpleList}/\code{CompressedList}, \code{j} selects the columns,
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
      \item{}{\code{SplitDataFrameList(..., compress = TRUE, cbindArgs =
      FALSE)}: If \code{cbindArgs} is \code{FALSE}, the \code{...}
      arguments are coerced to \code{DataFrame} objects and concatenated
      to form the result. The arguments must have the same number and
      names of columns. If \code{cbindArgs} is \code{TRUE}, the
      arguments are combined as columns. The arguments must then be the
      same length, with each element of an argument mapping to an
      element in the result. If \code{compress = TRUE}, returns a
      \code{CompressedSplitDataFrameList}; else returns a
      \code{SimpleSplitDataFrameList}.}
  }
}

\section{Combining}{
  In the following code snippets, objects in \code{...} are of class
  \code{DataFrameList}.

  \describe{
    \item{}{
      \code{rbind(...)}: Creates a new \code{DataFrameList} containing
      the element-by-element row concatenation of the objects in \code{...}.
    }
    \item{}{
      \code{cbind(...)}: Creates a new \code{DataFrameList} containing
      the element-by-element column concatenation of the objects in \code{...}.
    }
  }
}

\section{Coercion}{
  In the following code snippets, \code{x} is a \code{DataFrameList}.
  \describe{
    \item{}{\code{as(from, "DataFrame")}: Coerces a
      \code{DataFrameList} to an \code{DataFrame} by combining the
      rows of the elements. This essentially unsplits the
      \code{DataFrame}. Every element of \code{x} must have the same columns.
    }
    \item{}{\code{as(from, "SplitDataFrameList")}: By default, simply
      calls the \code{SplitDataFrameList} constructor
      on \code{from}. If \code{from} is a \code{List}, each element
      of \code{from} is passed as an argument
      to \code{SplitDataFrameList}, like calling \code{as.list} on a vector.}
    \item{}{\code{as.data.frame(x, row.names=NULL, optional=FALSE, ...)}:
      Unsplits the \code{DataFrame} and coerces it to a
      \code{data.frame}, with the rownames specified in
      \code{row.names}. The \code{optional} argument is ignored.
    }
    \item{}{\code{stack(x, index.var = "name")}: Unlists \code{x} and
      adds a column named \code{index.var} to the result, indicating the
      element of \code{x} from which each row was obtained.
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
