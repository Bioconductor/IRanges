\name{DataFrameList-class}
\docType{class}

\alias{class:DataFrameList}
\alias{DataFrameList-class}
\alias{DataFrameList}

\alias{class:DFrameList}
\alias{DFrameList-class}
\alias{DFrameList}

\alias{class:SimpleDataFrameList}
\alias{SimpleDataFrameList-class}
\alias{SimpleDataFrameList}

\alias{class:SimpleDFrameList}
\alias{SimpleDFrameList-class}
\alias{SimpleDFrameList}

\alias{class:SplitDataFrameList}
\alias{SplitDataFrameList-class}
\alias{SplitDataFrameList}

\alias{class:SplitDFrameList}
\alias{SplitDFrameList-class}
\alias{SplitDFrameList}

\alias{class:SimpleSplitDataFrameList}
\alias{SimpleSplitDataFrameList-class}
\alias{SimpleSplitDataFrameList}

\alias{class:SimpleSplitDFrameList}
\alias{SimpleSplitDFrameList-class}
\alias{SimpleSplitDFrameList}

\alias{class:CompressedDataFrameList}
\alias{CompressedDataFrameList-class}
\alias{CompressedDataFrameList}

\alias{class:CompressedDFrameList}
\alias{CompressedDFrameList-class}
\alias{CompressedDFrameList}

\alias{class:CompressedSplitDataFrameList}
\alias{CompressedSplitDataFrameList-class}
\alias{CompressedSplitDataFrameList}

\alias{class:CompressedSplitDFrameList}
\alias{CompressedSplitDFrameList-class}
\alias{CompressedSplitDFrameList}

% accessors
\alias{nrow,DataFrameList-method}
\alias{ncol,DataFrameList-method}
\alias{ncol,CompressedSplitDataFrameList-method}
\alias{ncol,SimpleSplitDataFrameList-method}
\alias{dim,DataFrameList-method}
\alias{rownames,DataFrameList-method}
\alias{colnames,DataFrameList-method}
\alias{colnames,SplitDataFrameList-method}
\alias{colnames,CompressedSplitDataFrameList-method}
\alias{dimnames,DataFrameList-method}
\alias{rownames<-,CompressedSplitDataFrameList-method}
\alias{rownames<-,SimpleDataFrameList-method}
\alias{colnames<-,CompressedSplitDataFrameList-method}
\alias{colnames<-,SimpleDataFrameList-method}
\alias{dimnames<-,DataFrameList-method}
\alias{NROW,DataFrameList-method}
\alias{ROWNAMES,DataFrameList-method}
\alias{columnMetadata}
\alias{columnMetadata<-}
\alias{columnMetadata,SimpleSplitDataFrameList-method}
\alias{columnMetadata<-,SimpleSplitDataFrameList-method}
\alias{columnMetadata,CompressedSplitDataFrameList-method}
\alias{columnMetadata<-,CompressedSplitDataFrameList-method}
\alias{commonColnames}
\alias{commonColnames<-}
\alias{commonColnames,SimpleSplitDataFrameList-method}
\alias{commonColnames<-,SimpleSplitDataFrameList-method}
\alias{commonColnames,CompressedSplitDataFrameList-method}
\alias{commonColnames<-,CompressedSplitDataFrameList-method}

% subsetting
\alias{[,SimpleSplitDataFrameList-method}
\alias{[,CompressedSplitDataFrameList-method}
\alias{[<-,SplitDataFrameList-method}

% transformation
\alias{transform,SplitDataFrameList-method}

% coercion
\alias{coerce,ANY,DataFrameList-method}
\alias{coerce,ANY,SimpleDataFrameList-method}
\alias{coerce,ANY,SplitDataFrameList-method}
\alias{coerce,ANY,SimpleSplitDataFrameList-method}
\alias{coerce,ANY,CompressedDataFrameList-method}
\alias{coerce,ANY,CompressedSplitDataFrameList-method}

\alias{coerce,DataFrameList,DFrame-method}
\alias{coerce,SplitDataFrameList,DFrame-method}

\alias{coerce,ANY,CompressedSplitDFrameList-method}
\alias{coerce,ANY,SimpleSplitDFrameList-method}
\alias{coerce,List,CompressedSplitDFrameList-method}
\alias{coerce,list,SplitDFrameList-method}
\alias{coerce,List,SimpleSplitDFrameList-method}
\alias{coerce,ANY,SplitDFrameList-method}
\alias{coerce,SimpleList,SplitDFrameList-method}
\alias{coerce,list,SplitDFrameList-method}
\alias{coerce,DataFrame,SplitDFrameList-method}
\alias{stack,DataFrameList-method}

% splitting and combining
\alias{cbind,DataFrameList-method}
\alias{rbind,DataFrameList-method}

% show
\alias{classNameForDisplay,SimpleDFrameList-method}
\alias{classNameForDisplay,CompressedDFrameList-method}
\alias{show,SplitDataFrameList-method}

% SDFLWrapperForTransform (internal)
\alias{colnames,SDFLWrapperForTransform-method}
\alias{[[,SDFLWrapperForTransform-method}
\alias{[[<-,SDFLWrapperForTransform-method}
\alias{as.env,SDFLWrapperForTransform-method}

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
    \item{}{\code{dimnames(x)}: Get the list of two CharacterLists,
      the first holding the rownames (possibly \code{NULL}) and the second
      the column names.
    }
  }

  In the following code snippets, \code{x} is a \code{SplitDataFrameList}.
  \describe{
    \item{}{\code{commonColnames(x)}: Get the character vector of 
      column names present in the individual DataFrames in \code{x}.} 
    \item{}{\code{commonColnames(x) <- value}: Set the column names of 
      the DataFrames in \code{x}.}
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
      be coerced into an \code{AtomicList} or \code{IntegerRangesList}
      subclass.
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

\section{Transformation}{
\describe{
    \item{}{\code{transform(`_data`, ...)}: Transforms a
    \code{SplitDataFrame} in a manner analogous to the
    base \code{\link{transform}}, where the columns are \code{List}
    objects adhering to the structure of \code{_data}.
  }
 }
}

\section{Coercion}{
  In the following code snippets, \code{x} is a \code{DataFrameList}.
  \describe{
    \item{}{\code{as(from, "DataFrame")}: Coerces a
      \code{SplitDataFrameList} to a \code{DataFrame}, which has a column
      for every column in \code{from}, except each column is a
      \code{List} with the same structure as \code{from}.
    }
    \item{}{\code{as(from, "SplitDataFrameList")}: By default, simply
      calls the \code{SplitDataFrameList} constructor
      on \code{from}. If \code{from} is a \code{List}, each element
      of \code{from} is passed as an argument
      to \code{SplitDataFrameList}, like calling \code{as.list} on a
      vector. If \code{from} is a \code{DataFrame}, each row becomes an
      element in the list.}
    \item{}{\code{stack(x, index.var = "name")}: Unlists \code{x} and
      adds a column named \code{index.var} to the result, indicating the
      element of \code{x} from which each row was obtained.
    }
    \item{}{
      \code{as.data.frame(x, row.names = NULL, optional = FALSE,
         ..., value.name = "value", use.outer.mcols = FALSE, 
         group_name.as.factor = FALSE)}: 
      Coerces \code{x} to a \code{data.frame}. See as.data.frame on the 
      \code{List} man page for details (?\code{List}). 
    }
  }
}

\examples{
# Making a DataFrameList, which has different columns.
out <- DataFrameList(DataFrame(X=1, Y=2), DataFrame(A=1:2, B=3:4))
out[[1]]

# A more interesting SplitDataFrameList, which is guaranteed
# to have the same columns.
out <- DataFrameList(DataFrame(X=1, Y=2), DataFrame(X=1:2, Y=3:4))
out[[1]]
out[,"X"]
out[,"Y"]

commonColnames(out)
commonColnames(out) <- c("x", "y")
out[[1]]

# We can also create these split objects using various split() functions:
out <- splitAsList(DataFrame(X=runif(100), Y=rpois(100, 5)), 
    sample(letters, 100, replace=TRUE))
out[[1]]
}

\author{ Michael Lawrence }
\seealso{
  \code{\linkS4class{DataFrame}}
}
\keyword{methods}
\keyword{classes}
