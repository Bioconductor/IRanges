\name{DataFrame-class}
\docType{class}
\alias{DataFrame-class}

% accessor
\alias{dim,DataFrame-method}
\alias{dimnames,DataFrame-method}
\alias{dimnames<-,DataFrame-method}

% constructor
\alias{DataFrame}
\alias{XDataFrame}

% subsetting
\alias{[,DataFrame-method}
\alias{[[,DataFrame-method}
\alias{[[<-,DataFrame-method}

% coerce
\alias{as.data.frame,DataFrame-method}
\alias{coerce,ANY,DataFrame-method}
\alias{coerce,matrix,DataFrame-method}
\alias{coerce,list,DataFrame-method}
\alias{coerce,integer,DataFrame-method}
\alias{coerce,data.frame,DataFrame-method}
\alias{coerce,DataFrame,data.frame-method}

% splitting and combining
\alias{cbind,DataFrame-method}
\alias{rbind,DataFrame-method}
\alias{split,DataFrame-method}

% show
\alias{show,DataFrame-method}

\title{External Data Frame}
\description{
  The \code{DataFrame} extends the \code{\linkS4class{DataTable}} virtual
  class and supports the storage of any type of object (with \code{length}
  and \code{[} methods) as columns.
}

\details{
  On the whole, the \code{DataFrame} behaves very similarly to
  \code{data.frame}, in terms of construction, subsetting, splitting,
  combining, etc. The most notable exception is that the row names are
  optional. This means calling \code{rownames(x)} will return
  \code{NULL} if there are no row names. Of course, it could return
  \code{seq_len(nrow(x))}, but returning \code{NULL} informs, for
  example, combination functions that no row names are desired (they are
  often a luxury when dealing with large data).

  As \code{DataFrame} derives from \code{\linkS4class{Sequence}}, it is
  possible to set an \code{annotation} string. Also, another
  \code{DataFrame} can hold metadata on the columns.
}

\section{Accessors}{
  In the following code snippets, \code{x} is an \code{DataFrame}.
  \describe{
    \item{}{\code{dim(x)}:
      Get the length two integer vector indicating in the first and
      second element the number of rows and columns, respectively.
    }
    \item{}{\code{dimnames(x)}, \code{dimnames(x) <- value}:
      Get and set the two element list containing the row names
      (character vector of length \code{nrow(x)} or \code{NULL})
      and the column names (character vector of length \code{ncol(x)}).
    }
  }
}

\section{Subsetting}{
  In the following code snippets, \code{x} is an \code{DataFrame}.
  \describe{
    \item{}{\code{x[i,j,drop]}: Behaves very similarly to the
      \code{\link{[.data.frame}} method, except \code{i} can be a
      logical \code{Rle} object and subsetting by \code{matrix} indices
      is not supported. Due to limitations in the subsetting of
      \code{XSequence} objects, indices containing \code{NA}'s are not
      supported.
    }
    \item{}{\code{x[[i]]}: Behaves very similarly to the
      \code{\link{[[.data.frame}} method, except arguments \code{j}
      and \code{exact} are not supported. Column name matching is
      always exact. Subsetting by matrices is not supported.
    }
    \item{}{\code{x[[i]] <- value}: Behaves very similarly to the
      \code{\link{[[<-.data.frame}} method, except the argument \code{j}
      is not supported. An attempt is made to coerce \code{value} to a
      \code{XSequence} object.
    }
  }
}

\section{Constructor}{
  \describe{\code{DataFrame(..., row.names = NULL)}:
    Constructs an \code{DataFrame} in similar fashion to
    \code{\link{data.frame}}. Each argument in \code{...} is coerced to
    an \code{DataFrame} and combined column-wise. No special effort is
    expended to automatically determine the row names from the
    arguments. The row names should be given in
    \code{row.names}; otherwise, there are no row names. This is by
    design, as row names are normally undesirable when data is large.
  }
}

\section{Splitting and Combining}{
  In the following code snippets, \code{x} is an \code{DataFrame}.
  
  \describe{
    \item{}{\code{split(x, f, drop = FALSE)}:
      Splits \code{x} into a \code{\linkS4class{SplitDataFrameList}},
      according to \code{f}, dropping elements corresponding to
      unrepresented levels if \code{drop} is \code{TRUE}.
    }
    \item{}{
      \code{rbind(...)}: Creates a new \code{DataFrame} by
      combining the rows of the \code{DataFrame} objects in
      \code{...}. Very similar to \code{\link{rbind.data.frame}}, except
      in the handling of row names. If all elements have row names, they
      are concatenated and made unique. Otherwise, the result does not
      have row names. Currently, factors are not handled well (their
      levels are dropped). This is not a high priority until there is an
      \code{XFactor} class.
    }
    \item{}{
      \code{cbind(...)}: Creates a new \code{DataFrame} by
      combining the columns of the \code{DataFrame} objects in
      \code{...}. Very similar to \code{\link{cbind.data.frame}}, except
      row names, if any, are dropped. Consider the \code{DataFrame}
      as an alternative that allows one to specify row names.
    }
  }
}

\section{Coercion}{
  \describe{
    \item{}{\code{as(from, "DataFrame")}:
      By default, constructs a new \code{DataFrame} with \code{from} as
      its only column. If \code{from} is a \code{matrix} or
      \code{data.frame}, all of 
      its columns become columns in the new \code{DataFrame}. In any
      case, there is an attempt to coerce 
      columns to \code{XSequence} before inserting them into the
      \code{DataFrame}. If \code{from} is a \code{list}, its elements
      become columns in the same way. Note that for the
      \code{DataFrame} to behave 
      correctly, each column object must support element-wise subsetting via
      the \code{[} method and return the number of elements with
      \code{length}. It is recommended to use the \code{DataFrame} 
      constructor, rather than this interface.
    }
    \item{}{\code{as.list(x)}: Coerces \code{x}, an \code{DataFrame},
      to a \code{list}, converting any \code{XSequence} objects to vectors
      along the way.
    }
    \item{}{\code{as.data.frame(x, row.names=NULL, optional=FALSE)}:
      Coerces \code{x}, an \code{DataFrame}, to a \code{data.frame}.
      Each column is coerced to a \code{vector} and stored as a column in
      the \code{data.frame}. If \code{row.names} is \code{NULL}, they
      are retrieved from \code{x}, if it has any. Otherwise, they are
      inferred by the \code{data.frame} constructor.
    }
    \item{}{\code{as(from, "data.frame")}: Coerces a \code{DataFrame}
      to a \code{data.frame} by calling \code{as.data.frame(from)}.
    }
  }
}

\author{ Michael Lawrence }
\seealso{
  \code{\linkS4class{DataTable}},
  \code{\linkS4class{Sequence}}, and
  \code{\linkS4class{RangedData}}, which makes heavy use of this class.
}
\examples{
  score <- c(1L, 3L, NA)
  counts <- c(10L, 2L, NA)
  row.names <- c("one", "two", "three")
  
  xdf <- DataFrame(score) # single column
  xdf[["score"]]
  xdf <- DataFrame(score, row.names = row.names) #with row names
  rownames(xdf)
  
  xdf <- DataFrame(vals = score) # explicit naming
  xdf[["vals"]]
  
  # a data.frame
  sw <- DataFrame(swiss)
  as.data.frame(sw) # swiss, without row names
  # now with row names
  sw <- DataFrame(swiss, row.names = rownames(swiss))
  as.data.frame(sw) # swiss

  # subsetting
    
  sw[] # identity subset
  sw[,] # same

  sw[NULL] # no columns
  sw[,NULL] # no columns
  sw[NULL,] # no rows

  ## select columns
  sw[1:3]
  sw[,1:3] # same as above
  sw[,"Fertility"]
  sw[,c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)]

  ## select rows and columns
  sw[4:5, 1:3]
  
  sw[1] # one-column DataFrame
  ## the same
  sw[, 1, drop = FALSE]
  sw[, 1] # a (unnamed) vector
  sw[[1]] # the same
  sw[["Fertility"]]

  sw[["Fert"]] # should return 'NULL'
  
  sw[1,] # a one-row DataFrame
  sw[1,, drop=TRUE] # a list

  ## duplicate row, unique row names are created
  sw[c(1, 1:2),]

  ## indexing by row names  
  sw["Courtelary",]
  subsw <- sw[1:5,1:4]
  subsw["C",] # partially matches

  ## row and column names
  cn <- paste("X", seq_len(ncol(swiss)), sep = ".")
  colnames(sw) <- cn
  colnames(sw)
  rn <- seq(nrow(sw))
  rownames(sw) <- rn
  rownames(sw)

  ## column replacement

  xdf[["counts"]] <- counts
  xdf[["counts"]]
  xdf[[3]] <- score
  xdf[["X"]]
  xdf[[3]] <- NULL # deletion

  ## split

  sw <- DataFrame(swiss)
  swsplit <- split(sw, sw[["Education"]])
  
  ## rbind

  do.call(rbind, as.list(swsplit))

  ## cbind

  cbind(DataFrame(score), DataFrame(counts))
}
\keyword{classes}
\keyword{methods}
