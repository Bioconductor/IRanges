\name{XDataFrame-class}
\docType{class}
\alias{XDataFrame-class}

% accessor
\alias{dim,XDataFrame-method}
\alias{dimnames,XDataFrame-method}
\alias{dimnames<-,XDataFrame,ANY-method}

% constructor
\alias{XDataFrame}

% subsetting
\alias{[,XDataFrame,ANY,ANY,ANY-method}
\alias{[[,XDataFrame-method}
\alias{[[<-,XDataFrame-method}

% coerce
\alias{as.data.frame,XDataFrame-method}
\alias{coerce,ANY,XDataFrame-method}
\alias{coerce,matrix,XDataFrame-method}
\alias{coerce,list,XDataFrame-method}
\alias{coerce,integer,XDataFrame-method}
\alias{coerce,data.frame,XDataFrame-method}
\alias{coerce,XDataFrame,data.frame-method}
\alias{as.list,XDataFrame-method}

% splitting and combining
\alias{cbind}
\alias{cbind,XDataFrame-method}
\alias{rbind}
\alias{rbind,XDataFrame-method}
\alias{split,XDataFrame-method}

% show
\alias{show,XDataFrame-method}

% other
\alias{is.array,XDataFrame-method}

\title{External Data Frame}
\description{
  The \code{XDataFrame} emulates the interface of
  \code{data.frame}, but it supports the storage of any type of object as a
  column, as long as the \code{length} and \code{[} methods are
  implemented. The \dQuote{X} in its name indicates that it attempts to coerce
  its columns to external \code{\linkS4class{XSequence}} objects in a
  way that is completely transparent to the user. This helps to avoid
  unncessary copying.
}

\details{
  On the whole, the \code{XDataFrame} behaves very similarly to
  \code{data.frame}, in terms of construction, subsetting, splitting,
  combining, etc. The most notable exception is that the row names are
  optional. This means calling \code{rownames(x)} will return
  \code{NULL} if there are no row names. Of course, it could return
  \code{seq_len(nrow(x))}, but returning \code{NULL} informs, for
  example, combination functions that no row names are desired (they are
  often a luxury when dealing with large data).

  As \code{XDataFrame} derives from \code{\linkS4class{AnnotatedList}},
  it is possible to set an \code{annotaiton} string. Also, another
  \code{XDataFrame} can hold metadata on the columns.
}

\note{ In the future, the general data frame functionality will probably
  be moved to a \code{DataFrame} class. \code{XDataFrame} will derive
  from \code{DataFrame} and encapsulate the behavior of attempting to
  coerce or even requiring columns to be \code{XSequence}.  }

\section{Accessors}{
  In the following code snippets, \code{x} is an \code{XDataFrame}.
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
  In the following code snippets, \code{x} is an \code{XDataFrame}.
  \describe{
    \item{}{\code{x[i,j,drop]}: Behaves very similarly to
      the \code{\link{[.data.frame}} method, except subsetting by
      \code{matrix} indices is not supported. Due to temporary limitations in
      the subsetting \code{XSequence} objects, indices containing
      \code{NA}'s are not yet supported.
    }
    \item{}{\code{x[[i]]}: Behaves very similarly to the
      \code{\link{[[.data.frame}} method, except arguments \code{j}
      (why?) and \code{exact} are not supported. Column name matching is
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
  \describe{\code{XDataFrame(..., row.names = NULL)}:
    Constructs an \code{XDataFrame} in similar fashion to
    \code{\link{data.frame}}. Each argument in \code{...} is coerced to
    an \code{XDataFrame} and combined column-wise. No special effort is
    expended to automatically determine the row names from the
    arguments. The row names should be given in
    \code{row.names}; otherwise, there are no row names. This is by
    design, as row names are normally undesirable when data is large.
  }
}

\section{Splitting and Combining}{
  In the following code snippets, \code{x} is an \code{XDataFrame}.
  
  \describe{
    \item{}{\code{split(x, f, drop = FALSE)}:
      Splits \code{x} into a \code{\linkS4class{SplitXDataFrameList}},
      according to \code{f}, dropping elements corresponding to
      unrepresented levels if \code{drop} is \code{TRUE}.
    }
    \item{}{
      \code{rbind(...)}: Creates a new \code{XDataFrame} by
      combining the rows of the \code{XDataFrame} objects in
      \code{...}. Very similar to \code{\link{rbind.data.frame}}, except
      in the handling of row names. If all elements have row names, they
      are concatenated and made unique. Otherwise, the result does not
      have row names. Currently, factors are not handled well (their
      levels are dropped). This is not a high priority until there is an
      \code{XFactor} class.
    }
    \item{}{
      \code{cbind(...)}: Creates a new \code{XDataFrame} by
      combining the columns of the \code{XDataFrame} objects in
      \code{...}. Very similar to \code{\link{cbind.data.frame}}, except
      row names, if any, are dropped. Consider the \code{XDataFrame}
      as an alternative that allows one to specify row names.
    }
  }
}

\section{Coercion}{
  \describe{
    \item{}{\code{as(from, "XDataFrame")}:
      By default, constructs a new \code{XDataFrame} with \code{from} as
      its only column. If \code{from} is a \code{matrix} or
      \code{data.frame}, all of 
      its columns become columns in the new \code{XDataFrame}. In any
      case, there is an attempt to coerce 
      columns to \code{XSequence} before inserting them into the
      \code{XDataFrame}. If \code{from} is a \code{list}, its elements
      become columns in the same way. Note that for the
      \code{XDataFrame} to behave 
      correctly, each column object must support element-wise subsetting via
      the \code{[} method and return the number of elements with
      \code{length}. It is recommended to use the \code{XDataFrame} 
      constructor, rather than this interface.
    }
    \item{}{\code{as.list(x)}: Coerces \code{x}, an \code{XDataFrame},
      to a \code{list}, converting any \code{XSequence} objects to vectors
      along the way.
    }
    \item{}{\code{as.data.frame(x, row.names=NULL, optional=FALSE)}:
      Coerces \code{x}, an \code{XDataFrame}, to a \code{data.frame}.
      Each column is coerced to a \code{vector} and stored as a column in
      the \code{data.frame}. If \code{row.names} is \code{NULL}, they
      are retrieved from \code{x}, if it has any. Otherwise, they are
      inferred by the \code{data.frame} constructor.
    }
    \item{}{\code{as(from, "data.frame")}: Coerces a \code{XDataFrame}
      to a \code{data.frame} by calling \code{as.data.frame(from)}.
    }
  }
}

\author{ Michael Lawrence }
\seealso{
  \code{\linkS4class{RangedData}}, which makes heavy use of this class.
}
\examples{
  score <- c(1L, 3L, NA)
  counts <- c(10L, 2L, NA)
  row.names <- c("one", "two", "three")
  
  xdf <- XDataFrame(score) # single column
  xdf[["score"]]
  xdf <- XDataFrame(score, row.names = row.names) #with row names
  rownames(xdf)
  
  xdf <- XDataFrame(vals = score) # explicit naming
  xdf[["vals"]]
  
  # a data.frame
  sw <- XDataFrame(swiss)
  as.data.frame(sw) # swiss, without row names
  # now with row names
  sw <- XDataFrame(swiss, row.names = rownames(swiss))
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
  
  sw[1] # one-column XDataFrame
  ## the same
  sw[, 1, drop = FALSE]
  sw[, 1] # a (unnamed) vector
  sw[[1]] # the same
  sw[["Fertility"]]

  sw[["Fert"]] # should return 'NULL'
  
  sw[1,] # a one-row XDataFrame
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

  sw <- XDataFrame(swiss)
  swsplit <- split(sw, sw[["Education"]])
  
  ## rbind

  do.call(rbind, as.list(swsplit))

  ## cbind

  cbind(XDataFrame(score), XDataFrame(counts))
}
\keyword{classes}
\keyword{methods}
