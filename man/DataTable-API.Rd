\name{DataTable-API}
\docType{class}

\alias{DataTable-API}

% DataTable class, functions and methods:
\alias{class:DataTable}
\alias{DataTable-class}
\alias{DataTable}
\alias{class:DataTableORNULL}
\alias{DataTableORNULL-class}
\alias{DataTableORNULL}

\alias{NROW,DataTable-method}
\alias{NCOL,DataTable-method}
\alias{dim,DataTable-method}
\alias{dimnames,DataTable-method}
\alias{dimnames<-,DataTable-method}

\alias{window,DataTable-method}
\alias{window<-,DataTable-method}
\alias{seqselect,DataTable-method}
\alias{seqselect<-,DataTable-method}
\alias{head,DataTable-method}
\alias{tail,DataTable-method}
\alias{subset,DataTable-method}
\alias{na.omit}
\alias{na.omit,DataTable-method}
\alias{na.exclude}
\alias{na.exclude,DataTable-method}
\alias{is.na,DataTable-method}
\alias{complete.cases,DataTable-method}

\alias{cbind,DataTable-method}
\alias{rbind,DataTable-method}
\alias{merge,DataTable,DataTable-method}
\alias{merge,data.frame,DataTable-method}
\alias{merge,DataTable,data.frame-method}

\alias{aggregate,DataTable-method}
\alias{by,DataTable-method}

\alias{as.env,DataTable-method}

\alias{duplicated,DataTable-method}
\alias{unique,DataTable-method}

\alias{show,DataTable-method}

\title{The DataTable API}

\description{
  DataTable is an API only (i.e. virtual class with no slots) for accessing
  objects with a rectangular shape like \link{DataFrame} or \link{RangedData}
  objects. It mimics the API for standard \link{data.frame} objects.
}

\section{Accessors}{
  In the following code snippets, \code{x} is a \code{DataTable}.
  \describe{
    \item{}{
      \code{nrow(x)}, \code{ncol(x)}:
      Get the number of rows and columns, respectively.
    }
    \item{}{
      \code{NROW(x)}, \code{NCOL(x)}:
      Same as \code{nrow(x)} and \code{ncol(x)}, respectively.
    }
    \item{}{
      \code{dim(x)}:
      Length two integer vector defined as \code{c(nrow(x), ncol(x))}.
    }
    \item{}{
      \code{rownames(x)}, \code{colnames(x)}:
      Get the names of the rows and columns, respectively.
    }
    \item{}{
      \code{dimnames(x)}:
      Length two list of character vectors defined as
      \code{list(rownames(x), colnames(x))}.
    }
  }
}

\section{Subsetting}{
  In the code snippets below, \code{x} is a DataTable object.

  \describe{
    \item{}{
      \code{x[i, j, drop=TRUE]}:
      Return a new DataTable object made of the selected rows and columns. For
      single column selection, the \code{drop} argument specifies whether or not
      to coerce the returned sequence to a standard vector.
    }
    \item{}{
      \code{window(x, start = NA, end = NA, width = NA, frequency = NULL, delta = NULL, ...)}:
      Extract the subsequence window from the DataTable object using:
      \describe{
        \item{\code{start}, \code{end}, \code{width}}{The start, end, or width
          of the window. Two of the three are required.}
        \item{\code{frequency}, \code{delta}}{Optional arguments that specify
          the sampling frequency and increment within the window.}
      }
      In general, this is more efficient than using \code{"["} operator.
    }
    \item{}{
      \code{window(x, start = NA, end = NA, width = NA, keepLength =
        TRUE) <- value}:
      Replace the subsequence window specified on the left (i.e. the subsequence
      in \code{x} specified by \code{start}, \code{end} and \code{width})
      by \code{value}.
      \code{value} must either be of class \code{class(x)}, belong to a subclass
      of \code{class(x)}, be coercible to \code{class(x)}, or be \code{NULL}.
      If \code{keepLength} is \code{TRUE}, the number of rows of \code{value}
      are repeated to create a DataTable with the same number of rows as the
      width of the subsequence window it is replacing.
      If \code{keepLength} is \code{FALSE}, this replacement method can modify
      the number of rows of \code{x}, depending on how the number of rows of the
      left subsequence window compares to the number of rows of \code{value}.
    }
    \item{}{
      \code{seqselect(x, start=NULL, end=NULL, width=NULL)}:
      Similar to \code{window}, except that multiple subsequences can be
      requested. The requested subsequences are concatenated.
    }
    \item{}{
      \code{seqselect(x, start=NULL, end=NULL, width=NULL) <- value}:
      Similar to \code{window<-}, except that multiple consecutive subsequences
      can be replaced by a \code{value} that spans those windows.
    }
    \item{}{
      \code{head(x, n = 6L)}:
      If \code{n} is non-negative, returns the first n rows of the DataTable
      object.
      If \code{n} is negative, returns all but the last \code{abs(n)} rows of
      the DataTable object.
    }
    \item{}{
      \code{tail(x, n = 6L)}:
      If \code{n} is non-negative, returns the last n rows of the DataTable
      object.
      If \code{n} is negative, returns all but the first \code{abs(n)} rows of
      the DataTable object.
    }
    \item{}{
      \code{subset(x, subset, select, drop = FALSE)}:
      Return a new DataTable object using:
      \describe{
        \item{subset}{logical expression indicating rows to keep, where missing
          values are taken as FALSE.}
        \item{select}{expression indicating columns to keep.}
        \item{drop}{passed on to \code{[} indexing operator.}
      }
    }
    \item{}{
      \code{\link[stats:na.fail]{na.omit}(object)}:
      Returns a subset with incomplete cases removed.
    }
    \item{}{
      \code{\link[stats:na.fail]{na.exclude}(object)}:
      Returns a subset with incomplete cases removed (but to be
      included with NAs in statistical results).
    }
    \item{}{
      \code{\link[base:NA]{is.na}(x)}:
      Returns a logical matrix indicating which cells are missing.
    }
    \item{}{
      \code{\link[stats]{complete.cases}(x)}:
      Returns a logical vector identifying which cases have no missing values.
    }
  }
}

\section{Combining}{
  In the code snippets below, \code{x} is a DataTable object.

  \describe{
    \item{}{
      \code{cbind(...)}: Creates a new \code{DataTable} by combining the columns
      of the \code{DataTable} objects in \code{...}.
    }
    \item{}{
      \code{rbind(...)}: Creates a new \code{DataTable} by combining the rows of
      the \code{DataTable} objects in \code{...}.
    }
    \item{}{
      \code{merge(x, y, ...)}: Merges two \code{DataTable}
      objects \code{x} and \code{y}, with arguments in \code{...} being
      the same as those allowed by the base \code{\link{merge}}. It is
      allowed for either \code{x} or \code{y} to be a \code{data.frame}.
    }
  }
}

\section{Looping}{
  In the code snippets below, \code{x} is a DataTable object.

  \describe{
    \item{}{
      \code{aggregate(x, by, FUN, start = NULL, end = NULL, width = NULL,
                      frequency = NULL, delta = NULL, ..., simplify = TRUE)}:
      Generates summaries on the specified windows and returns the result in a
      convenient form:
      \describe{
        \item{\code{by}}{An object with \code{start}, \code{end}, and
          \code{width} methods.}
        \item{\code{FUN}}{The function, found via \code{match.fun}, to be
          applied to each window of \code{x}.}
        \item{\code{start}, \code{end}, \code{width}}{the start, end, or width
          of the window. If \code{by} is missing, then must supply two of the
          three.}
        \item{\code{frequency}, \code{delta}}{Optional arguments that specify
          the sampling frequency and increment within the window.}
        \item{\dots}{Further arguments for \code{FUN}.}
        \item{\code{simplify}}{A logical value specifying whether or not the
          result should be simplified to a vector or matrix if possible.}
      }
    }
    
    \item{}{
      \code{by(data, INDICES, FUN, ..., simplify = TRUE)}:
      Apply \code{FUN} to each group of \code{data}, a \code{DataTable},
      formed by the factor (or list of factors) \code{INDICES}. Exactly 
      the same contract as \code{\link{as.data.frame}}.
    }
  }
}

\section{Utilities}{
  \describe{
    \item{}{\code{duplicated(x)}: Returns a logical vector indicating the rows
      that are identical to a previous row.
    }
    \item{}{\code{unique(x)}: Returns a new \code{DataTable} after
      removing the duplicated rows from \code{x}.
    }
  }
}

\section{Coercion}{
  \describe{
    \item{}{\code{as.env(x, enclos = parent.frame())}:
      Creates an environment from \code{x} with a symbol for each
      \code{colnames(x)}. The values are not actually copied into the
      environment. Rather, they are dynamically bound using
      \code{\link{makeActiveBinding}}. This prevents unnecessary copying
      of the data from the external vectors into R vectors. The values
      are cached, so that the data is not copied every time the symbol
      is accessed.
    }
  }
}
  
\seealso{
  \link{DataTable-stats} for statistical functionality, like fitting
  regression models,
  \link{data.frame}
}

\examples{
  showClass("DataTable")  # shows (some of) the known subclasses
}

\keyword{methods}
\keyword{classes}
