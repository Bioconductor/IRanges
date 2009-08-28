\name{DataTable-class}
\docType{class}

% DataTable class, functions and methods:
\alias{class:DataTable}
\alias{DataTable-class}
\alias{DataTableORNULL-class}
\alias{DataTable}

\alias{cbind}
\alias{cbind,DataTable-method}
\alias{dim,DataTable-method}
\alias{head,DataTable-method}
\alias{is.array,DataTable-method}
\alias{rbind}
\alias{rbind,DataTable-method}
\alias{seqextract,DataTable-method}
\alias{show,DataTable-method}
\alias{subset,DataTable-method}
\alias{tail,DataTable-method}
\alias{window,DataTable-method}
\alias{!=,DataTable,DataTable-method}
\alias{aggregate,DataTable-method}
\alias{by,DataTable-method}
\alias{as.env,DataTable-method}

\title{DataTable objects}

\description{
  The DataTable virtual class provides an interface for the storing rectangular
  data sets, like a basic data.frame object. It extends \code{Sequence}.
}

\section{Accessors}{
  In the following code snippets, \code{x} is an \code{DataTable}.
  \describe{
    \item{}{\code{dim(x)}:
      Get the length two integer vector indicating in the first and second
      element the number of rows and columns, respectively. This implies the
      existence of \code{nrow(x)} and \code{ncol(x)}.
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
      \code{head(x, n = 6L)}:
      If \code{n} is non-negative, returns the first n rows of the DataTable
      object.
      If \code{n} is negative, returns all but the last \code{abs(n)} rows of
      the DataTable object.
    }
    \item{}{
      \code{length(x)}:
      Return the number of columns in the DataTable object.
    }
    \item{}{
      \code{seqextract(x, start=NULL, end=NULL, width=NULL)}:
      Like \code{window}, except that multiple subsequences can be requested.
      The requested subsequences are concatenated.
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
      \code{tail(x, n = 6L)}:
      If \code{n} is non-negative, returns the last n rows of the DataTable
      object.
      If \code{n} is negative, returns all but the first \code{abs(n)} rows of
      the DataTable object.
    }
    \item{}{
      \code{window(x, start = NULL, end = NULL, width = NULL, frequency = NULL, delta = NULL, ...)}:
      Extract the subsequence window from the DataTable object using:
      \describe{
        \item{\code{start}, \code{end}, \code{width}}{The start, end, or width
          of the window. Two of the three are required.}
        \item{\code{frequency}, \code{delta}}{Optional arguments that specify
          the sampling frequency and increment within the window.}
      }
      In general, this is more efficient than using \code{"["} operator.
    }
  }
}

\section{Looping}{
  In the code snippets below, \code{x} is a DataTable object.

  \describe{
    \item{}{
      \code{aggregate(x, by, FUN, start = NULL, end = NULL, width = NULL,
                      frequency = NULL, delta = NULL, ..., simplify = TRUE))}:
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
      Apply \code{FUN} to each group of \code{data}, an \code{DataTable},
      formed by the factor (or list of factors) \code{INDICES}. Exactly 
      the same contract as \code{\link{as.data.frame}}.
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
  \link{DataFrame-class} for an implementation that mimics
  \code{data.frame},
  \link{Sequence-class}
}

\examples{
  showClass("DataTable")  # shows (some of) the known subclasses
}

\keyword{methods}
\keyword{classes}
