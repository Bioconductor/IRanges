\name{DataFrame-class}
\docType{class}

% DataFrame class, functions and methods:
\alias{class:DataFrame}
\alias{DataFrame-class}
\alias{DataFrame}

\alias{[,DataFrame-method}
\alias{[<-,DataFrame-method}
\alias{cbind}
\alias{cbind,DataFrame-method}
\alias{dim,DataFrame-method}
\alias{head,DataFrame-method}
\alias{is.array,DataFrame-method}
\alias{rbind}
\alias{rbind,DataFrame-method}
\alias{seqextract,DataFrame-method}
\alias{subset,DataFrame-method}
\alias{tail,DataFrame-method}
\alias{window,DataFrame-method}
\alias{!=,DataFrame,DataFrame-method}
\alias{aggregate,DataFrame-method}


\title{DataFrame objects}

\description{
  The DataFrame virtual class provides an interface for the storing parallel/
  aligned sequences, like a basic data.frame object. It extends \code{ListLike}.
}

\section{Accessors}{
  In the following code snippets, \code{x} is an \code{DataFrame}.
  \describe{
    \item{}{\code{dim(x)}:
      Get the length two integer vector indicating in the first and second
      element the number of rows and columns, respectively. This implies the
      existence of \code{nrow(x)} and \code{ncol(x)}.
    }
  }
}

\section{Combining}{
  In the code snippets below, \code{x} is a DataFrame object.

  \describe{
    \item{}{
      \code{cbind(...)}: Creates a new \code{DataFrame} by combining the columns
      of the \code{DataFrame} objects in \code{...}.
    }
    \item{}{
      \code{rbind(...)}: Creates a new \code{DataFrame} by combining the rows of
      the \code{DataFrame} objects in \code{...}.
    }
  }
}

\section{Subsetting}{
  In the code snippets below, \code{x} is a DataFrame object.

  \describe{
    \item{}{
      \code{x[i, j, drop=TRUE]}:
      Return a new DataFrame object made of the selected rows and columns. For
      single column selection, the \code{drop} argument specifies whether or not
      to coerce the returned sequence to a standard vector.
    }
    \item{}{
      \code{head(x, n = 6L)}:
      If \code{n} is non-negative, returns the first n rows of the DataFrame
      object.
      If \code{n} is negative, returns all but the last \code{abs(n)} rows of
      the DataFrame object.
    }
    \item{}{
      \code{length(x)}:
      Return the number of columns in the DataFrame object.
    }
    \item{}{
      \code{seqextract(x, start=NULL, end=NULL, width=NULL)}:
      Like \code{window}, except that multiple subsequences can be requested.
      The requested subsequences are concatenated.
    }
    \item{}{
      \code{subset(x, subset, select, drop = FALSE)}:
      Return a new DataFrame object using:
      \describe{
        \item{subset}{logical expression indicating rows to keep, where missing
          values are taken as FALSE.}
        \item{select}{expression indicating columns to keep.}
        \item{drop}{passed on to \code{[} indexing operator.}
      }
    }
    \item{}{
      \code{tail(x, n = 6L)}:
      If \code{n} is non-negative, returns the last n rows of the DataFrame
      object.
      If \code{n} is negative, returns all but the first \code{abs(n)} rows of
      the DataFrame object.
    }
    \item{}{
      \code{window(x, start = NULL, end = NULL, width = NULL, frequency = NULL, delta = NULL, ...)}:
      Extract the subsequence window from the DataFrame object using:
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
  In the code snippets below, \code{x} is a DataFrame object.

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
  }
}

\seealso{
  \link{ListLike-class},
  \link{XDataFrame-class},
  \code{\link{solveUserSEW}}
}

\examples{
  showClass("DataFrame")  # shows (some of) the known subclasses
}

\keyword{methods}
\keyword{classes}
