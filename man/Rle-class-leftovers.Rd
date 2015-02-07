\name{Rle-class-leftovers}
\docType{class}

\alias{ranges,Rle-method}

\alias{coerce,Rle,IRanges-method}
\alias{coerce,Rle,NormalIRanges-method}

\alias{window.Rle}
\alias{window,Rle-method}

\alias{findRange}
\alias{findRange,Rle-method}

\alias{splitRanges}
\alias{splitRanges,Rle-method}
\alias{splitRanges,vectorORfactor-method}


\title{Rle objects (old man page)}

\description{
  IMPORTANT NOTE - 7/3/2014: This man page is being refactored. Most of
  the things that used to be documented here have been moved to the man
  page for \link[S4Vectors]{Rle} objects located in the \pkg{S4Vectors}
  package.
}

\section{Coercion}{
  In the code snippets below, \code{from} is an Rle object:
  \describe{
    \item{}{
      \code{as(from, "IRanges")}: Creates an \link{IRanges} instance
      from a logical Rle. Note that this instance is guaranteed to be normal.
    }
    \item{}{
      \code{as(from, "NormalIRanges")}: Creates a \link{NormalIRanges} instance
      from a logical Rle.
    }
  }
}

\section{General Methods}{
  In the code snippets below, \code{x} is an Rle object:

  \describe{
    \item{}{
      \code{window(x, start=NA, end=NA, width=NA)}:
      Extract the subsequence window from \code{x} specified by:
      \describe{
        \item{\code{start}, \code{end}, \code{width}}{The start, end, or width
          of the window. Two of the three are required.}
      }
    }
    \item{}{
      \code{window(x, start=NA, end=NA, width=NA) <- value}:
      Replace the subsequence window specified on the left (i.e. the
      subsequence in \code{x} specified by \code{start}, \code{end} and
      \code{width}) by \code{value}.
      \code{value} must either be of class Rle, belong to a
      subclass of Rle, or be coercible to Rle or a
      subclass of Rle.
      The elements of \code{value} are repeated to create an Rle with the
      same number of elements as the width of the subsequence window it is
      replacing.
    }
    \item{}{
      \code{split(x, f, drop=FALSE)}:
      Splits \code{x} according to \code{f} to create a
      \link{CompressedRleList} object.
      If \code{f} is a list-like object then \code{drop} is ignored
      and \code{f} is treated as if it was
      \code{rep(seq_len(length(f)), sapply(f, length))},
      so the returned object has the same shape as \code{f} (it also
      receives the names of \code{f}).
      Otherwise, if \code{f} is not a list-like object, empty list
      elements are removed from the returned object if \code{drop} is
      \code{TRUE}.
    }
    \item{}{
      \code{findRange(x, vec)}:
      Returns an \link{IRanges} object representing the ranges in Rle \code{vec}
      that are referenced by the indices in the integer vector \code{x}.
    }
    \item{}{
      \code{splitRanges(x)}:
      Returns a \linkS4class{CompressedIRangesList} object that contains the
      ranges for each of the unique run values.
    }
  }
}

\seealso{
  The \link[S4Vectors]{Rle} class defined and documented in the
  \pkg{S4Vectors} package.
}

\examples{
x <- Rle(10:1, 1:10)
x

window(x, 4, 14)
}

\keyword{methods}
\keyword{classes}
