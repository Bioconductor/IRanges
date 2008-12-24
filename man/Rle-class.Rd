\name{Rle-class}
\docType{class}

\alias{class:Rle}
\alias{Rle-class}
\alias{Rle}
\alias{Rle,vector,missing-method}
\alias{Rle,vector,integer-method}
\alias{Rle,vector,numeric-method}

\alias{as.vector,Rle,missing-method}
\alias{as.logical,Rle-method}
\alias{as.integer,Rle-method}
\alias{as.numeric,Rle-method}
\alias{as.complex,Rle-method}
\alias{as.character,Rle-method}
\alias{as.raw,Rle-method}
\alias{coerce,vector,Rle-method}
\alias{coerce,Rle,vector-method}
\alias{coerce,Rle,logical-method}
\alias{coerce,Rle,integer-method}
\alias{coerce,Rle,numeric-method}
\alias{coerce,Rle,complex-method}
\alias{coerce,Rle,character-method}
\alias{coerce,Rle,raw-method}

\alias{length,Rle-method}
\alias{c,Rle-method}
\alias{[,Rle,ANY,ANY,ANY-method}
\alias{subseq,Rle-method}
\alias{rep,Rle-method}
\alias{show,Rle-method}

\alias{Ops,Rle,Rle-method}
\alias{Ops,Rle,vector-method}
\alias{Ops,vector,Rle-method}
\alias{Math,Rle-method}
\alias{Math2,Rle-method}
\alias{Summary,Rle-method}

\alias{!,Rle-method}
\alias{mean,Rle-method}
\alias{median,Rle-method}
\alias{var,Rle,missing-method}
\alias{sd,Rle-method}


\title{Rle objects}

\description{
  The Rle class is a general container for storing an atomic vector
  that is stored in a run-length encoding format. It is based on the
  \code{\link[base]{rle}} function from the base package.
}

\section{Constructors}{
  \describe{
    \item{}{
      \code{Rle(values)}:
      This constructor creates an \code{Rle} instances out of an atomic
      vector \code{values}.
    }
    \item{}{
      \code{Rle(values, lengths)}:
      This constructor creates an \code{Rle} instances out of an atomic
      vector \code{values} and an integer or numeric vector \code{lengths}
      with all positive elements that represent how many times each value
      is repeated. The length of these two vectors must be the same.
    }
    \item{}{
      \code{as(from, "Rle")}:
      This constructor creates an \code{Rle} instances out of an atomic
      vector \code{from}.
    }
  }
}

\section{Coercion}{
  In the code snippets below, \code{x} and \code{from} are \code{Rle} objects:

  \describe{
    \item{}{
      \code{as.vector(x)}, \code{as(from, "vector")}: Creates an atomic vector
      of the most appropriate type based on the values contained in \code{x}.
    }
    \item{}{
      \code{as.logical(x)}, \code{as(from, "logical")}: Creates a logical
      vector based on the values contained in \code{x}.
    }
    \item{}{
      \code{as.integer(x)}, \code{as(from, "integer")}: Creates an integer
      vector based on the values contained in \code{x}.
    }
    \item{}{
      \code{as.numeric(x)}, \code{as(from, "numeric")}: Creates a numeric
      vector based on the values contained in \code{x}.
    }
    \item{}{
      \code{as.complex(x)}, \code{as(from, "complex")}: Creates a complex
      vector based on the values contained in \code{x}.
    }
    \item{}{
      \code{as.character(x)}, \code{as(from, "character")}: Creates a character
      vector based on the values contained in \code{x}.
    }
    \item{}{
      \code{as.raw(x)}, \code{as(from, "raw")}: Creates a raw
      vector based on the values contained in \code{x}.
    }
  }
}

\section{General Methods}{
  In the code snippets below, \code{x} is an \code{Rle} objects:

  \describe{
    \item{}{
      \code{length(x)}:
      Returns the underlying vector length of \code{x}.
    }
    \item{}{
      \code{c(x, ...)}:
      Combines a set of \code{Rle} objects.
    }
    \item{}{
      \code{x[i]}:
      Subsets \code{x} by index \code{i}, where \code{i} can be positive
      integers, negative integers, a logical vector of the same length as
      \code{x}, or an \code{Rle} object of the same length as \code{x}
      containing logical values.
    }
    \item{}{
      \code{subseq(x, start=NA, end=NA, width=NA)}:
      Extract the subsequence from \code{x} specified by two of the three
      following values:  \code{start}, \code{end}, and \code{width}. This
      is more efficient for extracting consecutive values than \code{[}.
    }
    \item{}{
      \code{rep(x, times, length.out, each)}:
      Repeats the values in \code{x} through one of the following conventions:
      \describe{
        \item{\code{times}}{vector giving the number of times to repeat each
        element if of length \code{length(x)}, or to repeat the whole vector if
        of length 1.}
        \item{\code{length.out}}{non-negative integer. The desired length of
        the output vector.}
        \item{\code{each}}{non-negative integer.  Each element of \code{x} is
        repeated \code{each} times.}
      }
    }
    \item{}{
      \code{show(object)}:
      Prints out the \code{Rle} object in a user-friendly way.
    }
  }
}

\section{Group Generics}{
  \code{Rle} objects have support for Ops, Math, Math2, and Summary
  S4 group generic functionality. See \link[methods]{S4groupGeneric}
  for more details.
}

\section{Data Manipulation}{
  In the code snippets below, \code{x} is an \code{Rle} objects:

  \item{}{
    \code{!x}:
     Returns logical negation (NOT) of \code{x}.
  }
  \item{}{
    \code{mean(x, na.rm = FALSE)}:
    Calculates the mean of \code{x}. Removes \code{NA}s when
    \code{na.rm = TRUE}.
  }
  \item{}{
    \code{median(x, na.rm = FALSE)}:
    Calculates the median of \code{x}. Removes \code{NA}s when
    \code{na.rm = TRUE}.
  }
  \item{}{
    \code{var(x, na.rm = FALSE)}:
    Calculates the variance of \code{x}. Removes \code{NA}s when
    \code{na.rm = TRUE}.
  }
  \item{}{
    \code{sd(x, na.rm = FALSE)}:
    Calculates the standard deviation of \code{x}. Removes \code{NA}s
    when \code{na.rm = TRUE}.
  }
}

\author{Patrick Aboyoun}

\seealso{
  \code{\link[base]{rle}},
  \link{Sequence-class},
  \link[methods]{S4groupGeneric}
}

\examples{
  x <- Rle(10:1, 1:10)
  x
  sqrt(x)
  x^2 + 2 * x + 1
  x[c(1,3,5,7,9)]
  subseq(x, 4, 14)
  range(x)
  sum(x)
  mean(x)
  x > 4

  y <- Rle(c(TRUE,TRUE,FALSE,FALSE,TRUE,FALSE,TRUE,TRUE,TRUE))
  y
  as.vector(y)
  rep(y, 10)
  c(y, x > 5)

  Rle(letters, 1:26)
}

\keyword{methods}
\keyword{classes}
