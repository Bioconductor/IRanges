\name{XVector-class}
\docType{class}

% XVector class, functions and methods:
\alias{class:XVector}
\alias{XVector-class}
\alias{XVector}

\alias{length,XVector-method}
\alias{c,XVector-method}
\alias{[,XVector-method}
\alias{subseq}
\alias{subseq<-}
\alias{subseq,XVector-method}
\alias{subseq<-,XVector-method}
\alias{seqselect,XVector-method}
\alias{window,XVector-method}
\alias{as.numeric,XVector-method}
\alias{show,XVector-method}
\alias{==,XVector,XVector-method}

% XRaw class, functions and methods:
\alias{class:XRaw}
\alias{XRaw-class}
\alias{XRaw}

\alias{coerce,raw,XRaw-method}
\alias{coerce,raw,XVector-method}
\alias{coerce,numeric,XRaw-method}
\alias{as.raw,XRaw-method}
\alias{as.integer,XRaw-method}
\alias{as.vector,XRaw,missing-method}

% XInteger class, functions and methods:
\alias{class:XInteger}
\alias{XInteger-class}
\alias{XInteger}

\alias{coerce,numeric,XInteger-method}
\alias{coerce,integer,XVector-method}
\alias{as.integer,XInteger-method}
\alias{as.vector,XInteger,missing-method}

% XDouble class, functions and methods:
\alias{class:XDouble}
\alias{XDouble-class}
\alias{XDouble}
\alias{XNumeric}

\alias{coerce,numeric,XDouble-method}
\alias{coerce,numeric,XVector-method}
\alias{as.numeric,XDouble-method}
\alias{as.vector,XDouble,missing-method}
\alias{show,XDouble-method}


\title{XVector objects}

\description{
  The XVector virtual class is a general container for storing
  an "external vector". It inherits from the \link{Vector},
  which has a very rich interface.

  The following classes derive directly from the XVector class:

  The XRaw class is a container for storing an "external raw vector"
  i.e. an external sequence of bytes (stored as char values at the
  C level).

  The XInteger class is a container for storing an "external integer
  vector" i.e. an external sequence of integer values (stored as
  int values at the C level).

  The XDouble class is a container for storing an "external double
  vector" i.e. an external sequence of numeric values (stored as
  double values at the C level).

  Also the \link[Biostrings:XString-class]{XString} class from
  the Biostrings package.

  The purpose of the X* containers is to provide a "pass by address"
  semantic and also to avoid the overhead of copying the sequence
  data when a linear subsequence needs to be extracted.
}

\section{Additional Subsetting operations on XVector objects}{
  In the code snippets below, \code{x} is an XVector object.

  \describe{
    \item{}{
      \code{subseq(x, start=NA, end=NA, width=NA)}:
      Extract the subsequence from \code{x} specified by \code{start},
      \code{end} and \code{width}.
      The supplied start/end/width values are solved by a call to
      \code{solveUserSEW(length(x), start=start, end=end, width=width)}
      and therefore must be compliant with the rules of the SEW
      (Start/End/Width) interface (see \code{?solveUserSEW} for the
      details).

      A note about performance: \code{subseq} does NOT copy the sequence data
      of an XVector object. Hence it's very efficient and is therefore the
      recommended way to extract a linear subsequence (i.e. a set of consecutive
      elements) from an XVector object. For example, extracting a 100Mb
      subsequence from Human chromosome 1 (a 250Mb
      \link[Biostrings:DNAString-class]{DNAString} object) with \code{subseq}
      is (almost) instantaneous and has (almost) no memory footprint (the cost
      in time and memory does not depend on the length of the original sequence
      or on the length of the subsequence to extract).
    }
    \item{}{
      \code{subseq(x, start=NA, end=NA, width=NA) <- value}:
      Replace the subsequence specified on the left (i.e. the subsequence
      in \code{x} specified by \code{start}, \code{end} and \code{width})
      by \code{value}.
      \code{value} must belong to the same class as \code{x}, or to one of
      its subclasses, or must be \code{NULL}.
      This replacement method can modify the length of \code{x}, depending
      on how the length of the left subsequence compares to the length of
      \code{value}.
      It can be used for inserting elements in \code{x} (specify an empty
      left subsequence for this) or deleting elements from \code{x} (use
      a \code{NULL} right value for this).
      Unlike the extraction method above, this replacement method always
      copies the sequence data of \code{x} (even for XVector objects).
      NOTE: Only works for XRaw (and derived) objects for now.
    }
  }
}

\author{H. Pages}

\seealso{
  \link{Vector-class},
  \link[Biostrings]{DNAString-class},
  \link{XVectorList-class},
  \link{Views-class},
  \code{\link{solveUserSEW}},
  \code{\link{compact}}
}

\examples{
  ## ---------------------------------------------------------------------
  ## A. XRaw OBJECTS
  ## ---------------------------------------------------------------------

  x1 <- XRaw(4)  # values are not initialized
  x1
  x2 <- as(c(255, 255, 199), "XRaw")
  x2
  y <- c(x1, x2, NULL, x1)  # NULLs are ignored
  y
  subseq(y, start=-4)
  subseq(y, start=-4) <- x2
  y

  ## ---------------------------------------------------------------------
  ## B. XInteger OBJECTS
  ## ---------------------------------------------------------------------

  x3 <- XInteger(12, val=c(-1:10))
  x3
  length(x3)

  ## Subsetting
  x4 <- XInteger(99999, val=sample(99, 99999, replace=TRUE) - 50)
  x4
  subseq(x4, start=10)
  subseq(x4, start=-10)
  subseq(x4, start=-20, end=-10)
  subseq(x4, start=10, width=5)
  subseq(x4, end=10, width=5)
  subseq(x4, end=10, width=0)

  x3[length(x3):1]
  x3[length(x3):1, drop=FALSE]
}

\keyword{methods}
\keyword{classes}
