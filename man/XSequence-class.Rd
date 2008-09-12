\name{XSequence-class}
\docType{class}

% XSequence class, functions and methods:
\alias{class:XSequence}
\alias{XSequence-class}
\alias{XSequence}

\alias{length,XSequence-method}
\alias{subseq}
\alias{subseq,XSequence-method}
\alias{subseq,vector-method}
\alias{[<-,XSequence-method}

% XRaw class, functions and methods:
\alias{class:XRaw}
\alias{XRaw-class}
\alias{XRaw}

% XInteger class, functions and methods:
\alias{class:XInteger}
\alias{XInteger-class}
\alias{XInteger}

\alias{as.integer,XInteger-method}
\alias{[,XInteger-method}
\alias{show,XInteger-method}
\alias{==,XInteger,XInteger-method}
\alias{!=,XInteger,XInteger-method}

% XNumeric class, functions and methods:
\alias{class:XNumeric}
\alias{XNumeric-class}
\alias{XNumeric}


\title{XSequence objects}

\description{
  The XSequence virtual class is a general container for storing
  an "external sequence".
  The following classes derive directly from the XSequence class.

  The XRaw class is a container for storing an external sequence
  of bytes (stored as char values at the C level).

  The XInteger class is a container for storing an external sequence
  of integer values (stored as int values at the C level).

  The XNumeric class is a container for storing an external sequence
  of numeric values (stored as double values at the C level).

  The purpose of these containers is to provide a "pass by address"
  semantic and also to avoid the overhead of copying the sequence
  data when a linear subsequence needs to be extracted.
}

\section{Subsetting}{
  In the code snippets below, \code{x} is an XSequence object.

  \describe{
    \item{}{
      \code{subseq(x, start=NA, end=NA, width=NA)}:
      Provides a very efficient way to extract a linear subsequence
      from an XSequence object.
      For example, extracting a 100Mb substring from Human chromosome 1
      (a \link[Biostrings]{DNAString} object) with \code{subseq} is almost
      instantaneous and has almost no memory footprint. In fact, the cost in
      time and memory of a call to \code{subseq} doesn't depend on the length
      of the original object or on the length of the object to extract.
      The "trick" behind this "feature" is very simple: \code{subseq}
      does NOT copy the sequence data.
    }
    \item{}{
      \code{x[i, drop=TRUE]}:
      Return a new XSequence object made of the selected elements (subscript
      \code{i} must be an NA-free numeric vector specifying the positions of
      the elements to select). The \code{drop} argument specifies whether or
      not to coerce the returned sequence to a standard vector.
    }
  }
}

\seealso{
  \link{Views-class},
  \link[Biostrings]{DNAString-class}
}

\examples{
  x1 <- XInteger(12, c(-1:10))
  x1
  length(x1)

  ## Subsetting
  x2 <- XInteger(99999, sample(99, 99999, replace=TRUE) - 50)
  x2
  subseq(x2, start=10)
  subseq(x2, start=-10)
  subseq(x2, start=-20, end=-10)
  subseq(x2, start=10, width=5)
  subseq(x2, end=10, width=5)
  subseq(x2, end=10, width=0)

  x1[length(x1):1]
  x1[length(x1):1, drop=FALSE]
}

\keyword{methods}
\keyword{classes}
