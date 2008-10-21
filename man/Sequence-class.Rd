\name{Sequence-class}
\docType{class}

% Sequence class, functions and methods:
\alias{class:Sequence}
\alias{Sequence-class}
\alias{Sequence}
\alias{class:SequenceORvector}
\alias{SequenceORvector-class}
\alias{SequenceORvector}

\alias{subseq}
\alias{subseq,SequenceORvector-method}
\alias{rep,Sequence-method}
\alias{[<-,Sequence,ANY,ANY,ANY-method}
\alias{!=,Sequence,Sequence-method}

% XSequence class, functions and methods:
\alias{class:XSequence}
\alias{XSequence-class}
\alias{XSequence}

\alias{length,XSequence-method}
\alias{subseq,XSequence-method}

\alias{coerce,integer,XSequence-method}
\alias{coerce,numeric,XSequence-method}

% XRaw class, functions and methods:
\alias{class:XRaw}
\alias{XRaw-class}
\alias{XRaw}

% XInteger class, functions and methods:
\alias{class:XInteger}
\alias{XInteger-class}
\alias{XInteger}

\alias{as.integer,XInteger-method}
\alias{as.vector,XInteger,missing-method}
\alias{coerce,integer,XInteger-method}
\alias{[,XInteger,ANY,ANY,ANY-method}
\alias{show,XInteger-method}
\alias{==,XInteger,XInteger-method}

% XNumeric class, functions and methods:
\alias{class:XNumeric}
\alias{XNumeric-class}
\alias{XNumeric}

\alias{as.numeric,XNumeric-method}
\alias{as.vector,XNumeric,missing-method}
\alias{coerce,numeric,XNumeric-method}
\alias{[,XNumeric,ANY,ANY,ANY-method}
\alias{show,XNumeric-method}
\alias{==,XNumeric,XNumeric-method}


% XRle class, functions and methods:
\alias{class:XRle}
\alias{XRle-class}
\alias{XRle}

\alias{length,XRle-method}
\alias{rep,XRle-method}
\alias{reverse,XRle-method}
\alias{==,XRle,XRle-method}
\alias{[,XRle,ANY,ANY,ANY-method}

% XRleInteger class, function and methods:
\alias{class:XRleInteger}
\alias{XRleInteger-class}
\alias{XRleInteger}

\alias{Arith,integer,XRleInteger-method}
\alias{Arith,XRleInteger,integer-method}
\alias{Arith,XRleInteger,XRleInteger-method}
\alias{as.integer,XRleInteger-method}
\alias{as.vector,XRleInteger,missing-method}
\alias{coerce,integer,XRleInteger-method}
\alias{show,XRleInteger-method}


\title{Sequence objects}

\description{
  The Sequence virtual class is a general container for storing
  a sequence i.e. an ordered set of elements. These containers
  come in two types:  XSequence and XRle.

  The XSequence virtual class is a general container for storing
  an "external sequence".
  The following classes derive directly from the XSequence class.

  The XRaw class is a container for storing an external sequence
  of bytes (stored as char values at the C level).

  The XInteger class is a container for storing an external sequence
  of integer values (stored as int values at the C level).

  The XNumeric class is a container for storing an external sequence
  of numeric values (stored as double values at the C level).

  Also the \link[Biostrings]{XString} class from the Biostrings package
  
  The XRle virtual class is a general container for storing
  an "external sequence" that is stored in a run-time encoding format.
  The following classes derive directly from the XRle class.

  The XRle class is a container for storing an external run-time encoding
  of integers (stored as char values at the C level).

  The purpose of these containers is to provide a "pass by address"
  semantic and also to avoid the overhead of copying the sequence
  data when a linear subsequence needs to be extracted.
}

\section{Subsetting}{
  In the code snippets below, \code{x} is a Sequence object.

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
      of an XSequence object. Hence it's very efficient and is therefore the
      recommended way to extract a linear subsequence (i.e. a set of consecutive
      elements) from an XSequence object. For example, extracting a 100Mb
      subsequence from Human chromosome 1 (a 250Mb \link[Biostrings]{DNAString}
      object) with \code{subseq} is (almost) instantaneous and has (almost) no
      memory footprint (the cost in time and memory does not depend on the
      length of the original sequence or on the length of the subsequence to
      extract).
    }
    \item{}{
      \code{x[i, drop=TRUE]}:
      Return a new Sequence object made of the selected elements (subscript
      \code{i} must be an NA-free numeric vector specifying the positions of
      the elements to select). The \code{drop} argument specifies whether or
      not to coerce the returned sequence to a standard vector.
    }
    \item{}{
      \code{rep(x, times)}:
      Return a new Sequence object made of the repeated elements.
    }
  }
}

\seealso{
  \link{Views-class},
  \code{\link{solveUserSEW}},
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
