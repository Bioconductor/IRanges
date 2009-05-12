\name{Sequence-class}
\docType{class}

% Sequence class, functions and methods:
\alias{class:Sequence}
\alias{Sequence-class}
\alias{Sequence}

\alias{subseq}
\alias{subseq<-}
\alias{subseq,Sequence-method}
\alias{subseq,vector-method}
\alias{subseq<-,ANY-method}
\alias{seqextract}
\alias{seqextract,Sequence-method}
\alias{seqextract,vector-method}
\alias{[,Sequence-method}
\alias{[<-,Sequence-method}
\alias{c,Sequence-method}
\alias{head,Sequence-method}
\alias{length,Sequence-method}
\alias{rep,Sequence-method}
\alias{rev,Sequence-method}
\alias{tail,Sequence-method}
\alias{window,Sequence-method}
\alias{!=,Sequence,Sequence-method}
\alias{aggregate,Sequence-method}

% XSequence class, functions and methods:
\alias{class:XSequence}
\alias{XSequence-class}
\alias{XSequence}

\alias{length,XSequence-method}
\alias{subseq,XSequence-method}
\alias{seqextract,XSequence-method}
\alias{as.numeric,XSequence-method}
\alias{show,XSequence-method}

% XRaw class, functions and methods:
\alias{class:XRaw}
\alias{XRaw-class}
\alias{XRaw}

\alias{coerce,raw,XRaw-method}
\alias{coerce,raw,XSequence-method}
\alias{coerce,numeric,XRaw-method}
\alias{as.raw,XRaw-method}
\alias{as.integer,XRaw-method}
\alias{as.vector,XRaw,missing-method}
\alias{[,XRaw-method}
\alias{c,XRaw-method}

% XInteger class, functions and methods:
\alias{class:XInteger}
\alias{XInteger-class}
\alias{XInteger}

\alias{coerce,numeric,XInteger-method}
\alias{coerce,integer,XSequence-method}
\alias{as.integer,XInteger-method}
\alias{as.vector,XInteger,missing-method}
\alias{[,XInteger-method}
\alias{==,XInteger,XInteger-method}

% XNumeric class, functions and methods:
\alias{class:XNumeric}
\alias{XNumeric-class}
\alias{XNumeric}

\alias{coerce,numeric,XNumeric-method}
\alias{coerce,numeric,XSequence-method}
\alias{as.numeric,XNumeric-method}
\alias{as.vector,XNumeric,missing-method}
\alias{[,XNumeric-method}
\alias{show,XNumeric-method}
\alias{==,XNumeric,XNumeric-method}


\title{Sequence objects}

\description{
  The Sequence virtual class is a general container for storing
  a sequence i.e. an ordered set of elements. These containers
  come in two types:  XSequence and Rle.

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

  The purpose of the X* containers is to provide a "pass by address"
  semantic and also to avoid the overhead of copying the sequence
  data when a linear subsequence needs to be extracted.

  For information on the Rle class, type \code{help(Rle)}.
}

\section{Combining}{
  In the code snippets below, \code{x} is a Sequence object.

  \describe{
    \item{}{
      \code{c(x, ...)}:
      Combine \code{x} and the Sequence objects in \code{...} together.
      Any object in \code{...} must belong to the same class as \code{x},
      or to one of its subclasses, or must be \code{NULL}.
      The result is an object of the same class as \code{x}.
      NOTE: Only works for XRaw (and derived) objects for now.
    }
  }
}

\section{Subsetting}{
  In the code snippets below, \code{x} is a Sequence object or regular R vector
  object. The R vector object methods for \code{subseq} and \code{seqextract}
  are defined in this package and the remaining methods are defined in base R.

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
      a \code{NULL} rigth value for this).
      Unlike the extraction method above, this replacement method always
      copies the sequence data of \code{x} (even for XSequence objects).
      NOTE: Only works for XRaw (and derived) objects for now.
    }
    \item{}{
      \code{seqextract(x, start=NULL, end=NULL, width=NULL)}:
      Like \code{subseq}, except that multiple subsequences can be requested.
      The requested subsequences are concatenated. If the concatenation is
      undesirable, consider using \code{\link{Views}}.

      Note that, unlike with \code{subseq}, the sequence data are always
      copied, even for an XSequence object.
    }
    \item{}{
      \code{x[i, drop=TRUE]}:
      Return a new Sequence object made of the selected elements (subscript
      \code{i} must be an NA-free numeric vector specifying the positions of
      the elements to select). The \code{drop} argument specifies whether or
      not to coerce the returned sequence to a standard vector.
    }
    \item{}{
      \code{head(x, n = 6L)}:
      If \code{n} is non-negative, returns the first n elements of the Sequence
      object.
      If \code{n} is negative, returns all but the last \code{abs(n)} elements
      of the Sequence object.
    }
    \item{}{
      \code{length(x)}:
      Return the number of elements in the Sequence object.
    }
    \item{}{
      \code{rep(x, times)}:
      Return a new Sequence object made of the repeated elements.
    }
    \item{}{
      \code{rev(x)}:
      Return a new Sequence object made of the original elements in the reverse
      order.
    }
    \item{}{
      \code{tail(x, n = 6L)}:
      If \code{n} is non-negative, returns the last n elements of the Sequence
      object.
      If \code{n} is negative, returns all but the first \code{abs(n)} elements
      of the Sequence object.
    }
    \item{}{
      \code{window(x, start = NULL, end = NULL, width = NULL, frequency = NULL, delta = NULL, ...)}:
      Extract the subsequence window from the Sequence object using:
      \describe{
        \item{\code{start}, \code{end}, \code{width}}{The start, end, or width
          of the window. Two of the three are required.}
        \item{\code{frequency}, \code{delta}}{Optional arguments that specify
          the sampling frequency and increment within the window.}
      }
    }
  }
}

\section{Looping}{
  In the code snippets below, \code{x} is a Sequence object.

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
  \link{Rle-class},
  \link{Views-class},
  \code{\link{solveUserSEW}},
  \link[Biostrings]{DNAString-class}
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

  x5 <- LETTERS
  subseq(x5, 8, 11) <- x5[11:8]  # swap 8<->11 and 9<->10 elements
  subseq(x5, start=1, width=0) <- c("xx", "yyy")  # insert 2 elements at the beginning
  subseq(x5, end=-1, width=0) <- letters[1:3]  # insert 3 elements at the end
  subseq(x5, end=-4, width=5) <- NULL  # remove 5 elements before the just added ones
}

\keyword{methods}
\keyword{classes}
