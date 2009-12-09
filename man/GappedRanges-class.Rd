\name{GappedRanges-class}
\docType{class}

% Classes:
\alias{class:GappedRanges}
\alias{GappedRanges-class}
\alias{GappedRanges}

% Generics and methods:
\alias{length,GappedRanges-method}
\alias{start,GappedRanges-method}
\alias{end,GappedRanges-method}
\alias{ngap}
\alias{ngap,GappedRanges-method}
\alias{names,GappedRanges-method}
\alias{names<-,GappedRanges-method}
\alias{coerce,CompressedNormalIRangesList,GappedRanges-method}
\alias{coerce,CompressedIRangesList,GappedRanges-method}
\alias{coerce,GappedRanges,CompressedNormalIRangesList-method}
\alias{coerce,GappedRanges,NormalIRangesList-method}
\alias{coerce,GappedRanges,CompressedIRangesList-method}
\alias{coerce,GappedRanges,IRangesList-method}
\alias{coerce,GappedRanges,RangesList-method}
\alias{as.data.frame,GappedRanges-method}
\alias{show,GappedRanges-method}
\alias{[[,GappedRanges-method}
\alias{elementLengths,GappedRanges-method}
\alias{[,GappedRanges-method}
\alias{seqselect,GappedRanges-method}
\alias{window,GappedRanges-method}
\alias{c,GappedRanges-method}


\title{GappedRanges objects}

\description{
  The GappedRanges class is a vector-like container for storing a set
  of "gapped ranges".
}

\details{
  A "gapped range" is conceptually the union of 1 or more non-overlapping
  (and non-empty) ranges ordered from left to right.
  More precisely, a "gapped range" can be represented by a normal IRanges
  object of length >= 1. In particular normality here ensures that the
  individual ranges are non-empty and are separated by non-empty gaps.
  The start of a "gapped range" is the start of its first range.
  The end of a "gapped range" is the end of its last range.
  If we ignore the gaps, then a GappedRanges object can be seen as a
  \link{Ranges} object.
}

\section{Constructor}{
  No constructor function is provided for GappedRanges objects.
  The coercion methods described below can be used to create GappedRanges
  objects.
}

\section{Coercion}{
  \describe{
    \item{}{
      \code{as(from, "GappedRanges")}:
      Turns a \link{CompressedNormalIRangesList} or \link{CompressedIRangesList}
      object into a GappedRanges object.
    }
    \item{}{
      \code{as(from, "RangesList")}:
      Turns a GappedRanges object into a \link{RangesList} object (more precisely
      the result will be a \link{CompressedNormalIRangesList} object).
    }
  }
}

\section{Accessor methods}{
  In the code snippets below, \code{x} is a GappedRanges object.

  \describe{
    \item{}{
      \code{length(x)}:
      Returns the number of "gapped ranges" in \code{x}.
    }
    \item{}{
      \code{start(x), end(x)}:
      Returns an integer vector of length \code{length(x)}
      containing the start and end (respectively) of each
      "gapped range" in \code{x}. See Details section above for
      the exact definitions of the start and end of a "gapped range".
    }
    \item{}{
      \code{width(x)}:
      Defined as \code{end(x) - start(x) + 1L}.
    }
    \item{}{
      \code{ngap(x)}:
      Returns an integer vector of length \code{length(x)}
      containing the number of gaps for each "gapped range"
      in \code{x}.
      Equivalent to \code{elementLengths(x) - 1L}.
    }
    \item{}{
      \code{names(x)}:
      \code{NULL} or a character vector of length \code{length(x)}.
    }
  }
}

\section{Subsetting and related operations}{
  In the code snippets below, \code{x} is a GappedRanges object.

  \describe{
    \item{}{
      \code{x[i]}:
      Returns a new GappedRanges object made of the selected "gapped ranges".
      \code{i} can be a numeric, character or logical vector,
      or any of the types supported by the \code{[} method for
      \link{CompressedNormalIRangesList} objects.
    }
    \item{}{
      \code{x[[i]]}:
      Returns the \link{NormalIRanges} object representing the i-th
      element in \code{x}.
      Equivalent to \code{as(from, "RangesList")[[i]]}.
      \code{i} can be a single numeric value or a single character string.
    }
    \item{}{
      \code{elemenType(x)}:
      Returns the type of \code{x[[i]]} as a single string (always
      \code{"NormalIRanges"}).
      Note that the semantic of the \code{[[} method for GappedRanges
      objects is different from the semantic of the method for
      \link{Ranges} objects (the latter returns an integer vector).
    }
    \item{}{
      \code{elementLengths(x)}:
      Semantically equivalent to
      \preformatted{sapply(seq_len(length(x)), function(i) length(x[[i]]))}
      but much faster.
      Note that the semantic of the \code{elementLengths} method for
      GappedRanges objects is different from the semantic of the method
      for \link{Ranges} objects (the latter returns the \code{width} of
      the \link{Ranges} object).
    }
  }
}

\section{Combining and related operations}{
  In the code snippets below, \code{x} is a GappedRanges object.

  \describe{
    \item{}{
      \code{c(x, ...)}:
      Combine \code{x} and the GappedRanges objects in \code{...} together.
      The result is an object of the same class as \code{x}.
    }
  }
}

\author{H. Pages}

\seealso{
  \link{Ranges-class},
  \link{CompressedNormalIRangesList-class}
}

\examples{
  ## The 3 following IRanges objects are normal. Each of them will be
  ## stored as a "gapped range" in the GappedRanges object 'gr'.
  ir1 <- IRanges(start=c(11, 21, 23), end=c(15, 21, 30))
  ir2 <- IRanges(start=-2, end=15)
  ir3 <- IRanges(start=c(-2, 21), end=c(10, 22))
  irl <- IRangesList(ir1, ir2, ir3)

  gr <- as(irl, "GappedRanges")
  gr

  length(gr)
  start(gr)
  end(gr)
  width(gr)
  ngap(gr)
  gr[-1]
  gr[ngap(gr) >= 1]
  gr[[1]]
  as.integer(gr[[1]])
  gr[[2]]
  as.integer(gr[[2]])
  as(gr, "RangesList")
  start(as(gr, "RangesList"))  # not the same as 'start(gr)'
}

\keyword{methods}
\keyword{classes}
