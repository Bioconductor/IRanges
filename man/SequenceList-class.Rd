\name{SequenceList}
\docType{class}

% AtomicList classes
\alias{SequenceList}
\alias{SequenceList-class}

% Additional methods
\alias{seqextract,SequenceList-method}
\alias{aggregate,SequenceList-method}


\title{SequenceList objects}
\description{
  The SequenceList virtual class is used to represent a list of Sequence
  objects.
}

\details{
  The SequenceList virtual class extends the \linkS4class{Sequence} virtual
  class.
}

\section{Subsetting}{
  In addition to the normal subsetting methods for Sequence objects,
  SequenceList object have enhanced capabilities in the following functions.

  \describe{
    \item{}{
      \code{seqextract(x, start=NULL, end=NULL, width=NULL)}:
      In addition to normal usage, the \code{start} parameter can be a
      \code{RangesList}, \code{LogicalList}, or \code{IntegerList} object
      to perform sequence extraction within the list elements rather than
      across them.
    }
  }
}

\section{Looping}{
  In addition to the normal looping methods for Sequence objects,
  SequenceList object have enhanced capabilities in the following functions.

  \describe{
    \item{}{
      \code{aggregate(x, by, FUN, start = NULL, end = NULL, width = NULL,
                      frequency = NULL, delta = NULL, ..., simplify = TRUE))}:
      In addition to normal usage, the \code{by} parameter can be a
      \code{RangesList} to aggregate within the list elements rather
      than across them.
    }
  }
}

\author{P. Aboyoun}
\seealso{\code{\linkS4class{Sequence}} for the applicable methods.}

\examples{
  showClass("SequenceList")  # shows (some of) the known subclasses
}
\keyword{methods}
\keyword{classes}
