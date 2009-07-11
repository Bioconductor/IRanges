\name{SimpleList-class}
\docType{class}
\alias{SimpleList}
\alias{SimpleList-class}
\alias{CompressedList}
\alias{CompressedList-class}

% accessors
\alias{isEmpty,CompressedList-method}
\alias{isEmpty,SimpleList-method}
\alias{length,CompressedList-method}
\alias{length,SimpleList-method}
\alias{names,CompressedList-method}
\alias{names,SimpleList-method}
\alias{names<-,CompressedList-method}
\alias{names<-,SimpleList-method}

% subsetting
\alias{[,CompressedList-method}
\alias{[,SimpleList-method}
\alias{[[,CompressedList-method}
\alias{[[,SimpleList-method}
\alias{[[<-,CompressedList-method}
\alias{[[<-,SimpleList-method}
\alias{$<-,CompressedList-method}
\alias{$<-,SimpleList-method}

% splitting and combining
\alias{c,CompressedList-method}
\alias{c,SimpleList-method}

% looping
\alias{endomorph,CompressedList-method}
\alias{endomorph,SimpleList-method}
\alias{lapply,CompressedList-method}
\alias{lapply,SimpleList-method}

% coercion
\alias{as.list,CompressedList-method}
\alias{as.list,SimpleList-method}
\alias{unlist,CompressedList-method}
\alias{unlist,SimpleList-method}

% show
\alias{show,CompressedList-method}
\alias{show,SimpleList-method}


\title{Simple and Compressed List Classes}
\description{The (non-virtual) SimpleList and (virtual) CompressedList classes
  extend the \linkS4class{Sequence} virtual class.}

\details{
  The SimpleList and CompressedList classes provide an implementation that
  subclasses can easily extend. The underlying storage in a SimpleList
  subclass is a list object. The underlying storage in a CompressedList
  object is a virtually partitioned vector-like object. For more information
  on the available methods, see the \linkS4class{Sequence} man page.
}
\section{Constructor}{
  The SimpleList class constructor is used to create SimpleList objects:
  
  \describe{
    \item{}{\code{SimpleList(...)}: takes possibly named objects as elements
    for the new SimpleList object.
    }
  }
}

\author{P. Aboyoun}
\seealso{\linkS4class{Sequence},
         \linkS4class{IntegerList} and
         \linkS4class{RangesList} for example implementations}

\examples{
SimpleList(a = letters, ranges = IRanges(1:10, 1:10))
}
\keyword{methods}
\keyword{classes}
