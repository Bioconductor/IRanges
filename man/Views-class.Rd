\name{Views-class}

\alias{class:Views}
\alias{Views-class}
\alias{subject}
\alias{subject,Views-method}
\alias{width<-,Views-method}
\alias{initialize,Views-method}
\alias{Views}
\alias{Views,ANY-method}
\alias{views}
\alias{coerce,Views,NormalIRanges-method}
\alias{restrict,Views-method}
\alias{trim}
\alias{trim,Views-method}
\alias{narrow,Views-method}
\alias{subviews}
\alias{subviews,Views-method}
\alias{gaps,Views-method}


\title{Views objects}

\description{
  The Views class is a general container for representing a set of
  views on an arbitrary object, called the "subject", and for which there
  is a notion of length. For example it can be used to store views on a
  vector or an external vector.
  Note that more specific containers will typically be defined on top of
  this container by extending the Views class.
  The primary purpose of the Views container is to introduce concepts
  and provide some facilities shared by the more specific containers.
}

\section{Accesor methods}{
  In the code snippets below, \code{x} is a Views object.

  \describe{
    \item{}{
      \code{subject(x)}:
      The subject of \code{x}.
    }
  }
}

\section{Constructor}{
  \describe{
    \item{}{
      \code{Views(subject, start=NA, end=NA, names=NULL)}:
      Return the Views object containing the views on \code{subject}
      specified by the \code{start} and \code{end} integer vectors.
    }
  }
}

\section{Other methods}{
  In the code snippets below, \code{x} is a Views object.

  \describe{
    \item{}{
      \code{trim(x, use.names=TRUE)}:
      [TODO]
    }
    \item{}{
      \code{subviews(x, start=NA, end=NA, width=NA, use.names=TRUE)}:
      [TODO]
    }
  }
}

\seealso{
  \link{IRanges-class},
  \link{IRanges-utils},
  \link{XIntegerViews-class},
  \link[Biostrings]{XStringViews-class}
}

\examples{
  ## This creates a set of 4 views on a logical vector of length 10:
  v1 <- Views(logical(10), start=4:1, end=4:7)

  ## 'start' and 'end' are recycled
  Views(logical(10), 2:1, 4)
  Views(logical(10), 5:7, )
  Views(logical(10), , 5:7)

  ## Some views can be "out of limits"
  v2 <- Views(logical(10), 4:-1, 6)
  trim(v2)
  subviews(v2, end=-2)

  ## gaps() 
  v3 <- Views(logical(10), start=c(8, 3), end=c(14, 4))
  gaps(v3)
}

\keyword{methods}
