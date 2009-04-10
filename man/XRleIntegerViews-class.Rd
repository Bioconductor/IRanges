\name{XRleIntegerViews-class}
\docType{class}

% Classes:
\alias{class:XRleIntegerViews}
\alias{XRleIntegerViews-class}
\alias{XRleIntegerViews}

% Constructors:
\alias{Views,XRleInteger-method}

% Methods:
\alias{show,XRleIntegerViews-method}
\alias{[[,XRleIntegerViews-method}
\alias{[[<-,XRleIntegerViews-method}

\alias{==,XRleIntegerViews,XRleIntegerViews-method}
\alias{==,XRleIntegerViews,XRleInteger-method}
\alias{==,XRleIntegerViews,integer-method}
\alias{==,XRleInteger,XRleIntegerViews-method}
\alias{==,integer,XRleIntegerViews-method}

\alias{!=,XRleIntegerViews,XRleIntegerViews-method}
\alias{!=,XRleIntegerViews,XRleInteger-method}
\alias{!=,XRleIntegerViews,integer-method}
\alias{!=,XRleInteger,XRleIntegerViews-method}
\alias{!=,integer,XRleIntegerViews-method}


\title{The XRleIntegerViews class}

\description{
  The XRleIntegerViews class is the basic container for storing a set of views
  (start/end locations) on the same XRleInteger object.
}

\details{
  An XRleIntegerViews object contains a set of views (start/end locations) on
  the same \link{XRleInteger} object called "the subject integer vector" or
  simply "the subject".
  Each view is defined by its start and end locations: both are integers such
  that start <= end.
  An XRleIntegerViews object is in fact a particular case of a \link{Views}
  object (the XRleIntegerViews class contains the \link{Views} class) so it
  can be manipulated in a similar manner: see \code{?\link{Views}} for
  more information.
  Note that two views can overlap and that a view can be "out of limits"
  i.e. it can start before the first element of the subject or/and end
  after its last element.
}

\section{Other methods}{
  In the code snippets below,
  \code{x}, \code{object}, \code{e1} and \code{e2} are XRleIntegerViews objects,
  and \code{i} can be a numeric or logical vector.

  \describe{
    \item{}{
      \code{x[[i]]}:
      Extract a view as an \link{XRleInteger} object.
      \code{i} must be a single numeric value (a numeric vector of length 1).
      Can't be used for extracting a view that is "out of limits" (raise an
      error). The returned object has the same \link{XRleInteger} subtype as
      \code{subject(x)}.
    }
    \item{}{
      \code{e1 == e2}:
      A vector of logicals indicating the result of the view by
      view comparison. The views in the shorter of the two XRleIntegerViews
      object being compared are recycled as necessary.
    }
    \item{}{
      \code{e1 != e2}:
      Equivalent to \code{!(e1 == e2)}.
    }
  }
}

\author{P. Aboyoun}

\seealso{
  \link{Views-class},
  \link{XRleInteger-class},
  \link{Views-utils}
}

\examples{
\dontrun{
  ## One standard way to create an XIntegerViews object is to use
  ## the Views() constructor:
  subject <- XRleInteger(rep(c(3L, 2L, 18L, 0L), c(3,2,1,5)))
  myViews <- Views(subject, 3:0, 5:8)
  myViews
  subject(myViews)
  length(myViews)
  start(myViews)
  end(myViews)
  width(myViews)
  myViews[[2]]

  ## Here the first view doesn't even overlap with the subject:
  Views(XRleInteger(as.integer(c(97, 97, 97, 45, 45, 98))), -3:4, -3:4 + c(3:6, 6:3))
}
}

\keyword{methods}
\keyword{classes}
