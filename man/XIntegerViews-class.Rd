\name{XIntegerViews-class}
\docType{class}

% Classes:
\alias{class:XIntegerViews}
\alias{XIntegerViews-class}
\alias{XIntegerViews}

% Constructors:
\alias{Views,XInteger-method}
\alias{Views,integer-method}

% Methods:
\alias{show,XIntegerViews-method}

\alias{==,XIntegerViews,XIntegerViews-method}
\alias{==,XIntegerViews,XInteger-method}
\alias{==,XIntegerViews,integer-method}
\alias{==,XInteger,XIntegerViews-method}
\alias{==,integer,XIntegerViews-method}


\title{The XIntegerViews class}

\description{
  The XIntegerViews class is the basic container for storing a set of views
  (start/end locations) on the same XInteger object.
}

\details{
  An XIntegerViews object contains a set of views (start/end locations) on the
  same \link{XInteger} object called "the subject integer vector" or simply
  "the subject".
  Each view is defined by its start and end locations: both are integers such
  that start <= end.
  An XIntegerViews object is in fact a particular case of a \link{Views}
  object (the XIntegerViews class contains the \link{Views} class) so it
  can be manipulated in a similar manner: see \code{?\link{Views}} for
  more information.
  Note that two views can overlap and that a view can be "out of limits"
  i.e. it can start before the first element of the subject or/and end
  after its last element.
}

\section{Other methods}{
  In the code snippets below,
  \code{x}, \code{object}, \code{e1} and \code{e2} are XIntegerViews objects,
  and \code{i} can be a numeric or logical vector.

  \describe{
    \item{}{
      \code{x[[i]]}:
      Extract a view as an \link{XInteger} object.
      \code{i} must be a single numeric value (a numeric vector of length 1).
      Can't be used for extracting a view that is "out of limits" (raise an
      error). The returned object has the same \link{XInteger} subtype as
      \code{subject(x)}.
    }
    \item{}{
      \code{e1 == e2}:
      A vector of logicals indicating the result of the view by
      view comparison. The views in the shorter of the two XIntegerViews
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
  \link{XInteger-class},
  \link{Views-utils},
  \link{XDoubleViews-class}
}

\examples{
  ## One standard way to create an XIntegerViews object is to use
  ## the Views() constructor:
  subject <- as(c(45, 67, 84, 67, 45, 78), "XInteger")
  v4 <- Views(subject, start=3:0, end=5:8)
  v4
  subject(v4)
  length(v4)
  start(v4)
  end(v4)
  width(v4)

  ## Attach a comment to views #3 and #4:
  names(v4)[3:4] <- "out of limits"
  names(v4)

  ## A more programatical way to "tag" the "out of limits" views:
  idx <- start(v4) < 1 | end(v4) > length(subject(v4)) 
  names(v4)[idx] <- "out of limits"

  ## Extract a view as an XInteger object:
  v4[[2]]

  ## It is an error to try to extract an "out of limits" view:
\dontrun{
  v4[[3]] # Error!
}

  ## Here the first view doesn't even overlap with the subject:
  subject <- as(c(97, 97, 97, 45, 45, 98), "XInteger")
  Views(subject, start=-3:4, end=-3:4 + c(3:6, 6:3))
}

\keyword{methods}
\keyword{classes}
