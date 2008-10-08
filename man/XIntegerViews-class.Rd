\name{XIntegerViews-class}
\docType{class}

% Classes:
\alias{class:XIntegerViews}
\alias{XIntegerViews-class}
\alias{XIntegerViews}

% Constructors:
\alias{Views,integer-method}

% Methods:
\alias{show,XIntegerViews-method}
\alias{[[,XIntegerViews-method}
\alias{[[<-,XIntegerViews-method}

\alias{==,XIntegerViews,XIntegerViews-method}
\alias{==,XIntegerViews,XInteger-method}
\alias{==,XIntegerViews,integer-method}
\alias{==,XInteger,XIntegerViews-method}
\alias{==,integer,XIntegerViews-method}

\alias{!=,XIntegerViews,XIntegerViews-method}
\alias{!=,XIntegerViews,XInteger-method}
\alias{!=,XIntegerViews,integer-method}
\alias{!=,XInteger,XIntegerViews-method}
\alias{!=,integer,XIntegerViews-method}

\alias{as.integer,XIntegerViews-method}
\alias{as.list,XIntegerViews-method}


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
    \item{}{
      \code{as.integer(x, use.names, check.limits)}:
      Convert \code{x} to a list of integer vectors of the same length as
      \code{x}.
      Can't be used if \code{x} has "out of limits" views (raise an error).
    }
    \item{}{
      \code{as.list(x)}:
      Convert \code{x} to a list of \link{XInteger} objects.
      Can't be used if \code{x} has "out of limits" views (raise an error).
    }
  }
}

\author{P. Aboyoun}

\seealso{
  \link{Views-class},
  \link{XInteger-class},
  \link{XIntegerViews-utils}
}

\examples{
  ## One standard way to create an XIntegerViews object is to use
  ## the Views() constructor:
  subject <- XInteger(6, c(45, 67, 84, 67, 45, 78))
  v4 <- Views(subject, 3:0, 5:8)
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
  names(v4)[start(v4) < 1 | length(subject(v4)) < end(v4)] <- "out of limits"
  ## or just:
  names(v4)[length(subject(v4)) < width(v4)] <- "out of limits"

  ## Extract a view as an XInteger object:
  v4[[2]]

  ## It is an error to try to extract an "out of limits" view:
  #v4[[3]] # Error!

  ## Here the first view doesn't even overlap with the subject:
  Views(XInteger(6, c(97, 97, 97, 45, 45, 98)), -3:4, -3:4 + c(3:6, 6:3))
}

\keyword{methods}
\keyword{classes}
