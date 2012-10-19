\name{XDoubleViews-class}
\docType{class}

% Classes:
\alias{class:XDoubleViews}
\alias{XDoubleViews-class}
\alias{XDoubleViews}

% Constructors:
\alias{Views,XDouble-method}
\alias{Views,numeric-method}

% Methods:
\alias{show,XDoubleViews-method}

\alias{==,XDoubleViews,XDoubleViews-method}
\alias{==,XDoubleViews,XDouble-method}
\alias{==,XDoubleViews,numeric-method}
\alias{==,XDouble,XDoubleViews-method}
\alias{==,numeric,XDoubleViews-method}


\title{The XDoubleViews class}

\description{
  The XDoubleViews class is the basic container for storing a set of views
  (start/end locations) on the same XDouble object.
}

\details{
  An XDoubleViews object contains a set of views (start/end locations) on the
  same \link{XDouble} object called "the subject numeric vector" or simply
  "the subject".
  Each view is defined by its start and end locations: both are integers such
  that start <= end.
  An XDoubleViews object is in fact a particular case of a \link{Views}
  object (the XDoubleViews class contains the \link{Views} class) so it
  can be manipulated in a similar manner: see \code{?\link{Views}} for
  more information.
  Note that two views can overlap and that a view can be "out of limits"
  i.e. it can start before the first element of the subject or/and end
  after its last element.
}

\section{Other methods}{
  In the code snippets below,
  \code{x}, \code{object}, \code{e1} and \code{e2} are XDoubleViews objects,
  and \code{i} can be a numeric or logical vector.

  \describe{
    \item{}{
      \code{x[[i]]}:
      Extract a view as an \link{XDouble} object.
      \code{i} must be a single numeric value (a numeric vector of length 1).
      Can't be used for extracting a view that is "out of limits" (raise an
      error). The returned object has the same \link{XDouble} subtype as
      \code{subject(x)}.
    }
    \item{}{
      \code{e1 == e2}:
      A vector of logicals indicating the result of the view by
      view comparison. The views in the shorter of the two XDoubleViews
      object being compared are recycled as necessary.
    }
    \item{}{
      \code{e1 != e2}:
      Equivalent to \code{!(e1 == e2)}.
    }
  }
}

\author{
  P. Aboyoun for the \code{XIntegerViews*} code, which was adapted to work
  over \code{XDouble}'s by S. Lianoglou
}

\seealso{
  \link{Views-class},
  \link{XDouble-class},
  \link{Views-utils},
  \link{XIntegerViews-class}
}

\examples{
  ## One standard way to create an XDoubleViews object is to use
  ## the Views() constructor:
  subject <- as(rnorm(6), "XDouble")
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

  ## Extract a view as an XDouble object:
  v4[[2]]

  ## It is an error to try to extract an "out of limits" view:
\dontrun{
  v4[[3]] # Error!
}

  ## Here the first view doesn't even overlap with the subject:
  subject <- as(c(97, 97, 97, 45, 45, 98), "XDouble")
  Views(subject, start=-3:4, end=-3:4 + c(3:6, 6:3))

  ## Some fast view* functionalities:
  x <- rnorm(55)
  bounds <- IRanges(c(1, 11, 35, 20), width=c(5, 10, 15, 28))
  v <- Views(x, bounds)
  val <- viewMins(v)
  expect <- sapply(1:length(bounds), function(i) {
    min(x[start(bounds)[i]:end(bounds[i])])
  })
  stopifnot(all(val == expect))
}

\keyword{methods}
\keyword{classes}
