\name{RleViews-class}
\docType{class}

% Classes:
\alias{class:RleViews}
\alias{RleViews-class}
\alias{RleViews}

% Constructors:
\alias{Views,Rle-method}

% Methods:
\alias{show,RleViews-method}

% coercion
\alias{coerce,AtomicList,RleViews-method}

\title{The RleViews class}

\description{
  The RleViews class is the basic container for storing a set of views
  (start/end locations) on the same Rle object.
}

\details{
  An RleViews object contains a set of views (start/end locations) on the same
  \link{Rle} object called "the subject vector" or simply "the subject".
  Each view is defined by its start and end locations: both are integers such
  that start <= end.
  An RleViews object is in fact a particular case of a \link{Views}
  object (the RleViews class contains the \link{Views} class) so it
  can be manipulated in a similar manner: see \code{?\link{Views}} for
  more information.
  Note that two views can overlap and that a view can be "out of limits"
  i.e. it can start before the first element of the subject or/and end
  after its last element.
}

\author{P. Aboyoun}

\seealso{
  \link{Views-class},
  \link{Rle-class},
  \link{Views-utils}
}

\examples{
  subject <- Rle(rep(c(3L, 2L, 18L, 0L), c(3,2,1,5)))
  myViews <- Views(subject, 3:0, 5:8)
  myViews
  subject(myViews)
  length(myViews)
  start(myViews)
  end(myViews)
  width(myViews)
  myViews[[2]]

  set.seed(0)
  vec <- Rle(sample(0:2, 20, replace = TRUE))
  vec
  Views(vec, vec > 0)
}

\keyword{methods}
\keyword{classes}
