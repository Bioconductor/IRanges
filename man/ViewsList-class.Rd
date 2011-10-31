\name{ViewsList-class}
\docType{class}
\alias{class:ViewsList}
\alias{ViewsList-class}
\alias{ViewsList}
\alias{class:SimpleViewsList}
\alias{SimpleViewsList-class}
\alias{SimpleViewsList}

% accessors
\alias{ranges,SimpleViewsList-method}
\alias{start,SimpleViewsList-method}
\alias{end,SimpleViewsList-method}
\alias{width,SimpleViewsList-method}
\alias{universe,ViewsList-method}

% overlaps
\alias{findOverlaps,ViewsList,ViewsList-method}
\alias{findOverlaps,ANY,ViewsList-method}
\alias{findOverlaps,ViewsList,ANY-method}
\alias{countOverlaps,ViewsList,ViewsList-method}
\alias{countOverlaps,ANY,ViewsList-method}
\alias{countOverlaps,ViewsList,ANY-method}
\alias{match,ViewsList,ViewsList-method}
\alias{match,ANY,ViewsList-method}
\alias{match,ViewsList,ViewsList-method}
\alias{%in%,ViewsList,ViewsList-method}
\alias{%in%,ANY,ViewsList-method}
\alias{%in%,ViewsList,ANY-method}


\title{List of Views}
\description{An extension of \linkS4class{List} that holds only
  \linkS4class{Views} objects.
}

\details{
  ViewsList is a virtual class.
  Specialized subclasses like e.g. \linkS4class{RleViewsList} are useful
  for storing coverage vectors over a set of spaces (e.g. chromosomes),
  each of which requires a separate \linkS4class{RleViews} object.

  As a \linkS4class{Vector} subclass, ViewsList may be annotated with its
  universe identifier (e.g. a genome) in which all of its spaces exist.

  As a \linkS4class{List} subclass, ViewsList inherits all the methods
  available for \linkS4class{List} objects.
}

\author{P. Aboyoun and H. Pages}

\seealso{
  \link{List-class},
  \link{RleViewsList-class}
}

\examples{
  showClass("ViewsList")
}

\keyword{methods}
\keyword{classes}
