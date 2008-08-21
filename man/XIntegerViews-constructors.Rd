\name{XIntegerViews-constructors}

% Constructor-like functions and generics:
\alias{XIntegerViews-constructors}
\alias{views}
\alias{views,integer-method}
\alias{views,XInteger-method}
\alias{XIntegerViews}
\alias{XIntegerViews,ANY-method}
\alias{XIntegerViews,file-method}
\alias{XIntegerViews,XInteger-method}
\alias{XIntegerViews,XIntegerViews-method}

% Other functions:
\alias{restrict,XIntegerViews-method}
\alias{trim}
\alias{trim,XIntegerViews-method}
\alias{narrow,XIntegerViews-method}
\alias{subviews}
\alias{subviews,XIntegerViews-method}
\alias{gaps,XIntegerViews-method}


\title{Basic functions for creating or modifying XIntegerViews objects}

\description{
  A set of basic functions for creating or modifying XIntegerViews objects.
}

\usage{
  views(subject, start=NA, end=NA)
  XIntegerViews(x, start=NA, end=NA)
  trim(x, use.names=TRUE)
  subviews(x, start=NA, end=NA, width=NA, use.names=TRUE)
}

\arguments{
  \item{subject}{
    An \link{XInteger} object or an integer vector.
  }
  \item{start}{
    An integer vector containing the start positions of the views.
  }
  \item{end}{
    An integer vector containing the end positions of the views.
  }
  \item{width}{
    An integer vector containing the widths of the views.
  }
  \item{x}{
    An \link{XInteger} object or an integer vector for \code{XIntegerViews}.
    An XIntegerViews object for \code{trim} and \code{subviews}.
  }
  \item{use.names}{
    \code{TRUE} or \code{FALSE}. Should names be preserved?
  }
}

\details{
  The \code{views} function returns an XIntegerViews object
  containing views on \code{subject} with start and end positions
  given in the \code{start} and \code{end} vectors.

  The \code{XIntegerViews} constructor will try to create an XIntegerViews
  object from the value passed to its \code{x} argument.
  If \code{x} is an \link{XInteger} object or an integer vector, the returned
  object is made of a single view that starts at the first element and ends at
  the last element of \code{x}.
}

\value{
  All these functions return an XIntegerViews object \code{y} with the following
  number of views (\code{length(y)}):
  \code{max(length(start), length(end)} for the \code{views} function.
  For the \code{XIntegerViews} constructor, \code{length(y)} is \code{1}.
}

\seealso{
  \link{XIntegerViews-class},
  \link{XInteger-class},
  \code{\link{gaps}}
}

\examples{
  ## 'start' and 'end' are recycled
  views(1:10, 2:1, 4)
  views(1:10, 5:7, )
  views(1:10, , 5:7)

  ## Views can be "out of limits"
  views(1:10, 2:0, 4)

  ## Applying gaps() to an XStringViews object
  v2 <- views(1:10, start=c(8, 3), end=c(14, 4))
  gaps(v2)
}

\keyword{methods}
