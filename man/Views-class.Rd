\name{Views-class}

\alias{class:Views}
\alias{Views-class}
\alias{subject}
\alias{subject,Views-method}
\alias{width<-,Views-method}
\alias{Views}
\alias{Views,ANY-method}
\alias{views}
\alias{[[,Views-method}
\alias{[[<-,Views-method}
\alias{coerce,Views,NormalIRanges-method}
\alias{restrict,Views-method}
\alias{trim}
\alias{trim,Views-method}
\alias{narrow,Views-method}
\alias{subviews}
\alias{subviews,Views-method}
\alias{gaps,Views-method}
\alias{successiveViews}


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

\usage{
## Constructor:

  Views(subject, start=NA, end=NA, names=NULL)

## Accessor:

  subject(x)

## View extraction:

  \S4method{[[}{Views}(x, i, j, ...)

## Utilities:

  \S4method{restrict}{Views}(x, start, end, keep.all.ranges=FALSE, use.names=TRUE)

  trim(x, use.names=TRUE)

  \S4method{narrow}{Views}(x, start=NA, end=NA, width=NA, use.names=TRUE)

  subviews(x, start=NA, end=NA, width=NA, use.names=TRUE)

  \S4method{gaps}{Views}(x, start=NA, end=NA)

  successiveViews(subject, width, gapwidth=0, from=1)
}

\arguments{
  \item{subject}{
    The object on which to create the views.
  }
  \item{start, end}{
    For \code{Views}, they must be integer vectors (eventually with NAs)
    specifying the starting and ending positions of the views to create.

    For \code{restrict}, they must be single integers specifying the
    restriction window.

    For \code{narrow} and \code{subviews}, they can be single integers
    or NAs.

    For \code{gaps}, they can be single integers or NAs.
    The gap extraction will be restricted to the window specified by
    \code{start} and \code{end}. \code{start=NA} and \code{end=NA}
    are interpreted as \code{start=1} and \code{end=length(subject(x))},
    respectively, so, if \code{start} and \code{end} are not specified,
    then gaps are extracted from the entire subject.
  }
  \item{width}{
    For \code{narrow} and \code{subviews}, can be \code{NA}, a single
    integer, or an integer vector of the same length as \code{x}.

    For \code{successiveViews}, must be a vector of positive integers
    (with no NAs) specifying the widths of the views to create.
  }
  \item{names}{
    If not \code{NULL}, the names to assign to the views.
  }
  \item{x}{
    A Views object.
  }
  \item{i, j, ...}{
    Only one subscript is supported (\code{x[[i]]}).
  }
  \item{keep.all.ranges}{
    Not supported for Views objects (must be \code{FALSE}).
  }
  \item{use.names}{
    \code{TRUE} or \code{FALSE}. Should names be preserved?
  }
  \item{gapwidth}{
    A single integer or an integer vector with one less element than
    the \code{width} vector specifying the widths of the gaps separating
    one view from the next one.
  }
  \item{from}{
    A single integer specifying the starting position of the first view.
  }
}

\details{
  \code{restrict} will drop the views that don't overlap with the window
  specified by \code{start} and \code{end} and drop the parts of the
  remaining views that are outside the window.

  [TODO: give some details about \code{trim}]

  [TODO: give some details about \code{subviews}]

  \code{x[[i]]} extracts the i-th view from \code{x}. It cannot be used for
  extracting a view that is "out of limits" (an error will be raised).
  The returned object belong to the same class as \code{subject(x)}.

  \code{successiveViews} returns a Views object containing the views on
  \code{subject} that have the widths specified in the \code{width} vector
  and are separated by the gaps specified in \code{gapwidth}.
  The first view starts at position \code{from}.
}

\seealso{
  \link{IRanges-class},
  \link{IRanges-utils},
  \link{XIntegerViews-class},
  \link[Biostrings]{XStringViews-class}
}

\examples{
  ## Create a set of 4 views on a logical vector of length 10:
  v1 <- Views(logical(10), start=4:1, end=4:7)

  ## Extract the 2nd view:
  v1[[2]]

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

  ## Views on an XInteger object:
  subject <- XInteger(99999, sample(99, 99999, replace=TRUE) - 50)
  v4 <- Views(subject, start=1:99*1000, end=1:99*1001)
  v4
  v4[-1]
  v4[[5]]

  ## 31 adjacent views:
  successiveViews(subject, 40:10)
}

\keyword{methods}
