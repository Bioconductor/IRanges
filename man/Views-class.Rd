\name{Views-class}
\docType{class}

\alias{class:Views}
\alias{Views-class}
\alias{subject}
\alias{subject,Views-method}
\alias{newViews}
\alias{Views}
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
  The Views virtual class is a general container for storing a set of
  views on an arbitrary \link{Sequence} object, called the "subject".

  Its primary purpose is to introduce concepts and provide some facilities
  that can be shared by the concrete classes that derive from it.

  Some direct subclasses of the Views class are: \link{XIntegerViews},
  \link{RleViews}, \link[Biostrings]{XStringViews} (defined in the
  Biostrings package), etc...
}

\section{Constructor}{
  \describe{
    \item{}{
      \code{Views(subject, start=NA, end=NA, names=NULL)}:
      This constructor is a generic function with dispatch on argument
      \code{subject}. Specific methods must be defined for the subclasses
      of the Views class. For example a method for \link[Biostrings]{XString}
      subjects is defined that returns an \link[Biostrings]{XStringViews}
      object. There is no default method.

      \code{start} and \code{end} can be either integer vectors (eventually
      with NAs) specifying the starting and ending positions of the views
      to create, or \code{end} can be missing with \code{start} being an
      IRanges object or an Rle object containing logical values.
    }
  }
}

\section{Accessor-like methods}{
  All the accessor-like methods defined for \code{IRanges} objects
  work on Views objects. In addition, the following accessors are defined
  for Views objects:

  \describe{
    \item{}{
      \code{subject(x)}:
      Return the subject of the views.
    }
  }
}

\section{Subsetting and appending}{
  The \code{"["} and \code{c} methods defined for \code{IRanges} objects
  work on Views objects and return a Views object.
  In addition, the \code{"[["} operator is defined for Views objects:

  \describe{
    \item{}{
      \code{x[[i]]}:
      Extracts the view selected by \code{i} as an object of the same class
      as \code{subject(x)}. Subscript \code{i} can be a single integer
      or a character string.
      The result is the subsequence of \code{subject(x)} defined by
      \code{subseq(subject(x), start=start(x)[i], end=end(x)[i])}
      or an error if the view is "out of limits" (i.e. \code{start(x)[i] < 1}
      or \code{end(x)[i] > length(subject(x))}).
    }
  }
}

\section{Other methods}{
  \describe{
    \item{}{
       \code{restrict(x, start, end, keep.all.ranges=FALSE, use.names=TRUE)}:
       \code{start} and \code{end} must be single integers specifying the
       restriction window.
       \code{restrict} will drop the views that don't overlap with the
       restriction window and drop the parts of the remaining views that
       are outside the window.
    }
    \item{}{
       \code{trim(x, use.names=TRUE)}:
       [TODO]
    }
    \item{}{
       \code{narrow(x, start=NA, end=NA, width=NA, use.names=TRUE)}:
       [TODO]
    }
    \item{}{
       \code{subviews(x, start=NA, end=NA, width=NA, use.names=TRUE)}:
       [TODO]
    }
    \item{}{
       \code{gaps(x, start=NA, end=NA)}:
       \code{start} and \code{end} can be single integers or NAs.
       The gap extraction will be restricted to the window specified by
       \code{start} and \code{end}. \code{start=NA} and \code{end=NA}
       are interpreted as \code{start=1} and \code{end=length(subject(x))},
       respectively, so, if \code{start} and \code{end} are not specified,
       then gaps are extracted with respect to the entire subject.
    }
    \item{}{
       \code{successiveViews(subject, width, gapwidth=0, from=1)}:
       Equivalent to \code{Views(subject, successiveIRanges(width, gapwidth, from))}.
       See \code{?successiveIRanges} for a description of the \code{width},
       \code{gapwidth} and \code{from} arguments.
    }
  }
}

\author{H. Pages}

\seealso{
  \link{IRanges-class},
  \link{ListLike-class},
  \link{IRanges-utils},
  \link{Sequence},
  \link{XSequence}.

  Some direct subclasses of the Views class:
  \link{XIntegerViews-class},
  \link{RleViews-class},
  \link[Biostrings]{XStringViews-class}.
}

\examples{
  showClass("Views")  # shows (some of) the known subclasses

  ## Create a set of 4 views on an XInteger subject of length 10:
  subject <- XInteger(10, 3:-6)
  v1 <- Views(subject, start=4:1, end=4:7)

  ## Extract the 2nd view:
  v1[[2]]

  ## 'start' and 'end' are recycled
  Views(subject, 2:1, 4)
  Views(subject, 5:7, )
  Views(subject, , 5:7)

  ## Some views can be "out of limits"
  v2 <- Views(subject, 4:-1, 6)
  trim(v2)
  subviews(v2, end=-2)

  ## gaps() 
  v3 <- Views(subject, start=c(8, 3), end=c(14, 4))
  gaps(v3)

  ## Views on a big XInteger subject:
  subject <- XInteger(99999, sample(99, 99999, replace=TRUE) - 50)
  v4 <- Views(subject, start=1:99*1000, end=1:99*1001)
  v4
  v4[-1]
  v4[[5]]

  ## 31 adjacent views:
  successiveViews(subject, 40:10)
}

\keyword{methods}
\keyword{classes}
