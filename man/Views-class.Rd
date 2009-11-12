\name{Views-class}
\docType{class}

\alias{class:Views}
\alias{Views-class}
\alias{subject}
\alias{subject,Views-method}
\alias{newViews}
\alias{Views}
\alias{coerce,Sequence,Views-method}
\alias{[[,Views-method}
\alias{coerce,Views,NormalIRanges-method}
\alias{trim}
\alias{trim,Views-method}
\alias{subviews}
\alias{subviews,Views-method}
\alias{gaps,Views-method}
\alias{successiveViews}

% Old stuff:
\alias{views}


\title{Views objects}

\description{
  The Views virtual class is a general container for storing a set of
  views on an arbitrary \link{Sequence} object, called the "subject".

  Its primary purpose is to introduce concepts and provide some facilities
  that can be shared by the concrete classes that derive from it.

  Some direct subclasses of the Views class are: \link{XIntegerViews},
  \link{RleViews}, \link[Biostrings:XStringViews-class]{XStringViews}
  (defined in the Biostrings package), etc...
}

\section{Constructor}{
  \describe{
    \item{}{
      \code{Views(subject, start=NULL, end=NULL, width=NULL, names=NULL)}:
      This constructor is a generic function with dispatch on argument
      \code{subject}. Specific methods must be defined for the subclasses
      of the Views class. For example a method for
      \link[Biostrings:XString-class]{XString} subjects is defined
      in the Biostrings package that returns an
      \link[Biostrings:XStringViews-class]{XStringViews}
      object. There is no default method.

      The treatment of the \code{start}, \code{end} and \code{width}
      arguments is the same as with the \code{\link{IRanges}} constructor,
      except that, in addition, \code{Views} allows \code{start} to be a
      \link{Ranges} object. With this feature,
      \code{Views(subject, IRanges(my_starts, my_ends, my_widths, my_names))}
      and \code{Views(subject, my_starts, my_ends, my_widths, my_names)}
      are equivalent (except when \code{my_starts} is itself a
      \link{Ranges} object).
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
  \code{"["}, \code{c} and \code{"[["} work on a Views object.
  The first two operations are just inherited from the
  \code{IRanges} class but now they return a Views object.
  However, the \code{"[["} method for Views objects has a
  different semantic than the method for \code{IRanges} objects.

  \describe{
    \item{}{
      \code{x[[i]]}:
      Extracts the view selected by \code{i} as an object of the same class
      as \code{subject(x)}. Subscript \code{i} can be a single integer
      or a character string.
      The result is the subsequence of \code{subject(x)} defined by
      \code{window(subject(x), start=start(x)[i], end=end(x)[i])}
      or an error if the view is "out of limits" (i.e. \code{start(x)[i] < 1}
      or \code{end(x)[i] > length(subject(x))}).
    }
  }
}

\section{Other methods}{
  \describe{
    \item{}{
       \code{trim(x, use.names=TRUE)}:
       Equivalent to
       \code{restrict(x, start=1L, end=length(subject(x)), keep.all.ranges=TRUE,
                      use.names=use.names)}.
    }
    \item{}{
       \code{subviews(x, start=NA, end=NA, width=NA, use.names=TRUE)}:
       \code{start}, \code{end}, and \code{width} arguments must be vectors
       of integers, eventually with NAs, that contain coordinates relative
       to the current ranges. Equivalent to
       \code{trim(narrow(x, start=start, end=end, width=width, use.names=use.names))}.
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
  \link{Sequence-class},
  \link{IRanges-utils},
  \link{XVector}.

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

  ## Some views can be "out of limits"
  v2 <- Views(subject, start=4:-1, end=6)
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
