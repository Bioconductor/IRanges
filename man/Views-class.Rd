\name{Views-class}
\docType{class}

\alias{class:Views}
\alias{Views-class}
\alias{subject}
\alias{subject,Views-method}
\alias{ranges}
\alias{ranges,Views-method}
\alias{ranges<-}
\alias{ranges<-,Views-method}
\alias{length,Views-method}
\alias{start,Views-method}
\alias{end,Views-method}
\alias{width,Views-method}
\alias{names,Views-method}
\alias{start<-,Views-method}
\alias{end<-,Views-method}
\alias{width<-,Views-method}
\alias{names<-,Views-method}
\alias{elementNROWS,Views-method}
\alias{Views}
\alias{coerce,Vector,Views-method}
\alias{coerce,Views,Ranges-method}
\alias{coerce,Views,IRanges-method}
\alias{coerce,Views,NormalIRanges-method}
\alias{as.matrix,Views-method}
\alias{c,Views-method}
\alias{trim}
\alias{trim,Views-method}
\alias{subviews}
\alias{subviews,Views-method}
\alias{successiveViews}


\title{Views objects}

\description{
  The Views virtual class is a general container for storing a set of
  views on an arbitrary \link{Vector} object, called the "subject".

  Its primary purpose is to introduce concepts and provide some facilities
  that can be shared by the concrete classes that derive from it.

  Some direct subclasses of the Views class are: \link{RleViews},
  \link[XVector]{XIntegerViews} (defined in the XVector package),
  \link[Biostrings]{XStringViews} (defined in the Biostrings package),
  etc...
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

\section{Coercion}{
  In the code snippets below, \code{from} is a Views object:

  \describe{
    \item{}{
      \code{as(from, "IRanges")}: Creates an \code{IRanges} object
      containing the view locations in \code{from}.
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

\section{Subsetting}{
  \describe{
    \item{}{
      \code{x[i]}: Select the views specified by \code{i}.
    }
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

\section{Combining}{
  \describe{
    \item{}{
      \code{c(x, ..., ignore.mcols=FALSE)}: Combine \code{Views} objects.
      They must have the same subject.
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
       \code{successiveViews(subject, width, gapwidth=0, from=1)}:
       Equivalent to \code{Views(subject, successiveIRanges(width, gapwidth, from))}.
       See \code{?successiveIRanges} for a description of the \code{width},
       \code{gapwidth} and \code{from} arguments.
    }
  }
}

\author{Hervé Pagès}

\seealso{
  \link{IRanges-class},
  \link{Vector-class},
  \link{IRanges-utils},
  \link[XVector]{XVector}.

  Some direct subclasses of the Views class:
  \link{RleViews-class},
  \link[XVector]{XIntegerViews-class},
  \link[XVector]{XDoubleViews-class},
  \link[Biostrings]{XStringViews-class}.

  \code{\link{findOverlaps}}.
}

\examples{
showClass("Views")  # shows (some of) the known subclasses

## Create a set of 4 views on an XInteger subject of length 10:
subject <- Rle(3:-6)
v1 <- Views(subject, start=4:1, end=4:7)

## Extract the 2nd view:
v1[[2]]

## Some views can be "out of limits"
v2 <- Views(subject, start=4:-1, end=6)
trim(v2)
subviews(v2, end=-2)

## See ?`XIntegerViews-class` in the XVector package for more examples.
}

\keyword{methods}
\keyword{classes}
