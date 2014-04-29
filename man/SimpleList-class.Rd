\name{SimpleList-class}
\docType{class}
\alias{SimpleList}
\alias{SimpleList-class}
\alias{CompressedList}
\alias{CompressedList-class}

% accessors
\alias{isEmpty,CompressedList-method}
\alias{isEmpty,SimpleList-method}
\alias{length,CompressedList-method}
\alias{length,SimpleList-method}
\alias{names,CompressedList-method}
\alias{names,SimpleList-method}
\alias{names<-,CompressedList-method}
\alias{names<-,SimpleList-method}

% displaying
\alias{classNameForDisplay,SimpleList-method}
\alias{classNameForDisplay,CompressedList-method}

% subsetting
\alias{[,CompressedList-method}
\alias{[,SimpleList-method}
\alias{[[<-,CompressedList-method}
\alias{[[<-,SimpleList-method}
\alias{$<-,CompressedList-method}
\alias{$<-,SimpleList-method}

% splitting and combining
\alias{c,CompressedList-method}
\alias{c,SimpleList-method}

% looping
\alias{aggregate,CompressedList-method}
\alias{aggregate,SimpleList-method}
\alias{endoapply,CompressedList-method}
\alias{endoapply,SimpleList-method}
\alias{lapply,CompressedList-method}
\alias{lapply,SimpleList-method}
\alias{mendoapply,CompressedList-method}
\alias{mendoapply,SimpleList-method}
\alias{revElements,CompressedList-method}

% coercion
\alias{as.list,SimpleList-method}
\alias{as.list.SimpleList}
\alias{unlist,CompressedList-method}
\alias{coerce,ANY,SimpleList-method}

\title{Simple and Compressed List Classes}
\description{The (non-virtual) SimpleList and (virtual) CompressedList classes
  extend the \linkS4class{List} virtual class.}

\details{
  The SimpleList and CompressedList classes provide an implementation that
  subclasses can easily extend. The underlying storage in a SimpleList
  subclass is a list object. The underlying storage in a CompressedList
  object is a virtually partitioned vector-like object. For more information
  on the available methods, see the \linkS4class{List} man page.
}

\section{Constructor}{
  \code{List} objects are typically constructed by calling the
  constructor of a concrete implementation, such as
  \code{\link{RangesList}} or \code{\link{IntegerList}}. The simplest,
  most generic implementation is \code{SimpleList}, which has the
  following constructor:
  \describe{
    \item{}{\code{SimpleList(...)}: takes possibly named objects as elements
    for the new SimpleList object.
    }
  }

  Calling \code{as(x, "List")} will convert a vector-like object into a
  \code{List}, usually a \code{CompressedList}. To explicitly request
  a \code{SimpleList} derivative, call \code{as(x, "SimpleList")}
}

\section{Coercion}{
  In the following code snippets, \code{x} is a SimpleList or CompressedList
  object.

  \describe{
    \item{}{\code{as.list(x)}: Copies the elements of \code{x} into a new R
      list object.
    }
    \item{}{\code{unlist(x, recursive = TRUE, use.names = TRUE)}: Concatenates
      the elements of \code{x} into a single \code{elementType(x)} object.
    }
  }
}

\section{Displaying}{
  When a SimpleList or CompressedList object \code{x} is displayed,
  the "Compressed" or "Simple" prefix is removed from the real class name
  of \code{x}.
  See \link[S4Vectors]{classNameForDisplay} in the \pkg{S4Vectors} package
  for more information about this.
}

\section{Subsetting}{
  In the following code snippets, \code{x} is a SimpleList or CompressedList
  object.

  \describe{
    \item{}{
      \code{x[i]}:
      In addition to normal usage, the \code{i} parameter can be a
      \code{RangesList}, logical \code{RleList}, \code{LogicalList}, or
      \code{IntegerList} object to perform subsetting within the list
      elements rather than across them.
    }
    \item{}{
      \code{x[i] <- value}:
      In addition to normal usage, the \code{i} parameter can be a
      \code{RangesList}, logical \code{RleList}, \code{LogicalList}, or
      \code{IntegerList} object to perform subsetting within the list
      elements rather than across them.
    }
  }
}

\section{Looping}{
  In the following code snippets, \code{x} is a SimpleList or CompressedList
  object.

  \describe{
    \item{}{
      \code{aggregate(x, by, FUN, start = NULL, end = NULL, width = NULL,
                      frequency = NULL, delta = NULL, ..., simplify = TRUE))}:
      In addition to normal usage, the \code{by} parameter can be a
      \code{RangesList} to aggregate within the list elements rather
      than across them. When \code{by} is a \code{RangesList}, the output is
      either a \code{SimpleAtomicList} object, if possible, or a
      \code{SimpleList} object, if not.
    }
  }
}

\author{P. Aboyoun}
\seealso{\linkS4class{List},
         \linkS4class{AtomicList} and
         \linkS4class{RangesList} for example implementations}

\examples{
SimpleList(a = letters, ranges = IRanges(1:10, 1:10))

## Displaying a SimpleList or CompressedList object:
x1 <- IntegerList()
class(x1)
x2 <- IntegerList(compress=FALSE)
class(x2)

## The "Compressed" or "Simple" prefix is removed from the real class
## name of the object:
x1
x2

## This is controlled by internal helper classNameForDisplay():
classNameForDisplay(x1)
classNameForDisplay(x2)
}
\keyword{methods}
\keyword{classes}
