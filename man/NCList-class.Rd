\name{NCList-class}
\docType{class}

% NCList objects:
\alias{class:NCList}
\alias{NCList-class}
\alias{NCList}

\alias{ranges,NCList-method}
\alias{length,NCList-method}
\alias{names,NCList-method}
\alias{start,NCList-method}
\alias{end,NCList-method}
\alias{width,NCList-method}
\alias{coerce,NCList,IRanges-method}
\alias{coerce,Ranges,NCList-method}

% NCLists objects:
\alias{class:NCLists}
\alias{NCLists-class}
\alias{NCLists}

\alias{ranges,NCLists-method}
\alias{length,NCLists-method}
\alias{names,NCLists-method}
\alias{start,NCLists-method}
\alias{end,NCLists-method}
\alias{width,NCLists-method}
\alias{elementLengths,NCLists-method}
\alias{coerce,NCLists,CompressedIRangesList-method}
\alias{coerce,NCLists,IRangesList-method}
\alias{coerce,RangesList,NCLists-method}


\title{Nested Containment List objects}

\description{
  The NCList class is a container for storing the Nested Containment List
  representation of a \link{Ranges} object. Preprocessing a \link{Ranges}
  as a Nested Containment List allows efficient overlap-based operations
  like \code{\link{findOverlaps}}.

  The NCLists class allows the storage of a collection of NCList objects.
  It is typically obtained by preprocessing each list element of a
  \link{RangesList} object as a Nested Containment List. Like with NCList,
  the resulting NCLists object can then be used for efficient overlap-based
  operations.

  To preprocess a \link{Ranges} or \link{RangesList}, simply call the
  \code{NCList} or \code{NCLists} constructor functions on them.
}

\usage{
NCList(x, circle.length=NA_integer_)
NCLists(x, circle.length=NA_integer_)
}

\arguments{
  \item{x}{
    The \link{Ranges} or \link{RangesList} object to preprocess.
  }
  \item{circle.length}{
    Use only if the space on top of which the ranges in \code{x} are defined
    needs to be considered circular.
    If that's the case, then use \code{circle.length} to specify the
    length of the circular space.

    \code{circle.length} must be a single positive integer (or NA if the
    space is linear) for \code{NCList}. It must be an integer vector
    parallel to \code{x} (i.e. same length) and with positive or NA values
    for \code{NCLists}.
  }
}

\author{H. Pages}

\seealso{
  \item \link{Ranges} and \link{RangesList} objects.

  \item \code{\link{findOverlaps}} for finding/counting interval overlaps
        between two \emph{range-based} objects.
}

\examples{
## Either the query or the subject of findOverlaps() can be preprocessed:
query <- IRanges(c(1, 4, 9), c(5, 7, 10))
subject <- IRanges(c(2, 2, 10), c(2, 3, 12))

ppsubject <- NCList(subject)
hits1 <- findOverlaps(query, ppsubject)
hits1

ppquery <- NCList(query)
hits2 <- findOverlaps(ppquery, subject)
hits2

## Note that 'hits1' and 'hits2' contain the same hits but not in the
## same order.
stopifnot(identical(sort(hits1), sort(hits2)))
}

\keyword{classes}
\keyword{methods}
