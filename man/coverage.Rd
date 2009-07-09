\name{coverage}

% Functions to help migrating the interface of coverage() from "start/end"
% to "shift/width" (remove when migration is over):
\alias{coverage.isCalledWithStartEndInterface}
\alias{coverage.getShift0FromStartEnd}
\alias{coverage.getWidthFromStartEnd}

\alias{coverage.normargWidth}

\alias{coverage}
\alias{coverage,numeric-method}
\alias{coverage,IRanges-method}
\alias{coverage,Views-method}
\alias{coverage,MaskCollection-method}
\alias{coverage,RangesList-method}
\alias{coverage,RangedData-method}

\title{Coverage across a set of ranges}

\description{
  Counts the number of times a position is represented in a set of ranges. 
}

\usage{
  ## Old interface (IRanges < 1.1.58):
  #coverage(x, start=NA, end=NA, \dots)

  ## Transitional interface (the current one):
  coverage(x, start=NA, end=NA, shift=0L, width=NULL, weight=1L, \dots)
  \S4method{coverage}{RangesList}(x,
       start = structure(rep(list(NA), length(x)), names = names(x)),
       end = structure(rep(list(NA), length(x)), names = names(x)),
       shift = structure(rep(list(0L), length(x)), names = names(x)),
       width = structure(rep(list(NULL), length(x)), names = names(x)),
       weight = structure(rep(list(1L), length(x)), names = names(x)))

  ## New interface (in the near future):
  #coverage(x, shift=0L, width=NULL, weight=1L, \dots)
}

\note{
  The interface of the \code{coverage} generic is currently being migrated
  from "start/end" to "shift/width".
  In the near future, the \code{start} and \code{end} arguments will be
  dropped and the remaining arguments will be:
    \code{coverage(x, shift=0L, width=NULL, weight=1L, ...)}
  The "shift/width" interface is more intuitive, more convenient and
  offers slightly more control than the "start/end" interface.
  Also it makes sense to add the \code{weight} argument to the generic
  (versus having it supported only by some methods) since weighting the
  elements in \code{x} can be considered part of the concept of coverage
  in general.
}

\arguments{
  \item{x}{
    An \link{IRanges}, \link{Views}, \link{MaskCollection}, 
    \link{RangesList}, \link{RangedData} object, or any object for which a
    \code{coverage} method is defined.
  }
  \item{start, end}{
    For most methods, single integers specifying the position in \code{x}
    where to start and end the extraction of the coverage. For
    \link{RangesList} and \link{RangedData} objects, a list or vector of
    the same length as \code{x} to be used for the corresponding element
    in \code{x}.
    IMPORTANT NOTE: Please do not use these arguments (use the shift/width
    arguments below). They are temporarily kept for backward compatibility
    with existing code and will be dropped in the near future.
  }
  \item{shift}{
    For most methods, an integer vector (recycled to the length of
    \code{x}) specifying how each element in \code{x} should be
    (horizontally) shifted before the coverage is computed. For
    \link{RangesList} and \link{RangedData} objects, a list or vector of
    the same length as \code{x} to be used for the corresponding element
    in \code{x}.
  }
  \item{width}{
    For most methods, the length of the returned coverage vector. For
    \link{RangesList} and \link{RangedData} objects, a list or vector of
    the same length as \code{x} to be used for the corresponding element
    in \code{x}.

    If \code{width=NULL} (the default), then the specific \code{coverage}
    method that is actually selected will choose the length of the returned
    vector "in a way that makes sense".

    For example, when \code{width=NULL}, the method for \link{IRanges} objects
    returns a vector that has just enough elements to have its last
    element aligned with the rightmost end of all the ranges in \code{x} after
    those ranges have been shifted (see the \code{shift} argument above).
    This ensures that any longer coverage vector would be a "padded with zeros"
    version of the vector returned when \code{width=NULL}.

    When \code{width=NULL}, the method for \link{Views} objects
    returns a vector with \code{length(subject(x))} elements.

    When \code{width=NULL}, the method for \link{MaskCollection} objects
    returns a vector with \code{width(x)} elements.
  }
  \item{weight}{
    For most methods, an integer vector specifying how much each element in
    \code{x} counts. For \link{RangesList} and \link{RangedData} objects, a
    list or vector of the same length as \code{x} to be used for the
    corresponding element in \code{x}.
  }
  \item{\dots}{
    Further arguments to be passed to or from other methods.
  }
}

\value{
  For most methods, an \link{Rle} object representing the coverage of
  \code{x}. For \link{RangesList} and \link{RangedData} objects, a
  \link{SimpleRleList} object representing a list of coverage vectors.

  An integer value called the "coverage" can be associated to each position
  in \code{x}, indicating how many times this position is covered by the
  elements contained in \code{x}.
  For example, if \code{x} is a \link{Views} object, the coverage of
  a given position in \code{subject(x)} is the number of views it belongs to.
}

\author{H. Pages and P. Aboyoun}

\seealso{
  \link{IRanges-class},
  \link{Views-class},
  \link{Rle-class},
  \link{MaskCollection-class}
}

\examples{
  x <- IRanges(start=c(-2L, 6L, 9L, -4L, 1L, 0L, -6L, 10L),
               width=c( 5L, 0L, 6L,  1L, 4L, 3L,  2L,  3L))
  coverage(x)
  coverage(x, shift=7)
  coverage(x, shift=7, width=27)
  coverage(restrict(x, 1, 10))
  coverage(reduce(x), shift=7)
  coverage(gaps(shift(x, 7), start=1, end=27))

  mask1 <- Mask(mask.width=29, start=c(11, 25, 28), width=c(5, 2, 2))
  mask2 <- Mask(mask.width=29, start=c(3, 10, 27), width=c(5, 8, 1))
  mask3 <- Mask(mask.width=29, start=c(7, 12), width=c(2, 4))
  mymasks <- append(append(mask1, mask2), mask3)
  coverage(mymasks)
}

\keyword{methods}
