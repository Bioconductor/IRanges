\name{coverage}

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
coverage(x, shift=0L, width=NULL, weight=1L, \dots)
\S4method{coverage}{RangesList}(x, shift=0L, width=NULL, weight=1L,
       method = c("auto", "sort", "hash"))
}

\arguments{
  \item{x}{
    An \link{IRanges}, \link{Views}, \link{MaskCollection}, 
    \link{RangesList}, \link{RangedData} object, or any object for which a
    \code{coverage} method is defined.
  }
  \item{shift}{
    For most methods, an integer vector (recycled to the length of
    \code{x}) specifying how each element in \code{x} should be
    (horizontally) shifted before the coverage is computed. Only shifted
    indices in the range [1, \code{width}] will be included in the coverage
    calculation.
    For \link{RangesList} and \link{RangedData} objects, a list or vector
    of the same length as \code{x} to be used for the corresponding element
    in \code{x}. In addition for \link{RangedData} objects, can also be
    a character vector of length 1 denoting the column to use in
    \code{values(x)}.
  }
  \item{width}{
    For most methods, the length of the returned coverage vector. For
    \link{RangesList} and \link{RangedData} objects, a list or vector of
    the same length as \code{x} to be used for the corresponding element
    in \code{x}. In addition for \link{RangedData} objects, can also be
    a character vector of length 1 denoting the column to use in
    \code{values(x)}.

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
    For most methods, an integer or numeric vector specifying how much each
    element in \code{x} counts. For \link{RangesList} and \link{RangedData}
    objects, a list or vector of the same length as \code{x} to be used
    for the corresponding element in \code{x}.
    For \link{IRanges}, \link{Views}, and \link{MaskCollection} objects,
    this can also be a single string naming a metadata column (i.e. a
    column in \code{mcols(x)}) to be used as the weights.
    For \link{RangedData} objects, this can also be a single string naming
    a column of values (i.e. a column in \code{values(x)}) to be used as
    the weights.
  }
  \item{method}{
    If \code{method} is set to \code{"sort"}, then \code{x} is sorted
    previous to the calculation of the coverage. If \code{method} is set
    to \code{hash}, then \code{x} is hashed directly to a vector of length
    \code{width} without previous sorting.

    The \code{"hash"} method is faster than the \code{"sort"} method when
    \code{x} is large (i.e. contains a lot of ranges). When \code{x} is small
    and \code{width} is big (e.g. \code{x} represents a small set of reads
    aligned to a big chromosome), then \code{method="sort"} is faster and
    uses less memory than \code{method="hash"}.

    Using \code{method="auto"} selects the best method based on
    \code{length(x)} and \code{width}.
  }
  \item{\dots}{
    Further arguments to be passed to or from other methods.
  }
}

\value{
  For most methods, an integer-\link{Rle} (if \code{typeof(weight)} is
  \code{"integer"}) or numeric-\link{Rle} (if \code{typeof(weight)} is
  \code{"double"}) object representing the coverage of \code{x}.
  For \link{RangesList} and \link{RangedData} objects, a
  \link{SimpleRleList} object representing a list of coverage vectors.
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

cvg1 <- coverage(x[c(TRUE, FALSE)], width=99)
cvg2 <- coverage(x[c(FALSE, TRUE)], width=99)
stopifnot(identical(coverage(x, width=99), cvg1 + cvg2))
stopifnot(identical(coverage(x, width=99, weight=c(5L, -11L)),
                    cvg1 * 5L + cvg2 * -11L))

cvg3 <- coverage(x, weight=1.5)
cvg3  # numeric-Rle
stopifnot(identical(cvg3, coverage(x) * 1.5))

mask1 <- Mask(mask.width=29, start=c(11, 25, 28), width=c(5, 2, 2))
mask2 <- Mask(mask.width=29, start=c(3, 10, 27), width=c(5, 8, 1))
mask3 <- Mask(mask.width=29, start=c(7, 12), width=c(2, 4))
mymasks <- append(append(mask1, mask2), mask3)
coverage(mymasks)
}

\keyword{methods}
