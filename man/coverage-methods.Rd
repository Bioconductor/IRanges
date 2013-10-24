\name{coverage-methods}

\alias{coverage-methods}

\alias{coverage}
\alias{coverage,Ranges-method}
\alias{coverage,Views-method}
\alias{coverage,RangesList-method}
\alias{coverage,RangedData-method}

\title{Coverage across a set of ranges}

\description{
  For each position in the space underlying a set of ranges, counts the
  number of ranges that cover it.
}

\usage{
coverage(x, shift=0L, width=NULL, weight=1L, ...)

\S4method{coverage}{Ranges}(x, shift=0L, width=NULL, weight=1L,
       method = c("auto", "sort", "hash"))

\S4method{coverage}{RangesList}(x, shift=0L, width=NULL, weight=1L,
       method = c("auto", "sort", "hash"))
}

\arguments{
  \item{x}{
    A \link{Ranges}, \link{Views}, or \link{RangesList} object.
    See \code{?`\link[GenomicRanges]{coverage-methods}`} in the
    \pkg{GenomicRanges} package for \code{coverage} methods for
    other objects.
  }
  \item{shift}{
    Specifies how much each range in \code{x} should be shifted before the
    coverage is computed.
    \itemize{
      \item If \code{x} is a \link{Ranges} or \link{Views} object:
            \code{shift} must be an integer or numeric vector parallel to
            \code{x} (recycled if necessary) and with no NAs.
      \item If \code{x} is a \link{RangesList} object:
            \code{shift} must be a numeric vector or list-like object of
            the same length as \code{x} (recycled if necessary).
            If it's a numeric vector, it's first turned into a list with
            \code{as.list}.
            After recycling, each list element \code{shift[[i]]} must be an
            integer or numeric vector parallel to \code{x[[i]]} (recycled
            if necessary) and with no NAs.
    }
    A positive shift value will shift the corresponding range in \code{x} to
    the right, and a negative value to the left.
  }
  \item{width}{
    \itemize{
      \item If \code{x} is a \link{Ranges} object:
            \code{width} must be \code{NULL} (the default), an NA, or a
            single non-negative integer.
            After being shifted, the ranges in \code{x} are always clipped
            on the left to keep only their positive portion i.e. their
            intersection with the [1, +inf) interval. If \code{width} is
            a single non-negative integer, then they're also clipped on the
            right to keep only their intersection with the [1, width] interval.
            In that case \code{coverage} returns a vector of length
            \code{width}.
            Otherwise, it returns a vector that extends to the last position
            in the underlying space covered by the shifted ranges.

      \item If \code{x} is a \link{Views} object:
            Same as for a \link{Ranges} object, except that, if
            \code{width} is \code{NULL} then it's treated as if it
            was \code{length(subject(x))}.

      \item If \code{x} is a \link{RangesList} object:
            \code{width} must be \code{NULL} or an integer vector parallel
            to \code{x} (i.e. with one element per list element in \code{x}).
            If not \code{NULL}, the vector must contain NAs or non-negative
            integers and it will be recycled to the length of \code{x} if
            necessary.
            If \code{NULL}, it is replaced by \code{NA} and recycled to the
            length of \code{x}.
            Finally \code{width[i]} is used to compute the coverage vector
            for \code{x[[i]]} and is therefore treated like explained above
            (when \code{x} is a \link{Ranges} object).
    }
  }
  \item{weight}{
    Specifies the weight of each range in \code{x}.
    \itemize{
      \item If \code{x} is a \link{Ranges} or \link{Views} object:
            \code{weight} must be an integer or numeric vector parallel to
            \code{x} (recycled if necessary).
      \item If \code{x} is a \link{RangesList} object:
            \code{weight} must be a numeric vector or list-like object of
            the same length as \code{x} (recycled if necessary).
            If it's a numeric vector, it's first turned into a list with
            \code{as.list}.
            After recycling, each list element \code{weight[[i]]} must be an
            integer or numeric vector parallel to \code{x[[i]]} (recycled
            if necessary).
    }
    If \code{weight} is an integer vector or list-like object of integer
    vectors, the coverage vector(s) will be returned as integer-\link{Rle}
    object(s). If it's a numeric vector or list-like object of numeric
    vectors, the coverage vector(s) will be returned as numeric-\link{Rle}
    object(s).

    Finally note that \code{weight} can also be specified as a single string
    naming a metadata column in \code{x} (i.e. a column in \code{mcols(x)})
    to be used as the weights.
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
  \item{...}{
    Further arguments to be passed to or from other methods.
  }
}

\value{
  If \code{x} is a \link{Ranges} or \link{Views} object:
  An integer- or numeric-\link{Rle} object depending on whether \code{weight}
  is an integer or numeric vector.

  If \code{x} is a \link{RangesList} object:
  A \link{SimpleRleList} object with one coverage vector per list element
  in \code{x}. The i-th coverage vector can be either an integer- or
  numeric-\link{Rle} object depending on the type of \code{weight[[i]]}
  (after \code{weight} has gone thru \code{as.list} and recycling, like
  described previously).
}

\author{H. Pages and P. Aboyoun}

\seealso{
  \itemize{
    \item \link[GenomicRanges]{coverage-methods} in the \pkg{GenomicRanges}
          package for more \code{coverage} methods.

    \item The \code{\link{slice}} function for slicing the \link{Rle} or
          \link{RleList} object returned by \code{coverage}.

    \item The \link{Ranges}, \link{RangesList}, \link{Rle}, and
          \link{RleList} classes.
  }
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
}

\keyword{methods}
