\name{coverage-methods}

\alias{coverage-methods}

\alias{coverage}
\alias{coverage,Ranges-method}
\alias{coverage,Views-method}
\alias{coverage,RangesList-method}
\alias{coverage,RangedData-method}

\title{Coverage of a set of ranges}

\description{
  For each position in the space underlying a set of ranges, counts the
  number of ranges that cover it.
}

\usage{
coverage(x, shift=0L, width=NULL, weight=1L, ...)

\S4method{coverage}{Ranges}(x, shift=0L, width=NULL, weight=1L,
            method=c("auto", "sort", "hash"))

\S4method{coverage}{RangesList}(x, shift=0L, width=NULL, weight=1L,
            method=c("auto", "sort", "hash"))
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
            \code{x} (will get recycled if necessary) and with no NAs.
      \item If \code{x} is a \link{RangesList} object:
            \code{shift} must be a numeric vector or list-like object of
            the same length as \code{x} (will get recycled if necessary).
            If it's a numeric vector, it's first turned into a list with
            \code{as.list}.
            After recycling, each list element \code{shift[[i]]} must be an
            integer or numeric vector parallel to \code{x[[i]]} (will get
            recycled if necessary) and with no NAs.
    }
    A positive shift value will shift the corresponding range in \code{x} to
    the right, and a negative value to the left.
  }
  \item{width}{
    Specifies the length of the returned coverage vector(s).
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
            integers and it will get recycled to the length of \code{x} if
            necessary.
            If \code{NULL}, it is replaced with \code{NA} and recycled to the
            length of \code{x}.
            Finally \code{width[i]} is used to compute the coverage vector
            for \code{x[[i]]} and is therefore treated like explained above
            (when \code{x} is a \link{Ranges} object).
    }
  }
  \item{weight}{
    Assigns a weight to each range in \code{x}.
    \itemize{
      \item If \code{x} is a \link{Ranges} or \link{Views} object:
            \code{weight} must be an integer or numeric vector parallel to
            \code{x} (will get recycled if necessary).
      \item If \code{x} is a \link{RangesList} object:
            \code{weight} must be a numeric vector or list-like object of
            the same length as \code{x} (will get recycled if necessary).
            If it's a numeric vector, it's first turned into a list with
            \code{as.list}.
            After recycling, each list element \code{weight[[i]]} must be an
            integer or numeric vector parallel to \code{x[[i]]} (will get
            recycled if necessary).
    }
    If \code{weight} is an integer vector or list-like object of integer
    vectors, the coverage vector(s) will be returned as integer-\link{Rle}
    object(s). If it's a numeric vector or list-like object of numeric
    vectors, the coverage vector(s) will be returned as numeric-\link{Rle}
    object(s).

    Alternatively, \code{weight} can also be specified as a single string
    naming a metadata column in \code{x} (i.e. a column in \code{mcols(x)})
    to be used as the \code{weight} vector.
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
  An \link{RleList} object with one coverage vector per list element
  in \code{x}, and with \code{x} names propagated to it. The i-th coverage
  vector can be either an integer- or numeric-\link{Rle} object, depending
  on the type of \code{weight[[i]]} (after \code{weight} has gone thru
  \code{as.list} and recycling, like described previously).
}

\author{H. Pagès and P. Aboyoun}

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
## ---------------------------------------------------------------------
## A. COVERAGE OF AN IRanges OBJECT
## ---------------------------------------------------------------------
x <- IRanges(start=c(-2L, 6L, 9L, -4L, 1L, 0L, -6L, 10L),
             width=c( 5L, 0L, 6L,  1L, 4L, 3L,  2L,  3L))
coverage(x)
coverage(x, shift=7)
coverage(x, shift=7, width=27)
coverage(x, shift=c(-4, 2))  # 'shift' gets recycled
coverage(x, shift=c(-4, 2), width=12)
coverage(x, shift=-max(end(x)))

coverage(restrict(x, 1, 10))
coverage(reduce(x), shift=7)
coverage(gaps(shift(x, 7), start=1, end=27))

## With weights:
coverage(x, weight=as.integer(10^(0:7)))  # integer-Rle
coverage(x, weight=c(2.8, -10))  # numeric-Rle, 'shift' gets recycled

## ---------------------------------------------------------------------
## B. SOME MATHEMATICAL PROPERTIES OF THE coverage() FUNCTION
## ---------------------------------------------------------------------

## PROPERTY 1: The coverage vector is not affected by reordering the
## input ranges:
set.seed(24)
x <- IRanges(sample(1000, 40, replace=TRUE), width=17:10)
cvg0 <- coverage(x)
stopifnot(identical(coverage(sample(x)), cvg0))

## Of course, if the ranges are shifted and/or assigned weights, then
## this doesn't hold anymore, unless the 'shift' and/or 'weight'
## arguments are reordered accordingly.

## PROPERTY 2: The coverage of the concatenation of 2 Ranges objects 'x'
## and 'y' is the sum of the 2 individual coverage vectors:
y <- IRanges(sample(-20:280, 36, replace=TRUE), width=28)
stopifnot(identical(coverage(c(x, y), width=100),
                    coverage(x, width=100) + coverage(y, width=100)))

## Note that, because adding 2 vectors in R recycles the shortest to
## the length of the longest, the following is generally FALSE:
identical(coverage(c(x, y)), coverage(x) + coverage(y))  # FALSE

## It would only be TRUE if the 2 coverage vectors we add had the same
## length, which would only happen by chance. By using the same 'width'
## value when we computed the 2 coverages previously, we made sure they
## had the same length.

## Because of properties 1 & 2, we have:
x1 <- x[c(TRUE, FALSE)]  # pick up 1st, 3rd, 5th, etc... ranges
x2 <- x[c(FALSE, TRUE)]  # pick up 2nd, 4th, 6th, etc... ranges
cvg1 <- coverage(x1, width=100)
cvg2 <- coverage(x2, width=100)
stopifnot(identical(coverage(x, width=100), cvg1 + cvg2))

## PROPERTY 3: Multiplying the weights by a scalar has the effect of
## multiplying the coverage vector by the same scalar:
weight <- runif(40)
cvg3 <- coverage(x, weight=weight)
stopifnot(all.equal(coverage(x, weight=-2.68 * weight), -2.68 * cvg3))

## Because of properties 1 & 2 & 3, we have:
stopifnot(identical(coverage(x, width=100, weight=c(5L, -11L)),
                    5L * cvg1 - 11L * cvg2))

## PROPERTY 4: Using the sum of 2 weight vectors produces the same
## result as using the 2 weight vectors separately and summing the
## 2 results:
weight2 <- 10 * runif(40) + 3.7
stopifnot(all.equal(coverage(x, weight=weight + weight2),
                    cvg3 + coverage(x, weight=weight2)))

## PROPERTY 5: Repeating any input range N number of times is
## equivalent to multiplying its assigned weight by N:
times <- sample(0:10L, length(x), replace=TRUE)
stopifnot(all.equal(coverage(rep(x, times), weight=rep(weight, times)),
                    coverage(x, weight=weight * times)))

## In particular, if 'weight' is not supplied:
stopifnot(identical(coverage(rep(x, times)), coverage(x, weight=times)))

## PROPERTY 6: If none of the input range actually gets clipped during
## the "shift and clip" process, then:
##
##     sum(cvg) = sum(width(x) * weight)
##
stopifnot(sum(cvg3) == sum(width(x) * weight))

## In particular, if 'weight' is not supplied:
stopifnot(sum(cvg0) == sum(width(x)))

## Note that this property is sometimes used in the context of a
## ChIP-Seq analysis to estimate "the number of reads in a peak", that
## is, the number of short reads that belong to a peak in the coverage
## vector computed from the genomic locations (a.k.a. genomic ranges)
## of the aligned reads. Because of property 6, the number of reads in
## a peak is approximately the area under the peak divided by the short
## read length.

## PROPERTY 7: If 'weight' is not supplied, then disjoining or reducing
## the ranges before calling coverage() has the effect of "shaving" the
## coverage vector at elevation 1:
table(cvg0)
shaved_cvg0 <- cvg0
runValue(shaved_cvg0) <- pmin(runValue(cvg0), 1L)
table(shaved_cvg0)

stopifnot(identical(coverage(disjoin(x)), shaved_cvg0))
stopifnot(identical(coverage(reduce(x)), shaved_cvg0))

## ---------------------------------------------------------------------
## C. SOME SANITY CHECKS
## ---------------------------------------------------------------------
dummy.coverage <- function(x, shift=0L, width=NULL)
{
    y <- unlist(shift(x, shift))
    if (is.null(width))
        width <- max(c(0L, y))
    Rle(tabulate(y,  nbins=width))
}

check_real_vs_dummy <- function(x, shift=0L, width=NULL)
{
    res1 <- coverage(x, shift=shift, width=width)
    res2 <- dummy.coverage(x, shift=shift, width=width)
    stopifnot(identical(res1, res2))
}
check_real_vs_dummy(x)
check_real_vs_dummy(x, shift=7)
check_real_vs_dummy(x, shift=7, width=27)
check_real_vs_dummy(x, shift=c(-4, 2))
check_real_vs_dummy(x, shift=c(-4, 2), width=12)
check_real_vs_dummy(x, shift=-max(end(x)))

## With a set of distinct single positions:
x3 <- IRanges(sample(50000, 20000), width=1)
stopifnot(identical(sort(start(x3)), which(coverage(x3) != 0L)))

## ---------------------------------------------------------------------
## D. COVERAGE OF AN IRangesList OBJECT
## ---------------------------------------------------------------------
x <- IRangesList(A=IRanges(3*(4:-1), width=1:3), B=IRanges(2:10, width=5))
cvg <- coverage(x)
cvg

stopifnot(identical(cvg[[1]], coverage(x[[1]])))
stopifnot(identical(cvg[[2]], coverage(x[[2]])))

coverage(x, width=c(50, 9))
coverage(x, width=c(NA, 9))
coverage(x, width=9)  # 'width' gets recycled

## Each list element in 'shift' and 'weight' gets recycled to the length
## of the corresponding element in 'x'.
weight <- list(as.integer(10^(0:5)), -0.77)
cvg2 <- coverage(x, weight=weight)
cvg2  # 1st coverage vector is an integer-Rle, 2nd is a numeric-Rle

identical(mapply(coverage, x=x, weight=weight), as.list(cvg2))
}

\keyword{methods}
\keyword{utilities}
