\name{Ranges-comparison}

\alias{Ranges-comparison}

\alias{compare}
\alias{compare,Ranges,Ranges-method}
\alias{rangeComparisonCodeToLetter}

\alias{==,Ranges,Ranges-method}
\alias{<=,Ranges,Ranges-method}

\alias{duplicated,Ranges-method}
\alias{unique,Ranges-method}

\alias{order,Ranges-method}
\alias{sort,Ranges-method}
\alias{rank,Ranges-method}


\title{Comparing and ordering ranges}

\description{
  Methods for comparing and ordering the elements in one or more
  \link{Ranges} objects.
}

\usage{
## ==== Equality and related methods ====
## --------------------------------------

\S4method{==}{Ranges,Ranges}(e1, e2)

\S4method{!=}{Ranges,Ranges}(e1, e2)

\S4method{duplicated}{Ranges}(x, incomparables=FALSE, fromLast=FALSE,
           method=c("auto", "quick", "hash"), ...)

\S4method{unique}{Ranges}(x, incomparables=FALSE, fromLast=FALSE,
       method=c("auto", "quick", "hash"), ...)

## ==== Ordering and related methods ====
## --------------------------------------

\S4method{<=}{Ranges,Ranges}(e1, e2)
\S4method{>=}{Ranges,Ranges}(e1, e2)
\S4method{<}{Ranges,Ranges}(e1, e2)
\S4method{>}{Ranges,Ranges}(e1, e2)

\S4method{order}{Ranges}(..., na.last=TRUE, decreasing=FALSE)

\S4method{sort}{Ranges}(x, decreasing=FALSE, ...)

\S4method{rank}{Ranges}(x, na.last=TRUE,
     ties.method=c("average", "first", "random", "max", "min"))

## ==== Generalized range-wise comparison ====
## -------------------------------------------
compare(x, y)
rangeComparisonCodeToLetter(code)
}

\arguments{
  \item{e1, e2, x, y}{
    \link{Ranges} objects.
  }
  \item{incomparables}{
    Must be \code{FALSE}.
  }
  \item{fromLast}{
    See default S3 method for \code{\link[base]{duplicated}}.
  }
  \item{method}{
    Use a Quicksort-based (\code{method="quick"})
    or a hash-based (\code{method="hash"}) algorithm.
    The latter tends to give better performance, except maybe for some
    pathological input that we've not been able to determine so far.
    When \code{method="auto"} is specified, the hash-based algorithm
    is used as long as the length of \code{x} is <= 2^29, otherwise the
    Quicksort-based algorithm is used.
  }
  \item{...}{
    \link{Ranges} objects for \code{order}.
  }
  \item{na.last}{
    Ignored.
  }
  \item{decreasing}{
    \code{TRUE} or \code{FALSE}.
  }
  \item{ties.method}{
    A character string specifying how ties are treated. Only \code{"first"}
    is supported for now.
  }
  \item{code}{
    A vector of codes as returned by \code{compare}.
  }
}

\details{
  Two ranges are considered equal iff they share the same start and width.
  Note that with this definition, 2 empty ranges are generally
  not equal (they need to share the same start to be considered equal).
  This means that, when it comes to comparing ranges, an empty range is
  interpreted as a position between its end and start. For example, a
  typical usecase is comparison of insertion points defined along a string
  (like a DNA sequence) and represented as empty ranges.

  Ranges are ordered by starting position first, and then by width.
  This way, the space of ranges is totally ordered.
  The \code{order}, \code{sort} and \code{rank} methods for \link{Ranges}
  objects are consistent with this order.

  \describe{
    \item{}{
      \code{duplicated(x)}:
      Determines which elements of \code{x} are equal to elements
      with smaller subscripts, and returns a logical vector indicating
      which elements are duplicates.
      It is semantically equivalent to \code{duplicated(as.data.frame(x))}.
      See \code{\link[base]{duplicated}} in the base package for more details.
    }
    \item{}{
      \code{unique(x)}:
      Removes duplicate ranges from \code{x}.
      See \code{\link[base]{unique}} in the base package for more details.
    }
    \item{}{
      \code{order(...)}:
      Returns a permutation which rearranges its first argument (a \link{Ranges}
      object) into ascending order, breaking ties by further arguments (also
      \link{Ranges} objects).
      See \code{\link[BiocGenerics]{order}} in the BiocGenerics package
      for more information.
    }
    \item{}{
      \code{sort(x)}:
      Sorts \code{x}.
      See \code{\link[base]{sort}} in the base package for more details.
    }
    \item{}{
      \code{rank(x, na.last=TRUE, ties.method=c("average", "first", "random", "max", "min"))}:
      Returns the sample ranks of the ranges in \code{x}.
      See \code{\link[base]{rank}} in the base package for more details.
    }
    \item{}{
      \code{compare(x, y)}:
      Performs "generalized range-wise comparison" of \code{x} and \code{y},
      that is, returns an integer vector where the i-th element is a code
      describing how the i-th element in \code{x} is qualitatively positioned
      relatively to the i-th element in \code{y}.

      Here is a summary of the 13 predefined codes (and their letter
      equivalents) and their meanings:
      \preformatted{
      -6 a: x[i]: .oooo.......         6 m: x[i]: .......oooo.
            y[i]: .......oooo.              y[i]: .oooo.......

      -5 b: x[i]: ..oooo......         5 l: x[i]: ......oooo..
            y[i]: ......oooo..              y[i]: ..oooo......

      -4 c: x[i]: ...oooo.....         4 k: x[i]: .....oooo...
            y[i]: .....oooo...              y[i]: ...oooo.....

      -3 d: x[i]: ...oooooo...         3 j: x[i]: .....oooo...
            y[i]: .....oooo...              y[i]: ...oooooo...

      -2 e: x[i]: ..oooooooo..         2 i: x[i]: ....oooo....
            y[i]: ....oooo....              y[i]: ..oooooooo..

      -1 f: x[i]: ...oooo.....         1 h: x[i]: ...oooooo...
            y[i]: ...oooooo...              y[i]: ...oooo.....

                      0 g: x[i]: ...oooooo...
                           y[i]: ...oooooo...
      }

      Note that this way of comparing ranges is a refinement over the
      standard ranges comparison defined by the \code{==}, \code{!=},
      \code{<=}, \code{>=}, \code{<} and \code{>} operators. In particular
      a code that is \code{< 0}, \code{= 0}, or \code{> 0}, corresponds to
      \code{x[i] < y[i]}, \code{x[i] == y[i]}, or \code{x[i] > y[i]},
      respectively.

      The \code{compare} method for \link{Ranges} objects is guaranteed
      to return predefined codes only but methods for other objects (e.g.
      for \link[GenomicRanges]{GenomicRanges} objects) can return
      non-predefined codes. Like for the predefined codes, the sign of any
      non-predefined code must tell whether \code{x[i]} is less than, or
      greater than \code{y[i]}.
    }
    \item{}{
      \code{rangeComparisonCodeToLetter(x)}:
      Translate the codes returned by \code{compare}. The 13 predefined
      codes are translated as follow: -6 -> a; -5 -> b; -4 -> c; -3 -> d;
      -2 -> e; -1 -> f; 0 -> g; 1 -> h; 2 -> i; 3 -> j; 4 -> k; 5-> l; 6 -> m.
      Any non-predefined code is translated to X.
      The translated codes are returned in a factor with 14 levels:
      a, b, ..., l, m, X.
    }
  }
}

\seealso{
  \link{Ranges-class},
  \link{IRanges-class},
  \code{\link[base]{duplicated}},
  \code{\link[base]{unique}},
  \code{\link[BiocGenerics]{order}},
  \code{\link[base]{sort}},
  \code{\link[base]{rank}}
}

\examples{
  x <- IRanges(start=c(20L, 8L, 20L, 22L, 25L, 20L, 22L, 22L),
               width=c( 4L, 0L, 11L,  5L,  0L,  9L,  5L,  0L))
  x

  ## ---------------------------------------------------------------------
  ## A. EQUALITY AND RELATED METHODS
  ## ---------------------------------------------------------------------
  which(width(x) == 0)  # 3 empty ranges
  x[2] == x[2]  # TRUE
  x[2] == x[5]  # FALSE
  x == x[4]
  duplicated(x)
  unique(x)

  ## ---------------------------------------------------------------------
  ## B. ORDERING AND RELATED METHODS
  ## ---------------------------------------------------------------------
  x >= x[3]
  order(x)
  sort(x)
  rank(x, ties.method="first")

  ## ---------------------------------------------------------------------
  ## C. GENERALIZED RANGE-WISE COMPARISON OF 2 Ranges OBJECTS
  ## ---------------------------------------------------------------------
  x0 <- IRanges(1:11, width=4)
  x0
  y0 <- IRanges(6, 9)
  compare(x0, y0)
  compare(IRanges(4:6, width=6), y0)
  compare(IRanges(6:8, width=2), y0)
  compare(x0, y0) < 0   # equivalent to 'x0 < y0'
  compare(x0, y0) == 0  # equivalent to 'x0 == y0'
  compare(x0, y0) > 0   # equivalent to 'x0 > y0'

  rangeComparisonCodeToLetter(-10:10)
  rangeComparisonCodeToLetter(compare(x0, y0))
}

\keyword{methods}
