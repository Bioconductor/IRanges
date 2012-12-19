\name{Ranges-comparison}

\alias{Ranges-comparison}

\alias{compare}
\alias{compare,Ranges,Ranges-method}
\alias{rangeComparisonCodeToLetter}

\alias{==,Ranges,Ranges-method}
\alias{<=,Ranges,Ranges-method}

\alias{duplicated,Ranges-method}

\alias{match,Ranges,Ranges-method}
\alias{\%in\%,Ranges,Ranges-method}

\alias{order,Ranges-method}
\alias{rank,Ranges-method}


\title{Comparing and ordering ranges}

\description{
  Methods for comparing and ordering the elements in one or more
  \link{Ranges} objects.
}

\usage{
## Element-wise (aka "parallel") comparison of 2 Ranges objects
## ------------------------------------------------------------

\S4method{==}{Ranges,Ranges}(e1, e2)

\S4method{<=}{Ranges,Ranges}(e1, e2)

## duplicated()
## ------------

\S4method{duplicated}{Ranges}(x, incomparables=FALSE, ...)

## match()
## -------

\S4method{match}{Ranges,Ranges}(x, table, nomatch=NA_integer_, incomparables=NULL,
      match.if.overlap=FALSE)

## order() and related methods
## ----------------------------

\S4method{order}{Ranges}(..., na.last=TRUE, decreasing=FALSE)

\S4method{rank}{Ranges}(x, na.last=TRUE,
     ties.method=c("average", "first", "random", "max", "min"))

## Generalized element-wise (aka "parallel") comparison of 2 Ranges objects
## ------------------------------------------------------------------------

compare(x, y)
rangeComparisonCodeToLetter(code)
}

\arguments{
  \item{e1, e2, x, table, y}{
    \link{Ranges} objects.
  }
  \item{incomparables}{
    Not supported.
  }
  \item{...}{
    For \code{duplicated}: the following additional arguments are supported:
    \describe{
      \item{\code{fromLast}}{
        See default S3 method for \code{\link[base]{duplicated}}.
      }
      \item{\code{method}}{
        Use a Quicksort-based (\code{method="quick"}) or a
        hash-based (\code{method="hash"}) algorithm.
        The latter tends to give better performance, except maybe for some
        pathological input that we've not been able to determine so far.
        When \code{method="auto"} is specified, the hash-based algorithm
        is used as long as the length of \code{x} is <= 2^29, otherwise the
        Quicksort-based algorithm is used.
      }
    }

    For \code{order}: additional \link{Ranges} objects used for breaking ties.
  }
  \item{nomatch}{
    The value to be returned in the case when no match is found.
    It is coerced to an \code{integer}.
  }
  \item{match.if.overlap}{
    You temporarily need to explicitly provide this argument otherwise
    \code{match()} will issue a warning.

    Starting with BioC 2.12, the default behavior of \code{match()} on
    \link{Ranges} objects has changed to use \emph{equality} instead of
    \emph{overlap} for comparing elements between \link{Ranges} objects
    \code{x} and \code{table}. Now \code{x[i]} and \code{table[j]} are
    considered to match when they are equal (i.e. \code{x[i] == table[j]}),
    instead of when they overlap. This new behavior is consistent with
    \code{base::\link[base]{match}()}.

    If you need the new behavior, use \code{match.if.overlap=FALSE}.

    If you need the old behavior, you can either do:
    \code{findOverlaps(x, table, select="first")}
    which is the recommended way. Alternatively, you can call \code{match()}
    with \code{match.if.overlap=TRUE}.
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
  On a \link{Ranges} object, \code{order}, \code{sort}, and \code{rank}
  are consistent with this order.

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
      \code{match(x, table, nomatch=NA_integer_)}:
      Returns an integer vector of length \code{length(x)},
      containing the index of the first matching range in \code{table}
      (or \code{nomatch} if there is no matching range) for each range
      in \code{x}.
    }
    \item{}{
      \code{x \%in\% table}:
      A shortcut for finding the ranges in \code{x} that match any of
      the ranges in \code{table}. Returns a logical vector of length
      equal to the number of ranges in \code{x}.
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

\author{H. Pages}

\seealso{
  \itemize{
    \item The \link{Ranges} class.
    \item \code{\link{==}},
          \code{\link[BiocGenerics]{duplicated}},
          \code{\link[BiocGenerics]{unique}},
          \code{\link[BiocGenerics]{match}},
          \code{\link{\%in\%}},
          \code{\link[BiocGenerics]{order}},
          \code{\link[BiocGenerics]{sort}},
          \code{\link[BiocGenerics]{rank}} for general information about
          those functions.
    \item \link{intra-range-methods} and \link{inter-range-methods} for
          intra and inter range transformations.
    \item \link{setops-methods} for set operations on \link{IRanges}
          objects.
    \item \code{\link{findOverlaps}} for finding overlapping ranges.
  }
}

\examples{
x <- IRanges(start=c(20L, 8L, 20L, 22L, 25L, 20L, 22L, 22L),
             width=c( 4L, 0L, 11L,  5L,  0L,  9L,  5L,  0L))
x

## ---------------------------------------------------------------------
## A. ELEMENT-WISE (AKA "PARALLEL") COMPARISON OF 2 Ranges OBJECTS
## ---------------------------------------------------------------------
which(width(x) == 0)  # 3 empty ranges
x[2] == x[2]  # TRUE
x[2] == x[5]  # FALSE
x == x[4]
x >= x[3]

## ---------------------------------------------------------------------
## B. duplicated(), unique(), match(), %in%
## ---------------------------------------------------------------------
duplicated(x)
unique(x)

table <- x[c(2:4, 8)]
match(x, table)  # warning!
## Specify 'match.if.overlap=FALSE' to suppress the warning:
match(x, table, match.if.overlap=FALSE)

x \%in\% table  # warning!
## Use suppressWarnings() to suppress the warning:
suppressWarnings(x \%in\% table)

## ---------------------------------------------------------------------
## C. order() AND RELATED METHODS
## ---------------------------------------------------------------------
order(x)
sort(x)
rank(x, ties.method="first")

## ---------------------------------------------------------------------
## D. GENERALIZED ELEMENT-WISE COMPARISON OF 2 Ranges OBJECTS
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
