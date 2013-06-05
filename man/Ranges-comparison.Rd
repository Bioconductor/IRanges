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

\alias{findMatches}
\alias{findMatches,ANY,ANY-method}
\alias{findMatches,Ranges,Ranges-method}
\alias{countMatches}
\alias{countMatches,ANY,ANY-method}
\alias{countMatches,Ranges,Ranges-method}

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

\S4method{duplicated}{Ranges}(x, incomparables=FALSE, fromLast=FALSE,
           method=c("auto", "quick", "hash"))

## match()
## -------

\S4method{match}{Ranges,Ranges}(x, table, nomatch=NA_integer_, incomparables=NULL,
      method=c("auto", "quick", "hash"), match.if.overlap=FALSE)

## findMatches() / countMatches()
## ------------------------------

findMatches(x, table, select=c("all", "first", "last"), ...)
countMatches(x, table, ...)

## order() and related methods
## ----------------------------

\S4method{order}{Ranges}(..., na.last=TRUE, decreasing=FALSE)

\S4method{rank}{Ranges}(x, na.last=TRUE,
     ties.method=c("average", "first", "random", "max", "min"))

## Generalized element-wise (aka "parallel") comparison of 2 Ranges objects
## ------------------------------------------------------------------------

compare(x, y)
\S4method{compare}{Ranges,Ranges}(x, y)

rangeComparisonCodeToLetter(code)
}

\arguments{
  \item{e1, e2, x, table, y}{
    \link{Ranges} objects.
    For \code{findMatches} and \code{countMatches}, \code{x} and \code{table}
    can also be atomic vectors.
  }
  \item{incomparables}{
    Not supported.
  }
  \item{fromLast}{
    See default S3 method for \code{\link[base]{duplicated}}.
  }
  \item{method}{
    Use a Quicksort-based (\code{method="quick"}) or a
    hash-based (\code{method="hash"}) algorithm.
    The latter tends to give better performance, except maybe for some
    pathological input that we've not been able to determine so far.

    When \code{method="auto"} is specified, the most efficient algorithm will
    be used, that is, the hash-based algorithm if \code{length(x) <= 2^29},
    otherwise the Quicksort-based algorithm.
  }
  \item{nomatch}{
    The value to be returned in the case when no match is found.
    It is coerced to an \code{integer}.
  }
  \item{match.if.overlap}{
    This argument is deprecated in BioC 2.13 and won't be supported
    anymore in BioC 2.14. Please use
    \code{findOverlaps(x, table, select="first")}
    instead of
    \code{match(x, table, match.if.overlap=TRUE)}.
  }
  \item{select}{
    Not supported yet. Note that you can use \code{match} if you want to do
    \code{select="first"}. Otherwise you're welcome to request this on the
    Bioconductor mailing list.
  }
  \item{...}{
    For \code{findMatches}, \code{countMatches}: arguments to be passed
    down to \code{match} e.g. to specify one of the supported methods
    (note that, by default, i.e. with \code{method="auto"}, the most
    efficient algorithm will be used).

    For \code{order}: additional \link{Ranges} objects used for breaking ties.
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
      Returns an integer vector of the length of \code{x},
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
      \code{findMatches(x, table, select=c("all", "first", "last"), ...)}:
      An enhanced version of \code{match} that returns all the matches in
      a \link{Hits} object.
    }
    \item{}{
      \code{countMatches(x, table, ...)}:
      Returns an integer vector of the length of \code{x},
      containing the number of matches in \code{table} for each element
      in \code{x}.
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
    \item \link[GenomicRanges]{GenomicRanges-comparison} in the GenomicRanges
          package for comparing and ordering genomic ranges.
    \item \link{intra-range-methods} and \link{inter-range-methods} for
          intra and inter range transformations.
    \item \link{setops-methods} for set operations on \link{IRanges}
          objects.
    \item \code{\link{findOverlaps}} for finding overlapping ranges.
    \item \code{\link{==}},
          \code{\link[BiocGenerics]{duplicated}},
          \code{\link[BiocGenerics]{unique}},
          \code{\link[BiocGenerics]{match}},
          \code{\link{\%in\%}},
          \code{\link[BiocGenerics]{order}},
          \code{\link[BiocGenerics]{sort}},
          \code{\link[BiocGenerics]{rank}} for general information about
          those functions.
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
## B. duplicated(), unique()
## ---------------------------------------------------------------------
duplicated(x)
unique(x)

## ---------------------------------------------------------------------
## C. match(), %in%
## ---------------------------------------------------------------------
table <- x[c(2:4, 7:8)]
match(x, table)

x \%in\% table  # Warning! The warning will be removed in BioC 2.14.
## In the meantime, use suppressWarnings() to suppress the warning:
suppressWarnings(x \%in\% table)

## ---------------------------------------------------------------------
## D. findMatches(), countMatches()
## ---------------------------------------------------------------------
findMatches(x, table)
countMatches(x, table)

x_levels <- unique(x)
countMatches(x_levels, x)

## ---------------------------------------------------------------------
## E. order() AND RELATED METHODS
## ---------------------------------------------------------------------
order(x)
sort(x)
rank(x, ties.method="first")

## ---------------------------------------------------------------------
## F. GENERALIZED ELEMENT-WISE COMPARISON OF 2 Ranges OBJECTS
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

## Handling of zero-width ranges (a.k.a. empty ranges):
x <- IRanges(11:17, width=0)
x
compare(x, x[4])
compare(x, IRanges(12, 15))

## Note that x[2] and x[6] are empty ranges on the edge of non-empty
## range IRanges(12, 15). Even though -1 and 3 could also be considered
## valid codes for describing these configurations, compare()
## considers x[2] and x[6] to be *adjacent* to IRanges(12, 15), and
## thus returns codes -5 and 5:
compare(x[2], IRanges(12, 15))  # -5
compare(x[6], IRanges(12, 15))  #  5
}

\keyword{methods}
