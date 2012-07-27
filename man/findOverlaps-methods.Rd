\name{findOverlaps-methods}

\alias{findOverlaps-methods}

\alias{findOverlaps}
\alias{findOverlaps,Ranges,IntervalTree-method}
\alias{findOverlaps,Ranges,Ranges-method}
\alias{findOverlaps,integer,Ranges-method}
\alias{findOverlaps,ANY,missing-method}
\alias{findOverlaps,Views,Views-method}
\alias{findOverlaps,ANY,Views-method}
\alias{findOverlaps,Views,ANY-method}
\alias{findOverlaps,RangesList,RangesList-method}
\alias{findOverlaps,ViewsList,ViewsList-method}
\alias{findOverlaps,ANY,ViewsList-method}
\alias{findOverlaps,ViewsList,ANY-method}
\alias{findOverlaps,RangedData,RangedData-method}
\alias{findOverlaps,RangedData,RangesList-method}
\alias{findOverlaps,RangesList,RangedData-method}

\alias{countOverlaps}
\alias{countOverlaps,Ranges,Ranges-method}
\alias{countOverlaps,Views,Views-method}
\alias{countOverlaps,ANY,Views-method}
\alias{countOverlaps,Views,ANY-method}
\alias{countOverlaps,RangesList,RangesList-method}
\alias{countOverlaps,ViewsList,ViewsList-method}
\alias{countOverlaps,ANY,ViewsList-method}
\alias{countOverlaps,ViewsList,ANY-method}
\alias{countOverlaps,RangedData,RangedData-method}
\alias{countOverlaps,RangedData,RangesList-method}
\alias{countOverlaps,RangesList,RangedData-method}

\alias{subsetByOverlaps}
\alias{subsetByOverlaps,ANY,ANY-method}
\alias{subsetByOverlaps,RangesList,RangesList-method}
\alias{subsetByOverlaps,RangedData,RangedData-method}
\alias{subsetByOverlaps,RangedData,RangesList-method}
\alias{subsetByOverlaps,RangesList,RangedData-method}

\alias{match,Ranges,Ranges-method}
\alias{match,Views,Views-method}
\alias{match,ANY,Views-method}
\alias{match,Views,ANY-method}
\alias{match,RangesList,RangesList-method}
\alias{match,ViewsList,ViewsList-method}
\alias{match,ANY,ViewsList-method}
\alias{match,ViewsList,ANY-method}
\alias{match,RangedData,RangedData-method}
\alias{match,RangedData,RangesList-method}
\alias{match,RangesList,RangedData-method}

\alias{\%in\%,Ranges,Ranges-method}
\alias{\%in\%,Views,Views-method}
\alias{\%in\%,ANY,Views-method}
\alias{\%in\%,Views,ANY-method}
\alias{\%in\%,RangesList,RangesList-method}
\alias{\%in\%,ViewsList,ViewsList-method}
\alias{\%in\%,ANY,ViewsList-method}
\alias{\%in\%,ViewsList,ANY-method}
\alias{\%in\%,RangedData,RangedData-method}
\alias{\%in\%,RangedData,RangesList-method}
\alias{\%in\%,RangesList,RangedData-method}


\title{findOverlaps (and related) methods for Ranges, Views, RangesList,
       ViewsList, and RangedData objects}

\description{
  Various methods for finding/counting interval overlaps between two
  "range-based" objects: a query and a subject.

  NOTE: This man page describes the methods that operate on a query
  and a subject that are both either a \link{Ranges}, \link{Views},
  \link{RangesList}, \link{ViewsList}, or \link{RangedData} object.
  (In addition, if the query is a \link{Ranges} object, the subject
  can be an \link{IntervalTree} object. And if the subject is a
  \link{Ranges} object, the query can be an integer vector.)

  See \code{?`\link[GenomicRanges]{findOverlaps,GenomicRanges,GenomicRanges-method}`}
  in the GenomicRanges package for methods that operate on
  \link[GenomicRanges]{GRanges} or \link[GenomicRanges]{GRangesList}
  objects.
}

\usage{
findOverlaps(query, subject, maxgap=0L, minoverlap=1L,
             type=c("any", "start", "end", "within", "equal"),
             select=c("all", "first", "last", "arbitrary"), ...)

countOverlaps(query, subject, maxgap=0L, minoverlap=1L,
              type=c("any", "start", "end", "within", "equal"), ...)

subsetByOverlaps(query, subject, maxgap=0L, minoverlap=1L,
                 type=c("any", "start", "end", "within", "equal"), ...)

\S4method{match}{Ranges,Ranges}(x, table, nomatch=NA_integer_, incomparables=NULL)

# Also:  x \%in\% table
}

\arguments{
  \item{query, subject}{
    Each of them can be a \link{Ranges}, \link{Views}, \link{RangesList},
    \link{ViewsList}, or \link{RangedData} object.
    In addition, if \code{query} is a \link{Ranges} object, \code{subject}
    can be an \link{IntervalTree} object.
    And if \code{subject} is a \link{Ranges} object, \code{query} can
    be an integer vector to be converted to length-one ranges.
    If \code{query} is a \link{RangesList} or \link{RangedData},
    \code{subject} must be a \link{RangesList} or \link{RangedData}.

    If both lists have names, each element from the subject is paired
    with the element from the query with the matching name, if any.
    Otherwise, elements are paired by position. The overlap is then
    computed between the pairs as described below.

    If \code{query} is unsorted, it is sorted first, so it is usually
    better to sort up-front, to avoid a sort with each \code{findOverlaps}
    call.

    If \code{subject} is omitted, \code{query} is queried against
    itself. In this case, and only this case, the \code{ignoreSelf}
    and \code{ignoreRedundant} arguments are allowed. By default,
    the result will contain hits for each range against itself, and if
    there is a hit from A to B, there is also a hit for B to A. If
    \code{ignoreSelf} is \code{TRUE}, all self matches are dropped. If
    \code{ignoreRedundant} is \code{TRUE}, only one of A->B and B->A
    is returned.
  }
  \item{maxgap, minoverlap}{
    Intervals with a separation of \code{maxgap} or less and a minimum
    of \code{minoverlap} overlapping positions, allowing for
    \code{maxgap}, are considered to be overlapping.  \code{maxgap}
    should be a scalar, non-negative, integer. \code{minoverlap}
    should be a scalar, positive integer.
  }
  \item{type}{
    By default, any overlap is accepted. By specifying the \code{type}
    parameter, one can select for specific types of overlap. The types
    correspond to operations in Allen's Interval Algebra (see
    references). If \code{type} is \code{start} or \code{end}, the
    intervals are required to have matching starts or ends,
    respectively. While this operation seems trivial, the naive
    implementation using \code{outer} would be much less
    efficient. Specifying \code{equal} as the type returns the
    intersection of the \code{start} and \code{end} matches. If
    \code{type} is \code{within}, the query interval must be wholly
    contained within the subject interval. Note that all matches must
    additionally satisfy the \code{minoverlap} constraint described above.

    The \code{maxgap} parameter has special meaning with the special
    overlap types. For \code{start}, \code{end}, and \code{equal},
    it specifies the maximum difference in the starts, ends or both,
    respectively. For \code{within}, it is the maximum amount by which
    the query may be wider than the subject.
  }
  \item{select}{
    When \code{select} is \code{"all"} (the default), the results are
    returned as a \link{Hits} object. When \code{select} is \code{"first"},
    \code{"last"}, or \code{"arbitrary"} the results are returned as an
    integer vector of length \code{query} containing the first, last,
    or arbitrary overlapping interval in \code{subject}, with \code{NA}
    indicating intervals that did not overlap any intervals in \code{subject}.
 
    If \code{select} is \code{"all"}, a \link{Hits} object is returned.
    For all other \code{select} the return value depends on the \code{drop}
    argument. When \code{select != "all" && !drop}, an \link{IntegerList}
    is returned, where each element of the result corresponds to a space in
    \code{query}. When\code{select != "all" && drop}, an integer vector is
    returned containing indices that are offset to align with the unlisted
    \code{query}.
  }
  \item{...}{
    Further arguments to be passed to or from other methods:
    \itemize{
      \item \code{drop}: All methods accept the \code{drop} argument
            (\code{FALSE} by default). See \code{select} argument above
            for the details.
      \item \code{ignoreSelf}, \code{ignoreRedundant}: When \code{subject}
            is omitted, the \code{ignoreSelf} and \code{ignoreRedundant}
            arguments (both \code{FALSE} by default) are allowed.
            See \code{query} and \code{subject} arguments above for the
            details.
    }
  }
  \item{x, table}{
    Should either be both \code{Ranges} or \code{Views} objects, or
    both \code{RangesList} or \code{RangedData} objects, in which case
    the indices are into the unlisted \code{table}.
  }
  \item{nomatch}{
    The value \code{match} will assign to ranges in \code{x} that do not
    overlap any ranges in \code{table}.
  }
  \item{incomparables}{
    Currently ignored.
  }
}

\details{
  A common type of query that arises when working with intervals is
  finding which intervals in one set overlap those in another.

  The simplest approach is to call the \code{findOverlaps} function
  on a \link{Ranges} or other object with range information (aka
  "range-based object").

  An \link{IntervalTree} object is a derivative of \link{Ranges} and
  stores its ranges as a tree that is optimized for overlap queries.
  Thus, for repeated queries against the same subject, it is more
  efficient to create an \link{IntervalTree} once for the subject
  using the \code{\link{IntervalTree}} constructor and then perform
  the queries against the \link{IntervalTree} instance.
}

\value{
  \code{findOverlaps} returns either a \link{Hits} object when
  \code{select="all"} (the default), or an integer vector when
  \code{select} is not \code{"all"}.

  \code{countOverlaps} returns the overlap hit count for each range
  in \code{query} using the specified \code{findOverlaps} parameters.

  \code{subsetByOverlaps} returns the subset of \code{query} that
  has an overlap hit with a range in \code{subject} using the specified
  \code{findOverlaps} parameters.

  \code{match} returns an integer vector of length \code{length(x)},
  containing the index of the first overlapping range in \code{table}
  (or \code{nomatch} if there is no overlapping range) for each range
  in \code{x}.

  \code{x \%in\% table} is a shortcut for finding the ranges in \code{x}
  that overlap any of the ranges in \code{table}.
  For \code{Ranges} objects, it returns a logical vector of length
  equal to the number of ranges in \code{x}. For \link{RangesList}
  and \link{RangedData} objects, it returns a \link{LogicalList} object,
  where each element of the result corresponds to a space in \code{x}.
}

\references{
  Allen's Interval Algebra:
  James F. Allen: Maintaining knowledge about temporal intervals. In:
  Communications of the ACM. 26/11/1983. ACM Press. S. 832-843, ISSN 0001-0782
}

\author{Michael Lawrence}

\seealso{
  \itemize{
    \item The \link{Hits} class for representing a set of hits between
          2 vector-like objects.

    \item \link[GenomicRanges]{findOverlaps,GenomicRanges,GenomicRanges-method}
          in the GenomicRanges package for methods that operate on
          \link[GenomicRanges]{GRanges} or \link[GenomicRanges]{GRangesList}
          objects.

    \item The \code{\link{IntervalTree}} class and constructor.

    \item The \link{Ranges}, \link{Views}, \link{RangesList},
          \link{ViewsList}, and \link{RangedData} classes.

    \item The \link{IntegerList} and \link{LogicalList} classes.
  }
}

\examples{
  query <- IRanges(c(1, 4, 9), c(5, 7, 10))
  subject <- IRanges(c(2, 2, 10), c(2, 3, 12))
  tree <- IntervalTree(subject)

  ## at most one hit per query
  findOverlaps(query, tree, select = "first")
  findOverlaps(query, tree, select = "last")
  findOverlaps(query, tree, select = "arbitrary")

  ## overlap as long as distance <= 1
  findOverlaps(query, tree, maxgap = 1L)

  ## shortcut
  findOverlaps(query, subject)

  query <- IRanges(c(1, 4, 9), c(5, 7, 10))
  subject <- IRanges(c(2, 2), c(5, 4))
  tree <- IntervalTree(subject)

  ## one Ranges with itself
  findOverlaps(query)

  ## single points as query
  subject <- IRanges(c(1, 6, 13), c(4, 9, 14))
  findOverlaps(c(3L, 7L, 10L), subject, select = "first")

  ## alternative overlap types
  query <- IRanges(c(1, 5, 3, 4), width=c(2, 2, 4, 6))
  subject <- IRanges(c(1, 3, 5, 6), width=c(4, 4, 5, 4))

  findOverlaps(query, tree, type = "start")
  findOverlaps(query, tree, type = "start", maxgap = 1L)
  findOverlaps(query, tree, type = "end", select = "first")
  findOverlaps(query, tree, type = "within", maxgap = 1L)
}

\keyword{methods}
