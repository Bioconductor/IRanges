\name{findOverlaps-methods}

\alias{findOverlaps-methods}

\alias{findOverlaps}
\alias{findOverlaps,Ranges,IntervalTree-method}
\alias{findOverlaps,NCList,Ranges-method}
\alias{findOverlaps,Ranges,NCList-method}
\alias{findOverlaps,Ranges,Ranges-method}
\alias{findOverlaps,Vector,missing-method}
\alias{findOverlaps,integer,Ranges-method}
\alias{findOverlaps,Views,Views-method}
\alias{findOverlaps,Views,Vector-method}
\alias{findOverlaps,Vector,Views-method}
\alias{findOverlaps,RangesList,IntervalForest-method}
\alias{findOverlaps,RangesList,RangesList-method}
\alias{findOverlaps,ViewsList,ViewsList-method}
\alias{findOverlaps,ViewsList,Vector-method}
\alias{findOverlaps,Vector,ViewsList-method}
\alias{findOverlaps,RangedData,RangedData-method}
\alias{findOverlaps,RangedData,RangesList-method}
\alias{findOverlaps,RangesList,RangedData-method}

\alias{countOverlaps}
\alias{countOverlaps,ANY,Vector-method}
\alias{countOverlaps,ANY,missing-method}
\alias{countOverlaps,RangesList,RangesList-method}
\alias{countOverlaps,RangesList,IntervalForest-method}
\alias{countOverlaps,ViewsList,ViewsList-method}
\alias{countOverlaps,ViewsList,Vector-method}
\alias{countOverlaps,Vector,ViewsList-method}
\alias{countOverlaps,RangedData,RangedData-method}
\alias{countOverlaps,RangedData,RangesList-method}
\alias{countOverlaps,RangesList,RangedData-method}

\alias{overlapsAny}
\alias{overlapsAny,Ranges,Ranges-method}
\alias{overlapsAny,Views,Views-method}
\alias{overlapsAny,Views,Vector-method}
\alias{overlapsAny,Vector,Views-method}
\alias{overlapsAny,RangesList,RangesList-method}
\alias{overlapsAny,RangesList,IntervalForest-method}
\alias{overlapsAny,ViewsList,ViewsList-method}
\alias{overlapsAny,ViewsList,Vector-method}
\alias{overlapsAny,Vector,ViewsList-method}
\alias{overlapsAny,RangedData,RangedData-method}
\alias{overlapsAny,RangedData,RangesList-method}
\alias{overlapsAny,RangesList,RangedData-method}
\alias{overlapsAny,Vector,missing-method}

\alias{\%over\%}
\alias{\%within\%}
\alias{\%outside\%}

\alias{subsetByOverlaps}
\alias{subsetByOverlaps,Vector,Vector-method}
\alias{subsetByOverlaps,RangedData,RangedData-method}
\alias{subsetByOverlaps,RangedData,RangesList-method}
\alias{subsetByOverlaps,RangesList,RangedData-method}

\alias{mergeByOverlaps}

\alias{ranges,Hits-method}


\title{Finding overlapping ranges}

\description{
  Various methods for finding/counting interval overlaps between two
  "range-based" objects: a query and a subject.

  NOTE: This man page describes the methods that operate on a query
  and a subject that are both either a \link{Ranges}, \link{Views},
  \link{RangesList}, \link{ViewsList}, or \link{RangedData} object.
  (In addition, if the query is a \link{Ranges} object, the subject
  can be an \link{IntervalTree} object; if the query is a \link{RangesList} object,
  the subject can be a \link{IntervalForest} object. And if the subject is a \link{Ranges} 
  object, the query can be an integer vector.)

  See \code{?`\link[GenomicRanges]{findOverlaps,GenomicRanges,GenomicRanges-method}`}
  in the GenomicRanges package for methods that operate on
  \link[GenomicRanges]{GRanges} or \link[GenomicRanges]{GRangesList}
  objects. See also the \code{?`\link[GenomicRanges]{GIntervalTree}`} class and the
  \code{?`\link[GenomicRanges]{findOverlaps,GenomicRanges,GIntervalTree-method}`} method 
  for finding overlaps with persistent \link{IntervalForest} objects.
}

\usage{
findOverlaps(query, subject, maxgap=0L, minoverlap=1L,
             type=c("any", "start", "end", "within", "equal"),
             select=c("all", "first", "last", "arbitrary"),
             algorithm=c("nclist", "intervaltree"), ...)

countOverlaps(query, subject, maxgap=0L, minoverlap=1L,
              type=c("any", "start", "end", "within", "equal"),
              algorithm=c("nclist", "intervaltree"), ...)

overlapsAny(query, subject, maxgap=0L, minoverlap=1L,
            type=c("any", "start", "end", "within", "equal"),
            algorithm=c("nclist", "intervaltree"), ...)
query \%over\% subject
query \%within\% subject
query \%outside\% subject

subsetByOverlaps(query, subject, maxgap=0L, minoverlap=1L,
                 type=c("any", "start", "end", "within", "equal"),
                 algorithm=c("nclist", "intervaltree"), ...)

mergeByOverlaps(query, subject, ...)

\S4method{ranges}{Hits}(x, query, subject)
}

\arguments{
  \item{query, subject}{
    Each of them can be a \link{Ranges}, \link{Views}, \link{RangesList},
    \link{ViewsList}, or \link{RangedData} object.
    In addition, if \code{query} is a \link{Ranges} object, \code{subject}
    can be an \link{IntervalTree} object; if \code{query} is a \link{RangesList} object,
    then \code{subject} can be an \link{IntervalForest} object. 
    And if \code{subject} is a \link{Ranges} object, \code{query} can be an 
    integer vector to be converted to length-one ranges.
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
  \item{x}{
    \link{Hits} object returned by \code{findOverlaps}.
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
  the queries against the \link{IntervalTree} instance. An \link{IntervalForest}
  object is a derivative of \link{RangesList} and stores its ranges
  as a set of trees optimizized for partitioned overlap queries.
  Again, for repeated queries against the same subject list, it is more
  efficient to create an \link{IntervalForest} once and then perform
  the queries against the \link{IntervalForest} instance.
}

\value{
  \code{findOverlaps} returns either a \link{Hits} object when
  \code{select="all"} (the default), or an integer vector when
  \code{select} is not \code{"all"}. For \link{RangesList} objects
  it returns a \link{HitsList-class} object when \code{select="all"}, or
  an \link{IntegerList} when \code{select} is not \code{"all"}. 
  When \code{subject} is an \link{IntervalForest} object, 
  it returns a \link{CompressedHitsList} or \link{CompressedIntegerList}
  respectively.

  \code{countOverlaps} returns the overlap hit count for each range
  in \code{query} using the specified \code{findOverlaps} parameters.
  For \link{RangesList} objects, it returns an \link{IntegerList} object.
  When \code{subject} is an \link{IntervalForest} it returns a
  \link{CompressedIntegerList}.

  \code{overlapsAny} finds the ranges in \code{query} that overlap any
  of the ranges in \code{subject}. For \link{Ranges} or \link{Views}
  objects, it returns a logical vector of length equal to the number of
  ranges in \code{query}. For \link{RangesList}, \link{RangedData}, or
  \link{ViewsList} objects, it returns a \link{LogicalList} object,
  where each element of the result corresponds to a space in \code{query}.
  When \code{subject} is an \link{IntervalForest} object, it returns
  a \link{CompressedLogicalList} object.

  \code{\%over\%} and \code{\%within\%} are convenience wrappers for the
  2 most common use cases. Currently defined as
  \code{`\%over\%` <- function(query, subject) overlapsAny(query, subject)}
  and
  \code{`\%within\%` <- function(query, subject)
                        overlapsAny(query, subject,
  type="within")}. \code{\%outside\%} is simply the inverse of \code{\%over\%}.

  \code{subsetByOverlaps} returns the subset of \code{query} that
  has an overlap hit with a range in \code{subject} using the specified
  \code{findOverlaps} parameters.

  \code{mergeByOverlaps} computes the overlap between query and subject
  according to the arguments in \code{\dots}. It then extracts the
  corresponding hits from each object and returns a \code{DataFrame}
  containing one column for the query and one for the subject, as well
  as any \code{mcols} that were present on either object. The query and
  subject columns are named by quoting and deparsing the corresponding
  argument.
  
  \code{ranges(x, query, subject)} returns a \code{Ranges} of the same
  length as \link{Hits} object \code{x} holding the regions of intersection
  between the overlapping ranges in objects \code{query} and \code{subject},
  which should be the same query and subject used in the call to
  \code{findOverlaps} that generated \code{x}.
}

\references{
  Allen's Interval Algebra:
  James F. Allen: Maintaining knowledge about temporal intervals. In:
  Communications of the ACM. 26/11/1983. ACM Press. S. 832-843, ISSN 0001-0782
}

\author{Michael Lawrence with contributions by Hector Corrada Bravo}

\seealso{
  \itemize{
    \item The \link{Hits} and \linkS4class{HitsList} classes for representing a set of hits between
          2 vector-like objects.

    \item \link[GenomicRanges]{findOverlaps,GenomicRanges,GenomicRanges-method}
          in the GenomicRanges package for methods that operate on
          \link[GenomicRanges]{GRanges} or \link[GenomicRanges]{GRangesList}
          objects.

    \item \link[GenomicRanges]{findOverlaps,GenomicRanges,GIntervalTree-method} in
          the GenomicRanges package for methods that use \link{IntervalForest} objects
          to find overlaps.

    \item The \code{\link{IntervalTree}} class and constructor.

    \item The \code{\link{IntervalForest}} class and constructor.

    \item The \link{Ranges}, \link{Views}, \link{RangesList},
          \link{ViewsList}, and \link{RangedData} classes.

    \item The \link{IntegerList} and \link{LogicalList} classes.
  }
}

\examples{
query <- IRanges(c(1, 4, 9), c(5, 7, 10))
subject <- IRanges(c(2, 2, 10), c(2, 3, 12))
tree <- IntervalTree(subject)

## ---------------------------------------------------------------------
## findOverlaps()
## ---------------------------------------------------------------------

## at most one hit per query
findOverlaps(query, tree, select="first")
findOverlaps(query, tree, select="last")
findOverlaps(query, tree, select="arbitrary")

## overlap even if adjacent only
## (FIXME: the gap between 2 adjacent ranges should be still considered
## 0. So either we have an argument naming problem, or we should modify
## the handling of the 'maxgap' argument so that the user would need to
## specify maxgap=0L to obtain the result below.)
findOverlaps(query, tree, maxgap=1L)

## shortcut
findOverlaps(query, subject)

query <- IRanges(c(1, 4, 9), c(5, 7, 10))
subject <- IRanges(c(2, 2), c(5, 4))
tree <- IntervalTree(subject)

## one Ranges with itself
findOverlaps(query)

## single points as query
subject <- IRanges(c(1, 6, 13), c(4, 9, 14))
findOverlaps(c(3L, 7L, 10L), subject, select="first")

## alternative overlap types
query <- IRanges(c(1, 5, 3, 4), width=c(2, 2, 4, 6))
subject <- IRanges(c(1, 3, 5, 6), width=c(4, 4, 5, 4))

findOverlaps(query, subject, type="start")
findOverlaps(query, subject, type="start", maxgap=1L)
findOverlaps(query, subject, type="end", select="first")
ov <- findOverlaps(query, subject, type="within", maxgap=1L)
ov

## ---------------------------------------------------------------------
## overlapsAny()
## ---------------------------------------------------------------------

overlapsAny(query, subject, type="start")
overlapsAny(query, subject, type="end")
query \%over\% subject    # same as overlapsAny(query, subject)
query \%within\% subject  # same as overlapsAny(query, subject,
                        #                     type="within")

## ---------------------------------------------------------------------
## "ranges" METHOD FOR Hits OBJECTS
## ---------------------------------------------------------------------

## extract the regions of intersection between the overlapping ranges
ranges(ov, query, subject)

## ---------------------------------------------------------------------
## using IntervalForest objects
## ---------------------------------------------------------------------
query <- IRanges(c(1, 4, 9), c(5, 7, 10))
qpartition <- factor(c("a","a","b"))
qlist <- split(query, qpartition)

subject <- IRanges(c(2, 2, 10), c(2, 3, 12))
spartition <- factor(c("a","a","b"))
slist <- split(subject, spartition)

forest <- IntervalForest(slist)

## at most one hit per query
findOverlaps(qlist, forest, select="first")
findOverlaps(qlist, forest, select="last")
findOverlaps(qlist, forest, select="arbitrary")

query <- IRanges(c(1, 5, 3, 4), width=c(2, 2, 4, 6))
qpartition <- factor(c("a","a","b","b"))
qlist <- split(query, qpartition)

subject <- IRanges(c(1, 3, 5, 6), width=c(4, 4, 5, 4))
spartition <- factor(c("a","a","b","b"))
slist <- split(subject, spartition)
forest <- IntervalForest(slist)

overlapsAny(qlist, forest, type="start")
overlapsAny(qlist, forest, type="end")
qlist %over% forest

subsetByOverlaps(qlist, forest)
countOverlaps(qlist, forest)

}

\keyword{methods}
