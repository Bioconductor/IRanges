\name{findOverlaps-methods}

\alias{findOverlaps-methods}

\alias{findOverlaps}
\alias{findOverlaps,Ranges,Ranges-method}
\alias{findOverlaps,Vector,missing-method}
\alias{findOverlaps,integer,Ranges-method}
\alias{findOverlaps,Views,Views-method}
\alias{findOverlaps,Views,Vector-method}
\alias{findOverlaps,Vector,Views-method}
\alias{findOverlaps,RangesList,RangesList-method}
\alias{findOverlaps,ViewsList,ViewsList-method}
\alias{findOverlaps,ViewsList,Vector-method}
\alias{findOverlaps,Vector,ViewsList-method}
\alias{findOverlaps,RangedData,RangedData-method}
\alias{findOverlaps,RangedData,RangesList-method}
\alias{findOverlaps,RangesList,RangedData-method}
\alias{findOverlaps,Pairs,missing-method}
\alias{findOverlaps,Pairs,ANY-method}
\alias{findOverlaps,ANY,Pairs-method}
\alias{findOverlaps,Pairs,Pairs-method}

\alias{countOverlaps}
\alias{countOverlaps,Vector,Vector-method}
\alias{countOverlaps,Vector,missing-method}
\alias{countOverlaps,Ranges,Ranges-method}
\alias{countOverlaps,RangesList,RangesList-method}
\alias{countOverlaps,ViewsList,ViewsList-method}
\alias{countOverlaps,ViewsList,Vector-method}
\alias{countOverlaps,Vector,ViewsList-method}
\alias{countOverlaps,RangedData,RangedData-method}
\alias{countOverlaps,RangedData,RangesList-method}
\alias{countOverlaps,RangesList,RangedData-method}

\alias{overlapsAny}
\alias{overlapsAny,Vector,Vector-method}
\alias{overlapsAny,Vector,missing-method}
\alias{overlapsAny,RangesList,RangesList-method}
\alias{overlapsAny,ViewsList,ViewsList-method}
\alias{overlapsAny,ViewsList,Vector-method}
\alias{overlapsAny,Vector,ViewsList-method}
\alias{overlapsAny,RangedData,RangedData-method}
\alias{overlapsAny,RangedData,RangesList-method}
\alias{overlapsAny,RangesList,RangedData-method}

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
\alias{ranges,HitsList-method}

\alias{findOverlapPairs}

\alias{poverlaps}
\alias{poverlaps,Ranges,Ranges-method}
\alias{poverlaps,Ranges,integer-method}
\alias{poverlaps,integer,Ranges-method}

\title{Finding overlapping ranges}

\description{
  Various methods for finding/counting interval overlaps between two
  "range-based" objects: a query and a subject.

  NOTE: This man page describes the methods that operate on \link{Ranges},
  \link{Views}, \link{RangesList}, or \link{ViewsList} objects. See
  \code{?`\link[GenomicRanges]{findOverlaps,GenomicRanges,GenomicRanges-method}`}
  in the \pkg{GenomicRanges} package for methods that operate on
  \link[GenomicRanges]{GenomicRanges} or \link[GenomicRanges]{GRangesList}
  objects.
}

\usage{
findOverlaps(query, subject, maxgap=0L, minoverlap=1L,
             type=c("any", "start", "end", "within", "equal"),
             select=c("all", "first", "last", "arbitrary"),
             ...)

countOverlaps(query, subject, maxgap=0L, minoverlap=1L,
              type=c("any", "start", "end", "within", "equal"),
              ...)

overlapsAny(query, subject, maxgap=0L, minoverlap=1L,
            type=c("any", "start", "end", "within", "equal"),
            ...)
query \%over\% subject
query \%within\% subject
query \%outside\% subject

subsetByOverlaps(query, subject, maxgap=0L, minoverlap=1L,
                 type=c("any", "start", "end", "within", "equal"),
                 invert=FALSE,
                 ...)

mergeByOverlaps(query, subject, ...)
findOverlapPairs(query, subject, ...)

poverlaps(query, subject, maxgap = 0L, minoverlap = 1L,
          type = c("any", "start", "end", "within", "equal"),
          ...)

\S4method{ranges}{Hits}(x, query, subject)
\S4method{ranges}{HitsList}(x, query, subject)
}

\arguments{
  \item{query, subject}{
    Each of them can be a \link{Ranges}, \link{Views}, \link{RangesList},
    \link{ViewsList}, or \link{RangedData} object.
    In addition, if \code{subject} is a \link{Ranges} object, \code{query}
    can be an  integer vector to be converted to length-one ranges.

    If \code{query} is a \link{RangesList} or \link{RangedData},
    \code{subject} must be a \link{RangesList} or \link{RangedData}.
    If both lists have names, each element from the subject is paired
    with the element from the query with the matching name, if any.
    Otherwise, elements are paired by position. The overlap is then
    computed between the pairs as described below.

    If \code{subject} is omitted, \code{query} is queried against
    itself. In this case, and only this case, the \code{drop.self}
    and \code{drop.redundant} arguments are allowed. By default,
    the result will contain hits for each range against itself, and if
    there is a hit from A to B, there is also a hit for B to A. If
    \code{drop.self} is \code{TRUE}, all self matches are dropped. If
    \code{drop.redundant} is \code{TRUE}, only one of A->B and B->A
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
    the subject may be wider than the query.
  }
  \item{select}{
    If \code{query} is a \link{Ranges} or \link{Views} object:
    When \code{select} is \code{"all"} (the default), the results are
    returned as a \link[S4Vectors]{Hits} object.
    Otherwise the returned value is an integer vector parallel to \code{query}
    (i.e. same length) containing the first, last, or arbitrary overlapping
    interval in \code{subject}, with \code{NA} indicating intervals that did
    not overlap any intervals in \code{subject}.

    If \code{query} is a \link{RangesList}, \link{ViewsList}, or
    \link{RangedData} object:
    When \code{select} is \code{"all"} (the default), the results are
    returned as a \link[S4Vectors]{HitsList} object.
    Otherwise the returned value depends on the \code{drop} argument.
    When \code{select != "all" && !drop}, an \link{IntegerList} is returned,
    where each element of the result corresponds to a space in \code{query}.
    When \code{select != "all" && drop}, an integer vector is returned
    containing indices that are offset to align with the unlisted \code{query}.
  }
  \item{invert}{
    If \code{TRUE}, keep only the query ranges that do \emph{not}
    overlap the subject.
  }
  \item{...}{
    Further arguments to be passed to or from other methods:
    \itemize{
      \item \code{drop}: Supported only when \code{query} is a
            \link{RangesList}, \link{ViewsList}, or \link{RangedData} object.
            \code{FALSE} by default. See \code{select} argument above for the
            details.
      \item \code{drop.self}, \code{drop.redundant}: When \code{subject}
            is omitted, the \code{drop.self} and \code{drop.redundant}
            arguments (both \code{FALSE} by default) are allowed.
            See \code{query} and \code{subject} arguments above for the
            details.
    }
  }
  \item{x}{
    \link[S4Vectors]{Hits} or \link[S4Vectors]{HitsList} object returned
    by \code{findOverlaps}.
  }
}

\details{
  A common type of query that arises when working with intervals is
  finding which intervals in one set overlap those in another.

  The simplest approach is to call the \code{findOverlaps} function
  on a \link{Ranges} or other object with range information (aka
  "range-based object").
}

\value{
  For \code{findOverlaps}: see \code{select} argument above.

  For \code{countOverlaps}: the overlap hit count for each range
  in \code{query} using the specified \code{findOverlaps} parameters.
  For \link{RangesList} objects, it returns an \link{IntegerList} object.

  \code{overlapsAny} finds the ranges in \code{query} that overlap any
  of the ranges in \code{subject}. For \link{Ranges} or \link{Views}
  objects, it returns a logical vector of length equal to the number of
  ranges in \code{query}. For \link{RangesList}, \link{RangedData}, or
  \link{ViewsList} objects, it returns a \link{LogicalList} object,
  where each element of the result corresponds to a space in \code{query}.

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

  \code{findOverlapPairs} is like \code{mergeByOverlaps}, except it
  returns a formal \code{\link[S4Vectors:Pairs-class]{Pairs}} object
  that provides useful downstream conveniences, such as finding the
  intersection of the overlapping ranges with \code{\link{pintersect}}.

  \code{poverlaps} compares \code{query} and \code{subject} in parallel
  (like e.g., \code{pmin}) and returns a logical vector indicating
  whether each pair of ranges overlaps. Integer vectors are treated as
  width-one ranges.
  
  When \code{x} is a \link[S4Vectors]{Hits} (or \link[S4Vectors]{HitsList})
  object, \code{ranges(x, query, subject)} returns a \link{Ranges}
  (or \link{RangesList}) object of the \emph{same shape} as \code{x}
  holding the regions of intersection between the overlapping ranges
  in objects \code{query} and \code{subject}, which should be the same
  query and subject used in the call to \code{findOverlaps} that generated
  \code{x}.
  \emph{Same shape} means same length when \code{x} is a
  \link[S4Vectors]{Hits} object, and same length and same elementNROWS
  when \code{x} is a \link[S4Vectors]{HitsList} object.
}

\references{
  Allen's Interval Algebra:
  James F. Allen: Maintaining knowledge about temporal intervals. In:
  Communications of the ACM. 26/11/1983. ACM Press. S. 832-843, ISSN 0001-0782
}

\author{Michael Lawrence and Hervé Pagès}

\seealso{
  \itemize{
    \item \link[S4Vectors]{Hits} and \link[S4Vectors]{HitsList} objects
          in the \pkg{S4Vectors} package for representing a set of hits
          between 2 vector-like or list-like objects.

    \item \link[GenomicRanges]{findOverlaps,GenomicRanges,GenomicRanges-method}
          in the \pkg{GenomicRanges} package for methods that operate on
          \link[GenomicRanges]{GRanges} or \link[GenomicRanges]{GRangesList}
          objects.

    \item The \link{NCList} class and constructor.

    \item The \link{Ranges}, \link{Views}, \link{RangesList}, and
          \link{ViewsList} classes.

    \item The \link{IntegerList} and \link{LogicalList} classes.
  }
}

\examples{
query <- IRanges(c(1, 4, 9), c(5, 7, 10))
subject <- IRanges(c(2, 2, 10), c(2, 3, 12))

## ---------------------------------------------------------------------
## findOverlaps()
## ---------------------------------------------------------------------

## at most one hit per query
findOverlaps(query, subject, select="first")
findOverlaps(query, subject, select="last")
findOverlaps(query, subject, select="arbitrary")

## overlap even if adjacent only
## (FIXME: the gap between 2 adjacent ranges should be still considered
## 0. So either we have an argument naming problem, or we should modify
## the handling of the 'maxgap' argument so that the user would need to
## specify maxgap=0L to obtain the result below.)
findOverlaps(query, subject, maxgap=1L)

## shortcut
findOverlaps(query, subject)

query <- IRanges(c(1, 4, 9), c(5, 7, 10))
subject <- IRanges(c(2, 2), c(5, 4))

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

## Using pairs to find intersection of overlapping ranges
hits <- findOverlaps(query, subject)
p <- Pairs(query, subject, hits=hits)
pintersect(p)

## Shortcut
p <- findOverlapPairs(query, subject)
pintersect(p)

## ---------------------------------------------------------------------
## overlapsAny()
## ---------------------------------------------------------------------

overlapsAny(query, subject, type="start")
overlapsAny(query, subject, type="end")
query \%over\% subject    # same as overlapsAny(query, subject)
query \%within\% subject  # same as overlapsAny(query, subject,
                        #                     type="within")

## ---------------------------------------------------------------------
## "ranges" METHODS FOR Hits OR HitsList OBJECTS
## ---------------------------------------------------------------------

## extract the regions of intersection between the overlapping ranges
ranges(ov, query, subject)

## ---------------------------------------------------------------------
## Using RangesList objects
## ---------------------------------------------------------------------
query <- IRanges(c(1, 4, 9), c(5, 7, 10))
qpartition <- factor(c("a","a","b"))
qlist <- split(query, qpartition)

subject <- IRanges(c(2, 2, 10), c(2, 3, 12))
spartition <- factor(c("a","a","b"))
slist <- split(subject, spartition)

## at most one hit per query
findOverlaps(qlist, slist, select="first")
findOverlaps(qlist, slist, select="last")
findOverlaps(qlist, slist, select="arbitrary")

query <- IRanges(c(1, 5, 3, 4), width=c(2, 2, 4, 6))
qpartition <- factor(c("a","a","b","b"))
qlist <- split(query, qpartition)

subject <- IRanges(c(1, 3, 5, 6), width=c(4, 4, 5, 4))
spartition <- factor(c("a","a","b","b"))
slist <- split(subject, spartition)

overlapsAny(qlist, slist, type="start")
overlapsAny(qlist, slist, type="end")
qlist %over% slist

subsetByOverlaps(qlist, slist)
countOverlaps(qlist, slist)
}

\keyword{methods}
