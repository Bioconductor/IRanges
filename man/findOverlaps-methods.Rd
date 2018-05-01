\name{findOverlaps-methods}

\alias{findOverlaps-methods}

\alias{findOverlaps}
\alias{findOverlaps,IntegerRanges,IntegerRanges-method}
\alias{findOverlaps,integer,IntegerRanges-method}
\alias{findOverlaps,Vector,missing-method}
\alias{findOverlaps,IntegerRangesList,IntegerRangesList-method}
\alias{findOverlaps,RangedData,RangedData-method}
\alias{findOverlaps,RangedData,IntegerRangesList-method}
\alias{findOverlaps,IntegerRangesList,RangedData-method}
\alias{findOverlaps,Pairs,missing-method}
\alias{findOverlaps,Pairs,ANY-method}
\alias{findOverlaps,ANY,Pairs-method}
\alias{findOverlaps,Pairs,Pairs-method}

\alias{countOverlaps}
\alias{countOverlaps,Vector,Vector-method}
\alias{countOverlaps,integer,Vector-method}
\alias{countOverlaps,Vector,missing-method}
\alias{countOverlaps,IntegerRanges,IntegerRanges-method}
\alias{countOverlaps,IntegerRangesList,IntegerRangesList-method}
\alias{countOverlaps,RangedData,RangedData-method}
\alias{countOverlaps,RangedData,IntegerRangesList-method}
\alias{countOverlaps,IntegerRangesList,RangedData-method}

\alias{overlapsAny}
\alias{overlapsAny,Vector,Vector-method}
\alias{overlapsAny,integer,Vector-method}
\alias{overlapsAny,Vector,missing-method}
\alias{overlapsAny,IntegerRangesList,IntegerRangesList-method}

\alias{\%over\%}
\alias{\%within\%}
\alias{\%outside\%}

\alias{subsetByOverlaps}
\alias{subsetByOverlaps,Vector,Vector-method}

\alias{overlapsRanges}
\alias{overlapsRanges,IntegerRanges,IntegerRanges-method}
\alias{overlapsRanges,IntegerRangesList,IntegerRangesList-method}

\alias{poverlaps}
\alias{poverlaps,IntegerRanges,IntegerRanges-method}
\alias{poverlaps,IntegerRanges,integer-method}
\alias{poverlaps,integer,IntegerRanges-method}

\alias{mergeByOverlaps}

\alias{findOverlapPairs}

\title{Finding overlapping ranges}

\description{
  Various methods for finding/counting interval overlaps between two
  "range-based" objects: a query and a subject.

  NOTE: This man page describes the methods that operate on
  \link{IntegerRanges} and \link{IntegerRangesList} derivatives. See
  \code{?`\link[GenomicRanges]{findOverlaps,GenomicRanges,GenomicRanges-method}`}
  in the \pkg{GenomicRanges} package for methods that operate on
  \link[GenomicRanges]{GenomicRanges} or \link[GenomicRanges]{GRangesList}
  objects.
}

\usage{
findOverlaps(query, subject, maxgap=-1L, minoverlap=0L,
             type=c("any", "start", "end", "within", "equal"),
             select=c("all", "first", "last", "arbitrary"),
             ...)

countOverlaps(query, subject, maxgap=-1L, minoverlap=0L,
              type=c("any", "start", "end", "within", "equal"),
              ...)

overlapsAny(query, subject, maxgap=-1L, minoverlap=0L,
            type=c("any", "start", "end", "within", "equal"),
            ...)
query \%over\% subject
query \%within\% subject
query \%outside\% subject

subsetByOverlaps(x, ranges, maxgap=-1L, minoverlap=0L,
                 type=c("any", "start", "end", "within", "equal"),
                 invert=FALSE,
                 ...)

overlapsRanges(query, subject, hits=NULL, ...)

poverlaps(query, subject, maxgap = 0L, minoverlap = 1L,
          type = c("any", "start", "end", "within", "equal"),
          ...)

mergeByOverlaps(query, subject, ...)

findOverlapPairs(query, subject, ...)
}

\arguments{
  \item{query, subject, x, ranges}{
    Each of them can be an \link{IntegerRanges} (e.g. \link{IRanges},
    \link{Views}) or \link{IntegerRangesList} (e.g. \link{IRangesList},
    \link{ViewsList}) derivative.
    In addition, if \code{subject} or \code{ranges} is an \link{IntegerRanges}
    object, \code{query} or \code{x} can be an integer vector to be converted
    to length-one ranges.

    If \code{query} (or \code{x}) is an \link{IntegerRangesList} object,
    then \code{subject} (or \code{ranges}) must also be an
    \link{IntegerRangesList} object.

    If both arguments are list-like objects with names, each list element
    from the 2nd argument is paired with the list element from the 1st
    argument with the matching name, if any. Otherwise, list elements are
    paired by position. The overlap is then computed between the pairs as
    described below.

    If \code{subject} is omitted, \code{query} is queried against
    itself. In this case, and only this case, the \code{drop.self}
    and \code{drop.redundant} arguments are allowed. By default,
    the result will contain hits for each range against itself, and if
    there is a hit from A to B, there is also a hit for B to A. If
    \code{drop.self} is \code{TRUE}, all self matches are dropped. If
    \code{drop.redundant} is \code{TRUE}, only one of A->B and B->A
    is returned.
  }
  \item{maxgap}{
    A single integer >= -1.

    If \code{type} is set to \code{"any"}, \code{maxgap} is interpreted as
    the maximum \emph{gap} that is allowed between 2 ranges for the ranges
    to be considered as overlapping. The \emph{gap} between 2 ranges
    is the number of positions that separate them. The \emph{gap} between
    2 adjacent ranges is 0. By convention when one range has its start or
    end strictly inside the other (i.e. non-disjoint ranges), the \emph{gap}
    is considered to be -1.

    If \code{type} is set to anything else, \code{maxgap} has a special
    meaning that depends on the particular \code{type}. See \code{type}
    below for more information.
  }
  \item{minoverlap}{
    A single non-negative integer.

    Only ranges with a minimum of \code{minoverlap} overlapping positions
    are considered to be overlapping.

    When \code{type} is \code{"any"}, at least one of \code{maxgap} and
    \code{minoverlap} must be set to its default value.
  }
  \item{type}{
    By default, any overlap is accepted. By specifying the \code{type}
    parameter, one can select for specific types of overlap. The types
    correspond to operations in Allen's Interval Algebra (see
    references). If \code{type} is \code{start} or \code{end}, the
    intervals are required to have matching starts or ends,
    respectively. Specifying \code{equal} as the type returns the
    intersection of the \code{start} and \code{end} matches. If
    \code{type} is \code{within}, the query interval must be wholly
    contained within the subject interval. Note that all matches must
    additionally satisfy the \code{minoverlap} constraint described above.

    The \code{maxgap} parameter has special meaning with the special
    overlap types. For \code{start}, \code{end}, and \code{equal},
    it specifies the maximum difference in the starts, ends or both,
    respectively. For \code{within}, it is the maximum amount by which
    the subject may be wider than the query. If \code{maxgap} is set to -1
    (the default), it's replaced internally by 0.
  }
  \item{select}{
    If \code{query} is an \link{IntegerRanges} derivative:
    When \code{select} is \code{"all"} (the default), the results are
    returned as a \link[S4Vectors]{Hits} object.
    Otherwise the returned value is an integer vector \emph{parallel} to
    \code{query} (i.e. same length) containing the first, last,
    or arbitrary overlapping interval in \code{subject}, with \code{NA}
    indicating intervals that did not overlap any intervals in \code{subject}.

    If \code{query} is an \link{IntegerRangesList} derivative:
    When \code{select} is \code{"all"} (the default), the results are
    returned as a \link[S4Vectors]{HitsList} object.
    Otherwise the returned value depends on the \code{drop} argument.
    When \code{select != "all" && !drop}, an \link{IntegerList} is returned,
    where each element of the result corresponds to a space in \code{query}.
    When \code{select != "all" && drop}, an integer vector is returned
    containing indices that are offset to align with the unlisted \code{query}.
  }
  \item{invert}{
    If \code{TRUE}, keep only the ranges in \code{x} that do \emph{not}
    overlap \code{ranges}.
  }
  \item{hits}{
    The \link[S4Vectors]{Hits} or \link[S4Vectors]{HitsList} object returned
    by \code{findOverlaps}, or \code{NULL}. If \code{NULL} then \code{hits}
    is computed by calling \code{findOverlaps(query, subject, ...)} internally
    (the extra arguments passed to \code{overlapsRanges} are passed to
    \code{findOverlaps}).
  }
  \item{...}{
    Further arguments to be passed to or from other methods:
    \itemize{
      \item \code{drop}: Supported only when \code{query} is an
            \link{IntegerRangesList} derivative.
            \code{FALSE} by default. See \code{select} argument above for the
            details.
      \item \code{drop.self}, \code{drop.redundant}: When \code{subject}
            is omitted, the \code{drop.self} and \code{drop.redundant}
            arguments (both \code{FALSE} by default) are allowed.
            See \code{query} and \code{subject} arguments above for the
            details.
    }
  }
}

\details{
  A common type of query that arises when working with intervals is
  finding which intervals in one set overlap those in another.

  The simplest approach is to call the \code{findOverlaps} function
  on a \link{IntegerRanges} or other object with range information (aka
  "range-based object").
}

\value{
  For \code{findOverlaps}: see \code{select} argument above.

  For \code{countOverlaps}: the overlap hit count for each range
  in \code{query} using the specified \code{findOverlaps} parameters.
  For \link{IntegerRangesList} objects, it returns an \link{IntegerList}
  object.

  \code{overlapsAny} finds the ranges in \code{query} that overlap any
  of the ranges in \code{subject}. For \link{IntegerRanges} derivatives,
  it returns a logical vector of length equal to the number of
  ranges in \code{query}. For \link{IntegerRangesList} derivatives,
  it returns a \link{LogicalList} object where each element of the result
  corresponds to a space in \code{query}.

  \code{\%over\%} and \code{\%within\%} are convenience wrappers for the
  2 most common use cases. Currently defined as
  \code{`\%over\%` <- function(query, subject) overlapsAny(query, subject)}
  and
  \code{`\%within\%` <- function(query, subject)
                        overlapsAny(query, subject,
  type="within")}. \code{\%outside\%} is simply the inverse of \code{\%over\%}.

  \code{subsetByOverlaps} returns the subset of \code{x} that
  has an overlap hit with a range in \code{ranges} using the specified
  \code{findOverlaps} parameters.

  When \code{hits} is a \link[S4Vectors]{Hits} (or \link[S4Vectors]{HitsList})
  object, \code{overlapsRanges(query, subject, hits)} returns a
  \link{IntegerRanges} (or \link{IntegerRangesList}) object of the \emph{same
  shape} as \code{hits} holding the regions of intersection between the
  overlapping ranges in objects \code{query} and \code{subject}, which should
  be the same query and subject used in the call to \code{findOverlaps} that
  generated \code{hits}.
  \emph{Same shape} means same length when \code{hits} is a
  \link[S4Vectors]{Hits} object, and same length and same elementNROWS
  when \code{hits} is a \link[S4Vectors]{HitsList} object.

  \code{poverlaps} compares \code{query} and \code{subject} in parallel
  (like e.g., \code{pmin}) and returns a logical vector indicating
  whether each pair of ranges overlaps. Integer vectors are treated as
  width-one ranges.
  
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

    \item The \link{IntegerRanges}, \link{Views}, \link{IntegerRangesList},
          and \link{ViewsList} classes.

    \item The \link{IntegerList} and \link{LogicalList} classes.
  }
}

\examples{
## ---------------------------------------------------------------------
## findOverlaps()
## ---------------------------------------------------------------------

query <- IRanges(c(1, 4, 9), c(5, 7, 10))
subject <- IRanges(c(2, 2, 10), c(2, 3, 12))

findOverlaps(query, subject)

## at most one hit per query
findOverlaps(query, subject, select="first")
findOverlaps(query, subject, select="last")
findOverlaps(query, subject, select="arbitrary")

## including adjacent ranges in the result
findOverlaps(query, subject, maxgap=0L)

query <- IRanges(c(1, 4, 9), c(5, 7, 10))
subject <- IRanges(c(2, 2), c(5, 4))

## one IRanges object with itself
findOverlaps(query)

## single points as query
subject <- IRanges(c(1, 6, 13), c(4, 9, 14))
findOverlaps(c(3L, 7L, 10L), subject, select="first")

## special overlap types
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
## overlapsRanges()
## ---------------------------------------------------------------------

## Extract the regions of intersection between the overlapping ranges:
overlapsRanges(query, subject, ov)

## ---------------------------------------------------------------------
## Using IntegerRangesList objects
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
