\name{inter-range-methods}

\alias{inter-range-methods}

\alias{range}
\alias{range,Ranges-method}
\alias{range,IPos-method}
\alias{range,RangesList-method}
\alias{range,CompressedIRangesList-method}
\alias{range,RangedData-method}

\alias{reduce}
\alias{reduce,Ranges-method}
\alias{reduce,Views-method}
\alias{reduce,RangesList-method}
\alias{reduce,CompressedIRangesList-method}

\alias{gaps}
\alias{gaps,Ranges-method}
\alias{gaps,Views-method}
\alias{gaps,RangesList-method}
\alias{gaps,CompressedIRangesList-method}
\alias{gaps,MaskCollection-method}

\alias{disjoin}
\alias{disjoin,Ranges-method}
\alias{disjoin,NormalIRanges-method}
\alias{disjoin,RangesList-method}
\alias{disjoin,CompressedIRangesList-method}

\alias{isDisjoint}
\alias{isDisjoint,Ranges-method}
\alias{isDisjoint,IPos-method}
\alias{isDisjoint,NormalIRanges-method}
\alias{isDisjoint,RangesList-method}

\alias{disjointBins}
\alias{disjointBins,Ranges-method}
\alias{disjointBins,NormalIRanges-method}
\alias{disjointBins,RangesList-method}


\title{Inter range transformations of a Ranges, Views, RangesList,
       MaskCollection, or RangedData object}

\description{
  Range-based transformations are grouped in 2 categories:
  \enumerate{
    \item \emph{Intra range transformations} (e.g. \code{\link{shift}()})
          transform each range individually (and independently of the other
          ranges). They return an object \emph{parallel} to the input object,
          that is, where the i-th range corresponds to the i-th range in the
          input.
          Those transformations are described in the \link{intra-range-methods}
          man page (see \code{?`\link{intra-range-methods}`}).

    \item \emph{Inter range transformations} (e.g. \code{reduce()})
          transform all the ranges together as a set to produce a new set
          of ranges. They return an object that is generally \emph{NOT}
          parallel to the input object.
          Those transformations are described below.
  }
}

\usage{
## range()
## -------
\S4method{range}{Ranges}(x, ..., with.revmap=FALSE, na.rm=FALSE)

\S4method{range}{RangesList}(x, ..., with.revmap=FALSE, na.rm=FALSE)

## reduce()
## --------
reduce(x, drop.empty.ranges=FALSE, ...)

\S4method{reduce}{Ranges}(x, drop.empty.ranges=FALSE, min.gapwidth=1L,
       with.revmap=FALSE, with.inframe.attrib=FALSE)

\S4method{reduce}{Views}(x, drop.empty.ranges=FALSE, min.gapwidth=1L,
       with.revmap=FALSE, with.inframe.attrib=FALSE)

\S4method{reduce}{RangesList}(x, drop.empty.ranges=FALSE, min.gapwidth=1L,
       with.revmap=FALSE, with.inframe.attrib=FALSE)

## gaps()
## ------
gaps(x, start=NA, end=NA)

## disjoin(), isDisjoint(), and disjointBins()
## -------------------------------------------
disjoin(x, ...)

\S4method{disjoin}{Ranges}(x, with.revmap=FALSE)
\S4method{disjoin}{RangesList}(x, with.revmap=FALSE)

isDisjoint(x, ...)
disjointBins(x, ...)
}

\arguments{
  \item{x}{
    A \link{Ranges} or \link{RangesList} object for \code{range},
    \code{disjoin}, \code{isDisjoint}, and \code{disjointBins}.

    A \link{Ranges}, \link{Views}, or \link{RangesList} object for
    \code{reduce} and \code{gaps}.
  }
  \item{...}{
    For \code{range}, additional \link{Ranges} or \link{RangesList}
    object to consider.
  }
  \item{na.rm}{
    Ignored.
  }
  \item{drop.empty.ranges}{
    \code{TRUE} or \code{FALSE}. Should empty ranges be dropped?
  }
  \item{min.gapwidth}{
    Ranges separated by a gap of at least \code{min.gapwidth} positions
    are not merged.
  }
  \item{with.revmap}{
    \code{TRUE} or \code{FALSE}. Should the mapping from output to input
    ranges be stored in the returned object? If yes, then it is stored as
    metadata column \code{revmap} of type \link{IntegerList}.
  }
  \item{with.inframe.attrib}{
    \code{TRUE} or \code{FALSE}. For internal use.
  }
  \item{start, end}{
    \itemize{
      \item If \code{x} is a \link{Ranges} or \link{Views} object:
            A single integer or \code{NA}. Use these arguments to specify
            the interval of reference i.e. which interval the returned gaps
            should be relative to.
      \item If \code{x} is a \link{RangesList} object:
            Integer vectors containing the coordinate bounds for each
            \link{RangesList} top-level element.
    }
  }
}

\details{
  Unless specified otherwise, when \code{x} is a \link{RangesList} object,
  any transformation described here is equivalent to applying the
  transformation to each \link{RangesList} top-level element separately.

  \subsection{reduce}{

    \code{reduce} first orders the ranges in \code{x} from left to right,
    then merges the overlapping or adjacent ones.

  }\subsection{range}{

    \code{range} first combines \code{x} and the arguments in \code{...}.
    If the combined \link{IRanges} object contains at least 1 range,
    then \code{range} returns an \link{IRanges} instance with a single
    range, from the minimum start to the maximum end of the combined object.
    Otherwise (i.e. if the combined object contains no range),
    \code{IRanges()} is returned (i.e. an \link{IRanges} instance of
    length 0).

    When passing more than 1 \link{RangesList} object to \code{range()},
    they are first merged into a single \link{RangesList} object: by name
    if all objects have names, otherwise, if they are all of the same
    length, by position. Else, an exception is thrown.

    If \code{x} is a \link{RangedData} object, then \code{range} returns
    a \link{RangesList} object resulting from calling \code{range(ranges(x))},
    i.e. the bounds of the ranges in each space.

  }\subsection{gaps}{

    \code{gaps} returns the "normal" \link{Ranges} object representing
    the set of integers that remain after the set of integers represented
    by \code{x} has been removed from the interval specified by the
    \code{start} and \code{end} arguments.

    If \code{x} is a \link{Views} object, then \code{start=NA} and
    \code{end=NA} are interpreted as \code{start=1} and
    \code{end=length(subject(x))}, respectively, so, if \code{start}
    and \code{end} are not specified, then gaps are extracted with respect
    to the entire subject.

  }\subsection{isDisjoint}{

    A \link{Ranges} object \code{x} is considered to be "disjoint" if its
    ranges are non-overlapping. \code{isDisjoint} tests whether the object
    is "disjoint" or not.

    Note that a "normal" \link{Ranges} object is always "disjoint" but the
    opposite is not true. See \code{?isNormal} for more information about
    normal \link{Ranges} objects.

    About empty ranges. \code{isDisjoint} handles empty ranges (a.k.a.
    zero-width ranges) as follow: single empty range A is considered to
    overlap with single range B iff it's contained in B without being on
    the edge of B (in which case it would be ambiguous whether A is
    contained in or adjacent to B). More precisely, single empty range A
    is considered to overlap with single range B iff
    \preformatted{    start(B) < start(A) and end(A) < end(B)}
    Because A is an empty range it verifies \code{end(A) = start(A) - 1}
    so the above is equivalent to:
    \preformatted{    start(B) < start(A) <= end(B)}
    and also equivalent to:
    \preformatted{    start(B) <= end(A) < end(B)}
    Finally, it is also equivalent to:
    \preformatted{    pcompare(A, B) == 2}
    See \code{?`\link{Ranges-comparison}`} for the meaning of the codes
    returned by the \code{\link{pcompare}} function.

  }\subsection{disjoin}{

    \code{disjoin} returns a disjoint object, by finding the union of the
    end points in \code{x}. In other words, the result consists of a range
    for every interval, of maximal length, over which the set of overlapping
    ranges in \code{x} is the same and at least of size 1.

  }\subsection{disjointBins}{

    \code{disjointBins} segregates \code{x} into a set of bins so that the
    ranges in each bin are disjoint. Lower-indexed bins are filled first.
    The method returns an integer vector indicating the bin index for each
    range.
  }
}

\value{
  If \code{x} is a \link{Ranges} object:
  \itemize{
    \item \code{range}, \code{reduce}, \code{gaps}, and \code{disjoin}
          return an \link{IRanges} instance.
    \item \code{isDisjoint} returns \code{TRUE} or \code{FALSE}.
    \item \code{disjointBins} returns an integer vector \emph{parallel} to
          \code{x}, that is, where the i-th element corresponds to the i-th
          element in \code{x}.
  }

  If \code{x} is a \link{Views} object: \code{reduce} and \code{gaps}
  return a \link{Views} object on the same subject as \code{x} but with
  modified views.

  If \code{x} is a \link{RangesList} object:
  \itemize{
    \item \code{range}, \code{reduce}, \code{gaps}, and \code{disjoin}
          return a \link{RangesList} object \emph{parallel} to \code{x}.
    \item \code{isDisjoint} returns a logical vector \emph{parallel} to
          \code{x}.
    \item \code{disjointBins} returns an \link{IntegerList} object
          \emph{parallel} to \code{x}.
  }
}

\author{H. Pagès, M. Lawrence, and P. Aboyoun}

\seealso{
  \itemize{
    \item \link{intra-range-methods} for intra range transformations.

    \item The \link{Ranges}, \link{Views}, \link{RangesList},
          and \link{MaskCollection} classes.

    \item The \link[GenomicRanges]{inter-range-methods} man page in the
          \pkg{GenomicRanges} package for \emph{inter range transformations}
          of genomic ranges.

    \item \link{setops-methods} for set operations on \link{IRanges}
          objects.

    \item \code{\link[S4Vectors]{endoapply}} in the \pkg{S4Vectors} package.
  }
}

\examples{
## ---------------------------------------------------------------------
## range()
## ---------------------------------------------------------------------

## On a Ranges object:
x <- IRanges(start=c(-2, 6, 9, -4, 1, 0, -6, 3, 10),
             width=c( 5, 0, 6,  1, 4, 3,  2, 0,  3))
range(x)

## On a RangesList object (XVector package required):
range1 <- IRanges(start=c(1, 2, 3), end=c(5, 2, 8))
range2 <- IRanges(start=c(15, 45, 20, 1), end=c(15, 100, 80, 5))
range3 <- IRanges(start=c(-2, 6, 7), width=c(8, 0, 0))  # with empty ranges
collection <- IRangesList(one=range1, range2, range3)
if (require(XVector)) {
    range(collection)
}

irl1 <- IRangesList(a=IRanges(c(1, 2),c(4, 3)), b=IRanges(c(4, 6),c(10, 7)))
irl2 <- IRangesList(c=IRanges(c(0, 2),c(4, 5)), a=IRanges(c(4, 5),c(6, 7)))
range(irl1, irl2)  # matched by names
names(irl2) <- NULL
range(irl1, irl2)  # now by position

## ---------------------------------------------------------------------
## reduce()
## ---------------------------------------------------------------------

## On a Ranges object:
reduce(x)
y <- reduce(x, with.revmap=TRUE)
mcols(y)$revmap  # an IntegerList

reduce(x, drop.empty.ranges=TRUE)
y <- reduce(x, drop.empty.ranges=TRUE, with.revmap=TRUE)
mcols(y)$revmap

## Use the mapping from reduced to original ranges to split the DataFrame
## of original metadata columns by reduced range:
ir0 <- IRanges(c(11:13, 2, 7:6), width=3)
mcols(ir0) <- DataFrame(id=letters[1:6], score=1:6)
ir <- reduce(ir0, with.revmap=TRUE)
ir
revmap <- mcols(ir)$revmap
revmap
relist(mcols(ir0)[unlist(revmap), ], revmap)  # a SplitDataFrameList

## On a RangesList object. These 4 are the same:
res1 <- reduce(collection)
res2 <- IRangesList(one=reduce(range1), reduce(range2), reduce(range3))
res3 <- do.call(IRangesList, lapply(collection, reduce))
res4 <- endoapply(collection, reduce)

stopifnot(identical(res2, res1))
stopifnot(identical(res3, res1))
stopifnot(identical(res4, res1))

reduce(collection, drop.empty.ranges=TRUE)

## ---------------------------------------------------------------------
## gaps()
## ---------------------------------------------------------------------

## On a Ranges object:
x0 <- IRanges(start=c(-2, 6, 9, -4, 1, 0, -6, 10),
              width=c( 5, 0, 6,  1, 4, 3,  2,  3))
gaps(x0)
gaps(x0, start=-6, end=20)

## On a Views object:
subject <- Rle(1:-3, 6:2)
v <- Views(subject, start=c(8, 3), end=c(14, 4))
gaps(v)

## On a RangesList object. These 4 are the same:
res1 <- gaps(collection)
res2 <- IRangesList(one=gaps(range1), gaps(range2), gaps(range3))
res3 <- do.call(IRangesList, lapply(collection, gaps))
res4 <- endoapply(collection, gaps)

stopifnot(identical(res2, res1))
stopifnot(identical(res3, res1))
stopifnot(identical(res4, res1))

## On a MaskCollection object:
mask1 <- Mask(mask.width=29, start=c(11, 25, 28), width=c(5, 2, 2))
mask2 <- Mask(mask.width=29, start=c(3, 10, 27), width=c(5, 8, 1))
mask3 <- Mask(mask.width=29, start=c(7, 12), width=c(2, 4))
mymasks <- append(append(mask1, mask2), mask3)
mymasks
gaps(mymasks)

## ---------------------------------------------------------------------
## disjoin()
## ---------------------------------------------------------------------

## On a Ranges object:
ir <- IRanges(c(1, 1, 4, 10), c(6, 3, 8, 10))
disjoin(ir)  # IRanges(c(1, 4, 7, 10), c(3, 6, 8, 10))
disjoin(ir, with.revmap=TRUE)

## On a RangesList object:
disjoin(collection)
disjoin(collection, with.revmap=TRUE)

## ---------------------------------------------------------------------
## isDisjoint()
## ---------------------------------------------------------------------

## On a Ranges object:
isDisjoint(IRanges(c(2,5,1), c(3,7,3)))  # FALSE
isDisjoint(IRanges(c(2,9,5), c(3,9,6)))  # TRUE
isDisjoint(IRanges(1, 5))  # TRUE

## Handling of empty ranges:
x <- IRanges(c(11, 16, 11, -2, 11), c(15, 29, 10, 10, 10))
stopifnot(isDisjoint(x))

## Sliding an empty range along a non-empty range:
sapply(11:17,
       function(i) pcompare(IRanges(i, width=0), IRanges(12, 15)))

sapply(11:17,
       function(i) isDisjoint(c(IRanges(i, width=0), IRanges(12, 15))))

## On a RangesList object:
isDisjoint(collection)

## ---------------------------------------------------------------------
## disjointBins()
## ---------------------------------------------------------------------

## On a Ranges object:
disjointBins(IRanges(1, 5))  # 1L
disjointBins(IRanges(c(3, 1, 10), c(5, 12, 13)))  # c(2L, 1L, 2L)

## On a RangesList object:
disjointBins(collection)
}

\keyword{utilities}
