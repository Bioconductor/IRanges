\name{inter-range-methods}

\alias{inter-range-methods}

\alias{range,Ranges-method}
\alias{range,RangesList-method}
\alias{range,CompressedIRangesList-method}
\alias{range,RangedData-method}
\alias{reduce}
\alias{reduce,IRanges-method}
\alias{reduce,Ranges-method}
\alias{reduce,Views-method}
\alias{reduce,RangesList-method}
\alias{reduce,CompressedIRangesList-method}
\alias{reduce,RangedData-method}
\alias{gaps}
\alias{gaps,IRanges-method}
\alias{gaps,Ranges-method}
\alias{gaps,Views-method}
\alias{gaps,RangesList-method}
\alias{gaps,CompressedIRangesList-method}
\alias{gaps,MaskCollection-method}
\alias{disjoin}
\alias{disjoin,Ranges-method}
\alias{disjoin,RangesList-method}
\alias{disjoin,CompressedIRangesList-method}
\alias{isDisjoint}
\alias{isDisjoint,Ranges-method}
\alias{isDisjoint,RangesList-method}
\alias{disjointBins}
\alias{disjointBins,Ranges-method}
\alias{disjointBins,RangesList-method}


\title{Inter range transformations of a Ranges, Views, RangesList,
       MaskCollection, or RangedData object}

\description{
  Except for \code{isDisjoint()} and \code{disjointBins()},
  all the transformations described in this man page are \emph{endomorphisms} 
  that operate on a single "range-based" object, that is, they transform the
  ranges contained in the input object and return them in an object of the
  \emph{same class} as the input object.

  Range-based endomorphisms are grouped in 2 categories:
  \enumerate{
    \item Intra range transformations like \code{\link{shift}()} that
          transform each range individually (and independently of the other
          ranges) and return an object of the \emph{same length} as the
          input object.
          Those transformations are described in the \link{intra-range-methods}
          man page (see \code{?`\link{intra-range-methods}`}).
    \item Inter range transformations like \code{reduce()} that
          transform all the ranges together as a set to produce a new set
          of ranges and return an object not necessarily of the same length
          as the input object.
          Those transformations are described in this man page.
  }
}

\usage{
## range()
\S4method{range}{Ranges}(x, ..., na.rm=FALSE)
\S4method{range}{RangesList}(x, ..., na.rm=FALSE)

## reduce()
reduce(x, ...)
\S4method{reduce}{Ranges}(x, drop.empty.ranges=FALSE, min.gapwidth=1L, with.inframe.attrib=FALSE)
\S4method{reduce}{Views}(x, drop.empty.ranges=FALSE, min.gapwidth=1L, with.inframe.attrib=FALSE)
\S4method{reduce}{RangesList}(x, drop.empty.ranges=FALSE, min.gapwidth=1L, with.inframe.attrib=FALSE)
\S4method{reduce}{RangedData}(x, by = character(), drop.empty.ranges=FALSE,
       min.gapwidth=1L, with.inframe.attrib=FALSE)

## gaps()
gaps(x, start=NA, end=NA)

## disjoin()
disjoin(x, ...)

## isDisjoint()
isDisjoint(x, ...)

## disjointBins()
disjointBins(x, ...)
}

\arguments{
  \item{x}{
    A \link{Ranges}, \link{Views}, \link{RangesList}, \link{MaskCollection},
    or \link{RangedData} object.
  }
  \item{start, end}{
    \itemize{
      \item If \code{x} is a \link{Ranges} or \link{Views} object:
      A single integer or \code{NA}. Use these arguments to specify the
      interval of reference i.e. which interval the returned gaps
      should be relative to.
      \item If \code{x} is a \link{RangesList} object:
      Integer vectors containing the coordinate bounds for each
      \link{RangesList} top-level element.
    }
  }
  \item{drop.empty.ranges}{
    \code{TRUE} or \code{FALSE}. Should empty ranges be dropped?
  }
  \item{min.gapwidth}{
    Ranges separated by a gap of at least \code{min.gapwidth} positions
    are not merged.
  }
  \item{with.inframe.attrib}{
    \code{TRUE} or \code{FALSE}. For internal use.
  }
  \item{by}{
    A character vector.
  }
  \item{...}{
    For \code{range}, additional \code{Ranges} or \code{RangesList}
    to consider.
  }
  \item{na.rm}{Ignored}
}

\details{
  Here we start by describing how each transformation operates on a
  \link{Ranges} object \code{x}.

  \code{range} first combines \code{x} and the arguments in \code{...}.
  If the combined \link{IRanges} object contains at least 1 range,
  then \code{range} returns an \code{IRanges} instance with a single
  range, from the minimum start to the maximum end of the combined object.
  Otherwise (i.e. if the combined object contains no range),
  \code{IRanges()} is returned (i.e. an \link{IRanges} instance of
  length 0).

  If \code{x} is a \link{RangedData} object, then \code{range} returns
  a \code{RangesList} object resulting from calling \code{range(ranges(x))},
  i.e. the bounds of the ranges in each space.

  \code{reduce} first orders the ranges in \code{x} from left to right,
  then merges the overlapping or adjacent ones.
  If \code{x} is a \link{RangedData} object, \code{reduce} merges the
  ranges in each of the spaces after grouping by the \code{by} values
  columns and returns the result as a \code{RangedData} containing
  the reduced ranges and the \code{by} value columns.

  \code{gaps} returns the "normal" \link{Ranges} object representing
  the set of integers that remain after the set of integers represented
  by \code{x} has been removed from the interval specified by the
  \code{start} and \code{end} arguments.

  If \code{x} is a \link{Views} object, then \code{start=NA} and
  \code{end=NA} are interpreted as \code{start=1} and
  \code{end=length(subject(x))}, respectively, so, if \code{start}
  and \code{end} are not specified, then gaps are extracted with respect
  to the entire subject.

  \code{disjoin} returns a disjoint object, by finding
  the union of the end points in \code{x}. In other words, the result
  consists of a range for every interval, of maximal length, over which
  the set of overlapping ranges in \code{x} is the same and at least of
  size 1.

  \code{isDisjoint} returns a logical value indicating whether the
  ranges \code{x} are disjoint (i.e. non-overlapping).

  \code{disjointBins} segregates \code{x} into a set of bins so that the
  ranges in each bin are disjoint. Lower-indexed bins are filled
  first. The method returns an integer vector indicating the bin index
  for each range.

  When \code{x} in a \link{RangesList} object, doing any of the transformation
  above is equivalent to applying the transformation to each \link{RangesList}
  top-level element separately.

  For \code{range}, if there are additional \code{RangesList} objects
  in \code{...}, they are merged into \code{x} by name, if all objects
  have names, otherwise, if they are all of the same length, by
  position. Else, an exception is thrown.
}

\author{H. Pages, M. Lawrence, P. Aboyoun}

\seealso{
  \itemize{
    \item \link{intra-range-methods} for intra range transformations.
    \item The \link{Ranges}, \link{Views}, \link{RangesList},
          \link{MaskCollection}, and \link{RangedData} classes.
    \item The \link[GenomicRanges]{inter-range-methods} man page in the
          GenomicRanges package for methods that operate on
          \link[GenomicRanges]{GenomicRanges} and other objects.
    \item \link{IRanges-setops} for set operations on \link{IRanges}
          objects.
    \item \code{\link{solveUserSEW}} for the SEW (Start/End/Width)
          interface.
  }
}

\examples{
## ---------------------------------------------------------------------
## range()
## ---------------------------------------------------------------------

## On a Ranges object
x <- IRanges(start=c(-2, 6, 9, -4, 1, 0, -6, 3, 10),
             width=c( 5, 0, 6,  1, 4, 3,  2, 0,  3))
range(x)

## On a RangesList object
range1 <- IRanges(start=c(1, 2, 3), end=c(5, 2, 8))
range2 <- IRanges(start=c(15, 45, 20, 1), end=c(15, 100, 80, 5))
range3 <- IRanges(start=c(-2, 6, 7), width=c(8, 0, 0))  # with empty ranges
collection <- IRangesList(one=range1, range2, range3)
range(collection)

irl1 <- IRangesList(a = IRanges(c(1,2),c(4,3)), b = IRanges(c(4,6),c(10,7)))
irl2 <- IRangesList(c = IRanges(c(0,2),c(4,5)), a = IRanges(c(4,5),c(6,7)))
range(irl1, irl2)  # matched by names
names(irl2) <- NULL
range(irl1, irl2)  # now by position

## On a RangedData object
ranges <- IRanges(c(1,2,3),c(4,5,6))
score <- c(10L, 2L, NA)
rd <- RangedData(ranges, score)
range(rd)
rd2 <- RangedData(IRanges(c(5,2,0), c(6,3,1)))
range(rd, rd2)

## ---------------------------------------------------------------------
## reduce()
## ---------------------------------------------------------------------

## On a Ranges object
reduce(x)
reduce(x, drop.empty.ranges=TRUE)

## On a RangesList object. These 4 are the same:
res1 <- reduce(collection)
res2 <- IRangesList(one=reduce(range1), reduce(range2), reduce(range3))
res3 <- do.call(IRangesList, lapply(collection, reduce))
res4 <- endoapply(collection, reduce)

stopifnot(identical(res2, res1))
stopifnot(identical(res3, res1))
stopifnot(identical(res4, res1))

reduce(collection, drop.empty.ranges=TRUE)

## On a RangedData object
rd <- RangedData(
        RangesList(
          chrA=IRanges(start=c(1, 4, 6), width=c(3, 2, 4)),
          chrB=IRanges(start=c(1, 3, 6), width=c(3, 3, 4))),
        score=c(2, 7, 3, 1, 1, 1))
rd
reduce(rd)

## ---------------------------------------------------------------------
## gaps()
## ---------------------------------------------------------------------

## On a Ranges object
x0 <- IRanges(start=c(-2, 6, 9, -4, 1, 0, -6, 10),
              width=c( 5, 0, 6,  1, 4, 3,  2,  3))
gaps(x0)
gaps(x0, start=-6, end=20)

## On a Views object
subject <- XInteger(10, 3:-6)
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

## On a MaskCollection object
mask1 <- Mask(mask.width=29, start=c(11, 25, 28), width=c(5, 2, 2))
mask2 <- Mask(mask.width=29, start=c(3, 10, 27), width=c(5, 8, 1))
mask3 <- Mask(mask.width=29, start=c(7, 12), width=c(2, 4))
mymasks <- append(append(mask1, mask2), mask3)
mymasks
gaps(mymasks)

## ---------------------------------------------------------------------
## disjoin()
## ---------------------------------------------------------------------

## On a Ranges object
ir <- IRanges(c(1, 1, 4, 10), c(6, 3, 8, 10))
disjoin(ir)  # IRanges(c(1, 4, 7, 10), c(3, 6, 8, 10))

## On a RangesList object
disjoin(collection)

## ---------------------------------------------------------------------
## isDisjoint()
## ---------------------------------------------------------------------

## On a Ranges object
isDisjoint(IRanges(c(2,5,1), c(3,7,3)))  # FALSE
isDisjoint(IRanges(c(2,9,5), c(3,9,6)))  # TRUE
isDisjoint(IRanges(1, 5))  # TRUE

## On a RangesList object
isDisjoint(collection)

## ---------------------------------------------------------------------
## disjointBins()
## ---------------------------------------------------------------------

## On a Ranges object
disjointBins(IRanges(1, 5)) # 1L
disjointBins(IRanges(c(3, 1, 10), c(5, 12, 13))) # c(2L, 1L, 2L)

## On a RangesList object
disjointBins(collection)
}

\keyword{utilities}
