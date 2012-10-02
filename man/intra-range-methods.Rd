\name{intra-range-methods}

\alias{intra-range-methods}

\alias{shift}
\alias{shift,Ranges-method}
\alias{shift,Views-method}
\alias{shift,RangesList-method}
\alias{shift,CompressedIRangesList-method}
\alias{narrow}
\alias{narrow,Ranges-method}
\alias{narrow,Views-method}
\alias{narrow,RangesList-method}
\alias{narrow,CompressedIRangesList-method}
\alias{narrow,MaskCollection-method}
\alias{narrow,XVectorList-method}
\alias{flank}
\alias{flank,Ranges-method}
\alias{flank,RangesList-method}
\alias{flank,CompressedIRangesList-method}
\alias{reflect}
\alias{reflect,Ranges-method}
\alias{resize}
\alias{resize,Ranges-method}
\alias{resize,RangesList-method}
\alias{resize,CompressedIRangesList-method}
\alias{restrict}
\alias{restrict,Ranges-method}
\alias{restrict,RangesList-method}
\alias{restrict,CompressedIRangesList-method}
\alias{threebands}
\alias{threebands,IRanges-method}
\alias{threebands,XVectorList-method}
\alias{Ops,Ranges,numeric-method}
\alias{Ops,Ranges,ANY-method}
\alias{Ops,CompressedIRangesList,ANY-method}
\alias{Ops,RangesList,ANY-method}

\title{Intra range transformations of a Ranges, Views, RangesList,
       MaskCollection, or XVectorList object}

\description{
  Except for \code{threebands()},
  all the transformations described in this man page are \emph{endomorphisms}
  that operate on a single "range-based" object, that is, they transform the
  ranges contained in the input object and return them in an object of the
  \emph{same class} as the input object.

  Range-based endomorphisms are grouped in 2 categories:
  \enumerate{
    \item Intra range transformations like \code{shift()} that
          transform each range individually (and independently of the other
          ranges) and return an object of the \emph{same length} as the
          input object. Those transformations are described in this man page.
    \item Inter range transformations like \code{\link{reduce}()} that
          transform all the ranges together as a set to produce a new set
          of ranges and return an object not necessarily of the same length
          as the input object.
          Those transformations are described in the \link{inter-range-methods}
          man page (see \code{?`\link{inter-range-methods}`}).
  }
}

\usage{
## shift()
shift(x, shift=0L, use.names=TRUE)

## narrow()
narrow(x, start=NA, end=NA, width=NA, use.names=TRUE)

## flank()
flank(x, width, start=TRUE, both=FALSE, use.names=TRUE, ...)

## reflect()
reflect(x, bounds, use.names=TRUE)

## resize()
resize(x, width, fix="start", use.names=TRUE, ...)

## restrict()
restrict(x, start=NA, end=NA, keep.all.ranges=FALSE, use.names=TRUE)

## threebands()
threebands(x, start=NA, end=NA, width=NA)
}

\arguments{
  \item{x}{
    A \link{Ranges}, \link{Views}, \link{RangesList}, \link{MaskCollection},
    or \link{XVectorList} object.
  }
  \item{shift}{
    An integer vector containing the shift information. Recycled as 
    necessary so that each element corresponds to a range in \code{x}.
    It can also be an \link{IntegerList} object if \code{x} is a
    \link{RangesList} object.
  }
  \item{use.names}{
    \code{TRUE} or \code{FALSE}. Should names be preserved?
  }
  \item{start, end}{
    \itemize{
      \item If \code{x} is a \link{Ranges} or \link{Views} object:
            A vector of integers for all functions except for \code{flank}.
            For \code{restrict}, the supplied \code{start} and \code{end}
            arguments must be vectors of integers, eventually with NAs, that
            specify the restriction interval(s). Recycled as necessary so
            that each element corresponds to a range in \code{x}.
            Same thing for \code{narrow} and \code{threebands}, except that
            here \code{start} and \code{end} must contain coordinates relative
            to the ranges in \code{x}. See the Details section below.
            For \code{flank}, \code{start} is a logical indicating whether
            \code{x} should be flanked at the start (\code{TRUE}) or the end
            (\code{FALSE}). Recycled as necessary so that each element
            corresponds to a range in \code{x}.
      \item If \code{x} is a \link{RangesList} object:
            For \code{flank}, \code{start} must be either a logical vector
            or a \link{LogicalList} object indicating whether \code{x} should
            be flanked at the start (\code{TRUE}) or the end (\code{FALSE}).
            Recycled as necessary so that each element corresponds to a range
            in \code{x}. For \code{narrow}, \code{start} and \code{end} must
            be either an integer vector or an \link{IntegerList} object
            containing coordinates relative to the current ranges.
            For \code{restrict}, \code{start} and \code{end} must be either
            an integer vector or an \link{IntegerList} object (possibly
            containing NA's).
    }
  }
  \item{width}{
    \itemize{
      \item If \code{x} is a \link{Ranges} or \link{Views} object:
            For \code{narrow} and \code{threebands}, a vector of integers,
            eventually with NAs. See the SEW (Start/End/Width) interface for
            the details (\code{?solveUserSEW}).
            For \code{resize} and \code{flank}, the width of the resized or
            flanking regions. Note that if \code{both} is \code{TRUE}, this
            is effectively doubled. Recycled as necessary so that each
            element corresponds to a range in \code{x}.
      \item If \code{x} is a \link{RangesList} object:
            For \code{flank} and \code{resize}, either an integer vector or an
            \link{IntegerList} object containing the width of the flanking
            or resized regions. Recycled as necessary so that each element
            corresponds to a range in \code{x}. (Note for \code{flank}:
            if \code{both} is \code{TRUE}, this is effectively doubled.)
            For \code{narrow}, either an integer vector or a \link{IntegerList}
            object containing the widths to narrow to. See the SEW
            (Start/End/Width) interface for the details (\code{?solveUserSEW}).
    }
  }
  \item{both}{
    If \code{TRUE}, extends the flanking region \code{width} positions
    \emph{into} the range. The resulting range thus straddles the end
    point, with \code{width} positions on either side.
  }
  \item{bounds}{
    An \link{IRanges} object to serve as the reference bounds for the
    reflection, see below.
  }
  \item{fix}{
    \itemize{
      \item If \code{x} is a \link{Ranges} or \link{Views} object:
            A character vector or character-Rle of length 1 or \code{length(x)}
            containing the values \code{"start"}, \code{"end"}, and
            \code{"center"} denoting what to use as an anchor for each
            element in \code{x}.
      \item If \code{x} is a \link{RangesList} object:
            A character vector of length 1, a \link{CharacterList} object,
            or a character-RleList object containing the values
            \code{"start"}, \code{"end"}, and \code{"center"} denoting what
            to use as an anchor for each element in \code{x}.
    }
  }
  \item{keep.all.ranges}{
    \code{TRUE} or \code{FALSE}. Should ranges that don't overlap with
    the restriction interval(s) be kept?
    Note that "don't overlap" means that they end strictly before
    \code{start - 1} or start strictly after \code{end + 1}.
    Ranges that end at \code{start - 1} or start at \code{end + 1}
    are always kept and their width is set to zero in the returned
    \link{IRanges} object.
  }
  \item{...}{
    Additional arguments for methods.
  }
}

\details{
  Here we start by describing how each transformation operates on a
  \link{Ranges} object \code{x}.

  \code{shift} shifts all the ranges in \code{x} by the amount specified
  by the \code{shift} argument.

  \code{narrow} narrows the ranges in \code{x} i.e. each range in the
  returned \link{Ranges} object is a subrange of the corresponding
  range in \code{x}.
  The supplied start/end/width values are solved by a call to
  \code{solveUserSEW(width(x), start=start, end=end, width=width)}
  and therefore must be compliant with the rules of the SEW
  (Start/End/Width) interface (see \code{?\link{solveUserSEW}}
  for the details).
  Then each subrange is derived from the original range according
  to the solved start/end/width values for this range. Note that those
  solved values are interpreted relatively to the original range.

  \code{flank} generates flanking ranges for each range in \code{x}. If
  \code{start} is \code{TRUE} for a given range, the flanking occurs at
  the start, otherwise the end. The widths of the flanks are given by
  the \code{width} parameter. The widths can be negative, in which case
  the flanking region is reversed so that it represents a prefix or
  suffix of the range in \code{x}. The \code{flank} operation is
  illustrated below for a call of the form \code{flank(x, 3, TRUE)},
  where \code{x} indicates a range in \code{x} and \code{-} indicates
  the resulting flanking region:
  \preformatted{
    ---xxxxxxx
  }
  If \code{start} were \code{FALSE}:
  \preformatted{
       xxxxxxx---
  }
  For negative width, i.e. \code{flank(x, -3, FALSE)}, where \code{*}
  indicates the overlap between \code{x} and the result:
  \preformatted{
       xxxx***
  }
  If \code{both} is \code{TRUE}, then, for
  all ranges in \code{x}, the flanking regions are extended \emph{into}
  (or out of, if width is negative)
  the range, so that the result straddles the given endpoint and has
  twice the width given by \code{width}. This is illustrated below for
  \code{flank(x, 3, both=TRUE)}:
  \preformatted{
    ---***xxxx
  }

  \code{reflect} "reflects" or reverses each range in \code{x} relative to
  the corresponding range in \code{bounds}, which is recycled as
  necessary. Reflection preserves the width of a range, but shifts it
  such the distance from the left bound to the start of the range
  becomes the distance from the end of the range to the right
  bound. This is illustrated below, where \code{x} represents
  a range in \code{x} and \code{[} and \code{]} indicate the bounds:
  \preformatted{
    [..xxx.....]
    becomes
    [.....xxx..]
  }

  \code{restrict} restricts the ranges in \code{x} to the interval(s)
  specified by the \code{start} and \code{end} arguments.

  \code{resize} resizes the ranges to the specified width where either the
  start, end, or center is used as an anchor.

  \code{threebands} extends the capability of \code{narrow} by returning
  the 3 ranges objects associated to the narrowing operation.
  The returned value \code{y} is a list of 3 ranges objects named
  \code{"left"}, \code{"middle"} and \code{"right"}.
  The middle component is obtained by calling \code{narrow} with the
  same arguments (except that names are dropped). The left and right
  components are also instances of the same class as \code{x} and they
  contain what has been removed on the left and right sides (respectively)
  of the original ranges during the narrowing.

  Note that original object \code{x} can be reconstructed from the
  left and right bands with \code{punion(y$left, y$right,
    fill.gap=TRUE)}.

  When \code{x} in a \link{RangesList} object, doing any of the transformation
  above is equivalent to applying the transformation to each \link{RangesList}
  top-level element separately.
}

\author{H. Pages, M. Lawrence, P. Aboyoun}

\seealso{
  \itemize{
    \item \link{inter-range-methods} for inter range transformations.
    \item The \link{Ranges}, \link{Views}, \link{RangesList},
          \link{MaskCollection}, and \link{XVectorList} classes.
    \item The \link[GenomicRanges]{intra-range-methods} man page in the
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
## shift()
## ---------------------------------------------------------------------

## On a Ranges object
ir1 <- successiveIRanges(c(19, 5, 0, 8, 5))
ir1
shift(ir1, shift=-3)

## On a RangesList object
range1 <- IRanges(start=c(1, 2, 3), end=c(5, 2, 8))
range2 <- IRanges(start=c(15, 45, 20, 1), end=c(15, 100, 80, 5))
range3 <- IRanges(start=c(-2, 6, 7), width=c(8, 0, 0))  # with empty ranges
collection <- IRangesList(one=range1, range2, range3)
shift(collection, shift=5)

## ---------------------------------------------------------------------
## narrow()
## ---------------------------------------------------------------------

## On a Ranges object
ir2 <- ir1[width(ir1) != 0]
narrow(ir2, start=4, end=-2)
narrow(ir2, start=-4, end=-2)
narrow(ir2, end=5, width=3)
narrow(ir2, start=c(3, 4, 2, 3), end=c(12, 5, 7, 4))

## On a RangesList object
narrow(collection[-3], start=2)
narrow(collection[-3], end=-2)

## On a MaskCollection object
mask1 <- Mask(mask.width=29, start=c(11, 25, 28), width=c(5, 2, 2))
mask2 <- Mask(mask.width=29, start=c(3, 10, 27), width=c(5, 8, 1))
mask3 <- Mask(mask.width=29, start=c(7, 12), width=c(2, 4))
mymasks <- append(append(mask1, mask2), mask3)
mymasks
narrow(mymasks, start=8)

## ---------------------------------------------------------------------
## flank()
## ---------------------------------------------------------------------

## On a Ranges object
ir3 <- IRanges(c(2,5,1), c(3,7,3))
flank(ir3, 2)
flank(ir3, 2, start=FALSE)
flank(ir3, 2, start=c(FALSE, TRUE, FALSE))
flank(ir3, c(2, -2, 2))
flank(ir3, 2, both = TRUE)
flank(ir3, 2, start=FALSE, both=TRUE)
flank(ir3, -2, start=FALSE, both=TRUE)

## On a RangesList object
flank(collection, width=10)

## ---------------------------------------------------------------------
## reflect()
## ---------------------------------------------------------------------

## On a Ranges object
bounds <- IRanges(c(0, 5, 3), c(10, 6, 9))
reflect(ir3, bounds)

## reflect() does not yet support RangesList objects!

## ---------------------------------------------------------------------
## resize()
## ---------------------------------------------------------------------

## On a Ranges object
resize(ir2, 200)
resize(ir2, 2, fix="end")

## On a RangesList object
resize(collection, width=200)

## ---------------------------------------------------------------------
## restrict()
## ---------------------------------------------------------------------

## On a Ranges object
restrict(ir1, start=12, end=34)
restrict(ir1, start=20)
restrict(ir1, start=21)
restrict(ir1, start=21, keep.all.ranges=TRUE)

## On a RangesList object
restrict(collection, start=2, end=8)

## ---------------------------------------------------------------------
## threebands()
## ---------------------------------------------------------------------

## On a Ranges object
z <- threebands(ir2, start=4, end=-2)
ir2b <- punion(z$left, z$right, fill.gap=TRUE)
stopifnot(identical(ir2, ir2b))
threebands(ir2, start=-5)

## threebands() does not support RangesList objects.
}

\keyword{utilities}
