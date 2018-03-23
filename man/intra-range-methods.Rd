\name{intra-range-methods}

\alias{intra-range-methods}

\alias{shift}
\alias{shift,IntegerRanges-method}
\alias{shift,IPos-method}
\alias{shift,Views-method}
\alias{shift,RangesList-method}

\alias{narrow}
\alias{narrow,ANY-method}
\alias{windows,IntegerRanges-method}
\alias{windows,Views-method}
\alias{narrow,MaskCollection-method}

\alias{resize}
\alias{resize,IntegerRanges-method}
\alias{resize,Views-method}
\alias{resize,RangesList-method}

\alias{flank}
\alias{flank,IntegerRanges-method}
\alias{flank,Views-method}
\alias{flank,RangesList-method}

\alias{promoters}
\alias{promoters,IntegerRanges-method}
\alias{promoters,Views-method}
\alias{promoters,RangesList-method}

\alias{reflect}
\alias{reflect,IntegerRanges-method}

\alias{restrict}
\alias{restrict,IntegerRanges-method}
\alias{restrict,Views-method}
\alias{restrict,RangesList-method}

\alias{threebands}
\alias{threebands,IRanges-method}

\alias{Ops,IntegerRanges,numeric-method}
\alias{Ops,IntegerRanges,ANY-method}
\alias{Ops,CompressedIRangesList,numeric-method}
\alias{Ops,IntegerRangesList,numeric-method}


\title{Intra range transformations of an IntegerRanges, Views,
       RangesList, or MaskCollection object}

\description{
  Range-based transformations are grouped in 2 categories:
  \enumerate{
    \item \emph{Intra range transformations} (e.g. \code{shift()})
          transform each range individually (and independently of the other
          ranges). They return an object \emph{parallel} to the input object,
          that is, where the i-th range corresponds to the i-th range in the
          input.
          Those transformations are described below.

    \item \emph{Inter range transformations} (e.g. \code{\link{reduce}()})
          transform all the ranges together as a set to produce a new set
          of ranges. They return an object that is generally \emph{NOT}
          parallel to the input object.
          Those transformations are described in the \link{inter-range-methods}
          man page (see \code{?`\link{inter-range-methods}`}).
  }

  Except for \code{threebands()}, all the transformations described in this
  man page are \emph{endomorphisms} that operate on a single "range-based"
  object, that is, they transform the ranges contained in the input object
  and return them in an object of the \emph{same class} as the input object.
}

\usage{
shift(x, shift=0L, use.names=TRUE)

narrow(x, start=NA, end=NA, width=NA, use.names=TRUE)

resize(x, width, fix="start", use.names=TRUE, ...)

flank(x, width, start=TRUE, both=FALSE, use.names=TRUE, ...)

promoters(x, upstream=2000, downstream=200, use.names=TRUE, ...)

reflect(x, bounds, use.names=TRUE)

restrict(x, start=NA, end=NA, keep.all.ranges=FALSE, use.names=TRUE)

threebands(x, start=NA, end=NA, width=NA)
}

\arguments{
  \item{x}{
    An \link{IntegerRanges}, \link{Views}, \link{RangesList}, or
    \link{MaskCollection} object.
  }
  \item{shift}{
    An integer vector containing the shift information. Recycled as
    necessary so that each element corresponds to a range in \code{x}.

    Can also be a list-like object \emph{parallel} to \code{x}
    if \code{x} is a \link{RangesList} object.
  }
  \item{use.names}{
    \code{TRUE} or \code{FALSE}. Should names be preserved?
  }
  \item{start, end}{
    If \code{x} is an \link{IntegerRanges} or \link{Views} object:
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

    Can also be list-like objects \emph{parallel} to \code{x}
    if \code{x} is a \link{RangesList} object.
  }
  \item{width}{
    If \code{x} is an \link{IntegerRanges} or \link{Views} object:
    For \code{narrow} and \code{threebands}, a vector of integers,
    eventually with NAs. See the SEW (Start/End/Width) interface for
    the details (\code{?solveUserSEW}).
    For \code{resize} and \code{flank}, the width of the resized or
    flanking regions. Note that if \code{both} is \code{TRUE}, this
    is effectively doubled. Recycled as necessary so that each
    element corresponds to a range in \code{x}.

    Can also be a list-like object \emph{parallel} to \code{x}
    if \code{x} is a \link{RangesList} object.
  }
  \item{fix}{
    If \code{x} is an \link{IntegerRanges} or \link{Views} object:
    A character vector or character-Rle of length 1 or \code{length(x)}
    containing the values \code{"start"}, \code{"end"}, and
    \code{"center"} denoting what to use as an anchor for each
    element in \code{x}.

    Can also be a list-like object \emph{parallel} to \code{x}
    if \code{x} is a \link{RangesList} object.
  }
  \item{...}{
    Additional arguments for methods.
  }
  \item{both}{
    If \code{TRUE}, extends the flanking region \code{width} positions
    \emph{into} the range. The resulting range thus straddles the end
    point, with \code{width} positions on either side.
  }
  \item{upstream, downstream}{
    Vectors of non-NA non-negative integers. Recycled as
    necessary so that each element corresponds to a range in \code{x}.
    Can also be list-like objects \emph{parallel} to \code{x}
    if \code{x} is a \link{RangesList} object.

    \code{upstream} defines the number of nucleotides toward the 5' end
    and \code{downstream} defines the number toward the 3' end, relative
    to the transcription start site. Promoter regions are formed by merging
    the upstream and downstream ranges.

    Default values for \code{upstream} and \code{downstream} were chosen based
    on our current understanding of gene regulation. On average, promoter
    regions in the mammalian genome are 5000 bp upstream and downstream of the
    transcription start site.
  }
  \item{bounds}{
    An \link{IRanges} object to serve as the reference bounds for the
    reflection, see below.
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
}

\details{
  Unless specified otherwise, when \code{x} is a \link{RangesList}
  object, any transformation described here is equivalent to applying the
  transformation to each list element in \code{x}.

  \subsection{shift}{

    \code{shift} shifts all the ranges in \code{x} by the amount specified
    by the \code{shift} argument.

  }\subsection{narrow}{

    \code{narrow} narrows the ranges in \code{x} i.e. each range in the
    returned \link{IntegerRanges} object is a subrange of the corresponding
    range in \code{x}.
    The supplied start/end/width values are solved by a call to
    \code{solveUserSEW(width(x), start=start, end=end, width=width)}
    and therefore must be compliant with the rules of the SEW
    (Start/End/Width) interface (see \code{?\link{solveUserSEW}}
    for the details).
    Then each subrange is derived from the original range according
    to the solved start/end/width values for this range. Note that those
    solved values are interpreted relatively to the original range.

  }\subsection{resize}{

    \code{resize} resizes the ranges to the specified width where either
    the start, end, or center is used as an anchor.

  }\subsection{flank}{

    \code{flank} generates flanking ranges for each range in \code{x}. If
    \code{start} is \code{TRUE} for a given range, the flanking occurs at
    the start, otherwise the end. The widths of the flanks are given by
    the \code{width} parameter. The widths can be negative, in which case
    the flanking region is reversed so that it represents a prefix or
    suffix of the range in \code{x}. The \code{flank} operation is
    illustrated below for a call of the form \code{flank(x, 3, TRUE)},
    where \code{x} indicates a range in \code{x} and \code{-} indicates
    the resulting flanking region:
    \preformatted{    ---xxxxxxx}
    If \code{start} were \code{FALSE}:
    \preformatted{       xxxxxxx---}
    For negative width, i.e. \code{flank(x, -3, FALSE)}, where \code{*}
    indicates the overlap between \code{x} and the result:
    \preformatted{       xxxx***}
    If \code{both} is \code{TRUE}, then, for all ranges in \code{x}, the
    flanking regions are extended \emph{into} (or out of, if width is
    negative) the range, so that the result straddles the given endpoint
    and has twice the width given by \code{width}. This is illustrated below
    for \code{flank(x, 3, both=TRUE)}:
    \preformatted{    ---***xxxx}

  }\subsection{promoters}{

    \code{promoters} generates promoter ranges for each range in \code{x}
    relative to the transcription start site (TSS), where TSS is
    \code{start(x)}. The promoter range is expanded around the TSS
    according to the \code{upstream} and \code{downstream} arguments.
    \code{upstream} represents the number of nucleotides in the 5'
    direction and \code{downstream} the number in the 3' direction.
    The full range is defined as,
    (start(x) - upstream) to (start(x) + downstream - 1).
    For documentation for using \code{promoters} on a
    \link[GenomicRanges]{GRanges} object see
    \code{?`\link[GenomicRanges]{promoters,GenomicRanges-method}`} in
    the \pkg{GenomicRanges} package.

  }\subsection{reflect}{

    \code{reflect} "reflects" or reverses each range in \code{x} relative to
    the corresponding range in \code{bounds}, which is recycled as
    necessary. Reflection preserves the width of a range, but shifts it
    such the distance from the left bound to the start of the range
    becomes the distance from the end of the range to the right
    bound. This is illustrated below, where \code{x} represents
    a range in \code{x} and \code{[} and \code{]} indicate the bounds:
    \preformatted{      [..xxx.....]
      becomes
      [.....xxx..]}

  }\subsection{restrict}{

    \code{restrict} restricts the ranges in \code{x} to the interval(s)
    specified by the \code{start} and \code{end} arguments.

  }\subsection{threebands}{

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
    left and right bands with \code{punion(y$left, y$right, fill.gap=TRUE)}.

  }
}

\author{H. Pagès, M. Lawrence, and P. Aboyoun}

\seealso{
  \itemize{
    \item \link{inter-range-methods} for inter range transformations.

    \item The \link{IntegerRanges}, \link{Views}, \link{RangesList},
          and \link{MaskCollection} classes.

    \item The \link[GenomicRanges]{intra-range-methods} man page in the
          \pkg{GenomicRanges} package for \emph{intra range transformations}
          of genomic ranges.

    \item \link{setops-methods} for set operations on \link{IRanges}
          objects.

    \item \code{\link[S4Vectors]{endoapply}} in the \pkg{S4Vectors} package.
  }
}

\examples{
## ---------------------------------------------------------------------
## shift()
## ---------------------------------------------------------------------

## On an IRanges object:
ir1 <- successiveIRanges(c(19, 5, 0, 8, 5))
ir1
shift(ir1, shift=-3)

## On an IRangesList object:
range1 <- IRanges(start=c(1, 2, 3), end=c(5, 2, 8))
range2 <- IRanges(start=c(15, 45, 20, 1), end=c(15, 100, 80, 5))
range3 <- IRanges(start=c(-2, 6, 7), width=c(8, 0, 0))  # with empty ranges
collection <- IRangesList(one=range1, range2, range3)
shift(collection, shift=5)  # same as endoapply(collection, shift, shift=5)

## Sanity check:
res1 <- shift(collection, shift=5)
res2 <- endoapply(collection, shift, shift=5)
stopifnot(identical(res1, res2))

## ---------------------------------------------------------------------
## narrow()
## ---------------------------------------------------------------------

## On an IRanges object:
ir2 <- ir1[width(ir1) != 0]
narrow(ir2, start=4, end=-2)
narrow(ir2, start=-4, end=-2)
narrow(ir2, end=5, width=3)
narrow(ir2, start=c(3, 4, 2, 3), end=c(12, 5, 7, 4))

## On an IRangesList object:
narrow(collection[-3], start=2)
narrow(collection[-3], end=-2)

## On a MaskCollection object:
mask1 <- Mask(mask.width=29, start=c(11, 25, 28), width=c(5, 2, 2))
mask2 <- Mask(mask.width=29, start=c(3, 10, 27), width=c(5, 8, 1))
mask3 <- Mask(mask.width=29, start=c(7, 12), width=c(2, 4))
mymasks <- append(append(mask1, mask2), mask3)
mymasks
narrow(mymasks, start=8)

## ---------------------------------------------------------------------
## resize()
## ---------------------------------------------------------------------

## On an IRanges object:
resize(ir2, 200)
resize(ir2, 2, fix="end")

## On an IRangesList object:
resize(collection, width=200)

## ---------------------------------------------------------------------
## flank()
## ---------------------------------------------------------------------

## On an IRanges object:
ir3 <- IRanges(c(2,5,1), c(3,7,3))
flank(ir3, 2)
flank(ir3, 2, start=FALSE)
flank(ir3, 2, start=c(FALSE, TRUE, FALSE))
flank(ir3, c(2, -2, 2))
flank(ir3, 2, both = TRUE)
flank(ir3, 2, start=FALSE, both=TRUE)
flank(ir3, -2, start=FALSE, both=TRUE)

## On an IRangesList object:
flank(collection, width=10)

## ---------------------------------------------------------------------
## promoters()
## ---------------------------------------------------------------------

## On an IRanges object:
ir4 <- IRanges(20:23, width=3)
promoters(ir4, upstream=0, downstream=0) ## no change
promoters(ir4, upstream=0, downstream=1) ## start value only
promoters(ir4, upstream=1, downstream=0) ## single upstream nucleotide

## On an IRangesList object:
promoters(collection, upstream=5, downstream=2)

## ---------------------------------------------------------------------
## reflect()
## ---------------------------------------------------------------------

## On an IRanges object:
bounds <- IRanges(c(0, 5, 3), c(10, 6, 9))
reflect(ir3, bounds)

## reflect() does not yet support IRangesList objects!

## ---------------------------------------------------------------------
## restrict()
## ---------------------------------------------------------------------

## On an IRanges object:
restrict(ir1, start=12, end=34)
restrict(ir1, start=20)
restrict(ir1, start=21)
restrict(ir1, start=21, keep.all.ranges=TRUE)

## On an IRangesList object:
restrict(collection, start=2, end=8)

## ---------------------------------------------------------------------
## threebands()
## ---------------------------------------------------------------------

## On an IRanges object:
z <- threebands(ir2, start=4, end=-2)
ir2b <- punion(z$left, z$right, fill.gap=TRUE)
stopifnot(identical(ir2, ir2b))
threebands(ir2, start=-5)

## threebands() does not support IRangesList objects.
}

\keyword{utilities}
