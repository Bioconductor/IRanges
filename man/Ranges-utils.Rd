\name{Ranges-utils}

\alias{Ranges-utils}

\alias{flank}
\alias{flank,Ranges-method}
\alias{flank,NormalIRanges-method}
\alias{narrow}
\alias{narrow,Ranges-method}
\alias{narrow,NormalIRanges-method}
\alias{reflect}
\alias{reflect,Ranges-method}
\alias{reflect,NormalIRanges-method}
\alias{resize}
\alias{resize,Ranges-method}
\alias{resize,NormalIRanges-method}
\alias{restrict}
\alias{restrict,Ranges-method}
\alias{restrict,NormalIRanges-method}
\alias{shift}
\alias{shift,Ranges-method}

\alias{range,Ranges-method}
\alias{range,IRanges-method}
\alias{reduce}
\alias{reduce,Ranges-method}
\alias{reduce,IRanges-method}
\alias{threebands}
\alias{threebands,IRanges-method}

\title{Ranges utility functions}

\description{
  Utility functions for modifying \link{Ranges} objects.
}

\usage{
flank(x, width, start=TRUE, both=FALSE, use.names=TRUE)
narrow(x, start=NA, end=NA, width=NA, use.names=TRUE)
reflect(x, bounds, use.names=TRUE)
resize(x, width, fix="start", use.names=TRUE, ...)
restrict(x, start=NA, end=NA, keep.all.ranges=FALSE, use.names=TRUE)
shift(x, shift, use.names=TRUE)

\S4method{range}{Ranges}(x, ..., na.rm = FALSE)
\S4method{reduce}{Ranges}(x, drop.empty.ranges=FALSE, min.gapwidth=1L,
       with.inframe.attrib=FALSE)

threebands(x, start=NA, end=NA, width=NA)
}

\arguments{
  \item{x}{
    An \link{Ranges} object.
  }
  \item{width}{
    For \code{narrow} and \code{threebands}, a vector of integers, eventually
    with NAs. See the SEW (Start/End/Width) interface for the details
    (\code{?solveUserSEW}).

    For \code{resize} and \code{flank}, the width of the resized or flanking
    regions. Note that if \code{both} is \code{TRUE}, this is effectively
    doubled. Recycled as necessary so that each element corresponds to a range
    in \code{x}.
  }
  \item{start, end}{
    A single integer or \code{NA} for all functions except \code{narrow},
    \code{threebands}, and \code{flank}.

    For \code{narrow} and \code{threebands}, the supplied \code{start}
    and \code{end} arguments must be vectors of integers, eventually with NAs,
    that contain coordinates relative to the current ranges.
    See the Details section below.

    For \code{flank}, \code{start} is a logical indicating whether \code{x}
    should be flanked at the start (\code{TRUE}) or the end (\code{FALSE}).
    Recycled as necessary so that each element corresponds to a range in
    \code{x}.
  }
  \item{both}{
    If \code{TRUE}, extends the flanking region \code{width} positions
    \emph{into} the range. The resulting range thus straddles the end
    point, with \code{width} positions on either side.
  }
  \item{use.names}{
    \code{TRUE} or \code{FALSE}. Should names be preserved?
  }
  \item{bounds}{
    An \link{IRanges} object to serve as the reference bounds for the
    reflection, see below.
  }
  \item{fix}{
    For \code{resize}, a character vector or character Rle of length 1 or
    \code{length(x)} containing the values \code{"start"}, \code{"end"},
    and \code{"center"} denoting what to use as an anchor for each element
    in \code{x}.
  }
  \item{keep.all.ranges}{
    \code{TRUE} or \code{FALSE}. Should ranges that don't overlap with
    the interval specified by \code{start} and \code{end} be kept?
    Note that "don't overlap" means that they end strictly before
    \code{start - 1} or start strictly after \code{end + 1}.
    Ranges that end at \code{start - 1} or start at \code{end + 1}
    are always kept and their width is set to zero in the returned
    \link{IRanges} object.
  }
  \item{shift}{
    A single integer.
  }
  \item{drop.empty.ranges}{
    \code{TRUE} or \code{FALSE}. Should empty ranges be dropped?
  }
  \item{min.gapwidth}{
    Ranges separated by a gap of at least \code{min.gapwidth} positions
    won't be merged in the \link{Ranges} object returned by \code{reduce}.
    Otherwise, they will.
  }
  \item{with.inframe.attrib}{
    \code{TRUE} or \code{FALSE}. For internal use.
  }
  \item{...}{
    For \code{range}, additional \code{Ranges} to consider.
  }
  \item{na.rm}{Ignored}
}

\details{
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

  \code{resize} resizes the ranges to the specified width where either the
  start, end, or center is used as an anchor.

  \code{restrict} restricts the ranges in \code{x} to the interval
  specified by the \code{start} and \code{end} arguments.

  \code{shift} shifts all the ranges in \code{x}.

  \code{range} returns an \code{IRanges} instance with a single range,
  from the minimum start to the maximum end in the combined ranges of
  \code{x} and the arguments in \code{...}.
  
  \code{reduce} first orders the ranges in \code{x} from left to right,
  then merges the overlapping or adjacent ones.

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

\author{H. Pages, M. Lawrence, P. Aboyoun}

\seealso{
  \code{threebands} could be described as a parallel variant
  of \code{\link{disjoin}}.
  
  \link{Ranges-class},
  \link{IRanges-setops},
  \code{\link{solveUserSEW}}
}

\examples{
  vec <- as.integer(c(19, 5, 0, 8, 5))
  x <- successiveIRanges(vec)
  x

  shift(x, -3)

  restrict(x, start=12, end=34)
  restrict(x, start=20)
  restrict(x, start=21)
  restrict(x, start=21, keep.all.ranges=TRUE)

  y <- x[width(x) != 0]
  narrow(y, start=4, end=-2)
  narrow(y, start=-4, end=-2)
  narrow(y, end=5, width=3)
  narrow(y, start = c(3, 4, 2, 3), end = c(12, 5, 7, 4))

  resize(y, width = 200)
  resize(y, width = 2, fix = "end")

  z <- threebands(y, start=4, end=-2)
  y0 <- punion(z$left, z$right, fill.gap=TRUE)
  identical(y, y0)  # TRUE
  threebands(y, start=-5)

  x <- IRanges(start=c(-2, 6, 9, -4, 1, 0, -6, 3, 10),
               width=c( 5, 0, 6,  1, 4, 3,  2, 0,  3))
  reduce(x)
  reduce(x, drop.empty.ranges=TRUE)

  ir1 <- IRanges(c(2,5,1), c(3,7,3))

  bounds <- IRanges(c(0, 5, 3), c(10, 6, 9))
  reflect(ir1, bounds)

  flank(ir1, 2)
  flank(ir1, 2, FALSE)
  flank(ir1, 2, c(FALSE, TRUE, FALSE))
  flank(ir1, c(2, -2, 2))
  flank(ir1, 2, both = TRUE)
  flank(ir1, 2, FALSE, TRUE)
  flank(ir1, -2, FALSE, TRUE)
}

\keyword{utilities}
