\name{IRanges-utils}

\alias{IRanges-utils}

\alias{successiveIRanges}
\alias{whichAsIRanges}
\alias{shift}
\alias{shift,IRanges-method}
\alias{restrict}
\alias{restrict,IRanges-method}
\alias{narrow}
\alias{narrow,IRanges-method}
\alias{narrow,NormalIRanges-method}
\alias{threebands}
\alias{threebands,IRanges-method}
\alias{reduce}
\alias{reduce,IRanges-method}
\alias{reduce,Ranges-method}
\alias{gaps}
\alias{gaps,IRanges-method}
\alias{gaps,Ranges-method}
\alias{reflect}
\alias{reflect,Ranges-method}
\alias{flank}
\alias{flank,Ranges-method}
\alias{range,Ranges-method}
\alias{*,Ranges,numeric-method}

% Coercion:
\alias{asNormalIRanges}
\alias{coerce,IRanges,NormalIRanges-method}

% Deprecated:
\alias{intToRanges}
\alias{intToAdjacentRanges}
\alias{toNormalIRanges}

\title{IRanges utility functions}

\description{
  Utility functions for creating or modifying \link{IRanges} objects.
}

\usage{
  ## Create an IRanges instance:
  successiveIRanges(width, gapwidth=0, from=1)

  ## Turn a logical vector into a set of ranges:
  whichAsIRanges(x)

  ## Modify an IRanges object (endomorphisms):
  shift(x, shift, use.names=TRUE)
  restrict(x, start=NA, end=NA, keep.all.ranges=FALSE, use.names=TRUE)
  narrow(x, start=NA, end=NA, width=NA, use.names=TRUE)
  threebands(x, start=NA, end=NA, width=NA)
  reduce(x, with.inframe.attrib=FALSE)
  gaps(x, start=NA, end=NA)

  ## Other utilities
  \S4method{reflect}{Ranges}(x, bounds)
  \S4method{flank}{Ranges}(x, width, start = TRUE, both = FALSE)
  \S4method{range}{Ranges}(x, ..., na.rm = FALSE)

  ## Coercion:
  asNormalIRanges(x, force=TRUE)
}

\arguments{
  \item{width}{
    For \code{successiveIRanges}, must be a vector of non-negative integers
    (with no NAs) specifying the widths of the ranges to create.

    For \code{narrow} and \code{threebands}, a vector of integers, eventually
    with NAs. See the SEW (Start/End/Width) interface for the details
    (\code{?solveUserSEW}).

    For \code{flank}, the width of the flanking regions. Note that if
    \code{both} is \code{TRUE}, this is effectively doubled. Recycled as
    necessary so that each element
    corresponds to a range in \code{x}.
  }
  \item{gapwidth}{
    A single integer or an integer vector with one less element than
    the \code{width} vector specifying the widths of the gaps separating
    one range from the next one.
  }
  \item{from}{
    A single integer specifying the starting position of the first range.
  }
  \item{x}{
    A logical vector for \code{whichAsIRanges}.

    An \link{IRanges} object for \code{shift}, \code{restrict}, \code{narrow},
    \code{threebands}, \code{reduce}, \code{gaps} and \code{asNormalIRanges}.
  }
  \item{shift}{
    A single integer.
  }
  \item{use.names}{
    \code{TRUE} or \code{FALSE}. Should names be preserved?
  }
  \item{start, end}{
    A single integer or \code{NA} for all functions except \code{narrow}
    and \code{threebands}.

    For \code{narrow} and \code{threebands}, the supplied \code{start}
    and \code{end} arguments must be vectors of integers, eventually with NAs,
    that contain coordinates relative to the current ranges.
    See the Details section below.

    For \code{flank}, \code{start} is a logical indicating whether
    \code{x} should be flanked at the start (\code{TRUE}) or the end
    (\code{FALSE}). Recycled as necessary so that each element
    corresponds to a range in \code{x}.
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
  \item{with.inframe.attrib}{
    \code{TRUE} or \code{FALSE}. For internal use.
  }
  \item{bounds}{
    An \link{IRanges} object to serve as the reference bounds for the
    reflection, see below.
  }
  \item{both}{
    If \code{TRUE}, extends the flanking region \code{width} positions
    \emph{into} the range. The resulting range thus straddles the end
    point, with \code{width} positions on either side.
  }
  \item{...}{
    Additional \code{Ranges} to consider.
  }
  \item{na.rm}{Ignored}
  \item{force}{
    \code{TRUE} or \code{FALSE}. Should \code{x} be turned into a
    \link{NormalIRanges} object even if \code{isNormal(x)} is \code{FALSE}?
  }
}

\details{
  \code{successiveIRanges} returns an IRanges instance containing the
  ranges that have the widths specified in the \code{width} vector
  and are separated by the gaps specified in \code{gapwidth}.
  The first range starts at position \code{from}.
  When \code{gapwidth=0} and \code{from=1} (the defaults), the returned
  IRanges can be seen as a partitioning of the 1:sum(width) interval.
  See \code{?Partitioning} for more details on this.

  \code{whichAsIRanges} returns an \link{IRanges} instance containing all of
  the ranges where \code{x} is \code{TRUE}.

  \code{shift} shifts all the ranges in \code{x}.

  \code{restrict} restricts the ranges in \code{x} to the interval
  specified by the \code{start} and \code{end} arguments.

  \code{narrow} narrows the ranges in \code{x} i.e. each range in the
  returned \link{IRanges} object is a subrange of the corresponding
  range in \code{x}.
  The supplied start/end/width values are solved by a call to
  \code{solveUserSEW(width(x), start=start, end=end, width=width)}
  and therefore must be compliant with the rules of the SEW
  (Start/End/Width) interface (see \code{?solveUserSEW} for the
  details).
  Then each subrange is derived from the original range according
  to the solved start/end/width values for this range. Note that those
  solved values are interpreted relatively to the original range.

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

  \code{reduce} first orders the ranges in \code{x} from left to right,
  then merges the overlapping or adjacent ones.

  \code{gaps} returns the normal \link{IRanges} object describing
  the set of integers that remain after the set of integers described
  by \code{x} has been removed from the interval specified by the
  \code{start} and \code{end} arguments.

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

  \code{range} returns an \code{IRanges} instance with a single range,
  from the minimum start to the maximum end in the combined ranges of
  \code{x} and the arguments in \code{...}.
  
  If \code{force=TRUE} (the default), then \code{asNormalIRanges} will
  turn \code{x} into a \link{NormalIRanges} instance by reordering and
  reducing the set of ranges if necessary (i.e. only if \code{isNormal(x)}
  is \code{FALSE}, otherwise the set of ranges will be untouched).
  If \code{force=FALSE}, then \code{asNormalIRanges} will turn \code{x}
  into a \link{NormalIRanges} instance only if \code{isNormal(x)} is
  \code{TRUE}, otherwise it will raise an error.
  Note that when \code{force=FALSE}, the returned object is guaranteed
  to contain exactly the same set of ranges than \code{x}.
  \code{as(x, "NormalIRanges")} is equivalent to \code{asNormalIRanges(x, force=TRUE)}.
}

\author{H. Pages and M. Lawrence}

\seealso{
  \link{Ranges-class},
  \link{IRanges-class},
  \link{IRanges-setops},
  \code{\link{solveUserSEW}},
  \code{\link{successiveViews}}
}

\examples{
  vec <- as.integer(c(19, 5, 0, 8, 5))
  whichAsIRanges(vec >= 5)
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

  z <- threebands(y, start=4, end=-2)
  y0 <- punion(z$left, z$right, fill.gap=TRUE)
  identical(y, y0)  # TRUE
  threebands(y, start=-5)

  x <- IRanges(start=c(-2L, 6L, 9L, -4L, 1L, 0L, -6L, 10L),
               width=c( 5L, 0L, 6L,  1L, 4L, 3L,  2L,  3L))
  reduce(x)
  gaps(x)
  gaps(x, start=-6, end=20)  # Regions of the -6:20 range that are not masked by 'x'.


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

  asNormalIRanges(x)   # 3 ranges ordered from left to right and separated by
                       # gaps of width >= 1.

  ## More on normality:
  example(`IRanges-class`)
  isNormal(x16)                        # FALSE
  if (interactive())
      x16 <- asNormalIRanges(x16)      # Error!
  whichFirstNotNormal(x16)             # 57
  isNormal(x16[1:56])                  # TRUE
  xx <- asNormalIRanges(x16[1:56])
  class(xx)
  max(xx)
  min(xx)
}

\keyword{utilities}
