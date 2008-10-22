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
\alias{reduce}
\alias{reduce,IRanges-method}
\alias{gaps}
\alias{gaps,IRanges-method}

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

  ## Modify an IRanges object:
  shift(x, shift, use.names=TRUE)
  restrict(x, start=NA, end=NA, keep.all.ranges=FALSE, use.names=TRUE)
  narrow(x, start=NA, end=NA, width=NA, use.names=TRUE)
  reduce(x, with.inframe.attrib=FALSE)
  gaps(x, start=NA, end=NA)

  ## Coercion:
  asNormalIRanges(x, force=TRUE)
}

\arguments{
  \item{width}{
    For \code{successiveIRanges}, must be a vector of non-negative integers
    (with no NAs) specifying the widths of the ranges to create.

    For \code{narrow}, a vector of integers, eventually with NAs.
    See the SEW (Start/End/Width) interface for the details
    (\code{?solveUserSEW}).
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
    \code{reduce}, \code{gaps} and \code{asNormalIRanges}.
  }
  \item{shift}{
    A single integer.
  }
  \item{use.names}{
    \code{TRUE} or \code{FALSE}. Should names be preserved?
  }
  \item{start, end}{
    A single integer or \code{NA} for all functions except \code{narrow}.

    For \code{narrow}, the supplied \code{start} and \code{end} arguments must
    be vectors of integers, eventually with NAs, that contain coordinates
    relative to the current ranges. See the Details section below.
  }
  \item{keep.all.ranges}{
    \code{TRUE} or \code{FALSE}. Should ranges that become "out of limits"
    after restriction be kept?
  }
  \item{with.inframe.attrib}{
    \code{TRUE} or \code{FALSE}. For internal use.
  }
  \item{force}{
    \code{TRUE} or \code{FALSE}. Should \code{x} be turned into a
    \link{NormalIRanges} object even if \code{isNormal(x)} is \code{FALSE}?
  }
}

\details{
  \code{successiveIRanges} returns an IRanges object containing the ranges on
  \code{subject} that have the widths specified in the \code{width} vector
  and are separated by the gaps specified in \code{gapwidth}.
  The first range starts at position \code{from}.

  \code{whichAsIRanges} returns an \link{IRanges} object containing all of
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

  \code{reduce} first orders the ranges in \code{x} from left to right,
  then merges the overlapping or adjacent ones.

  \code{gaps} returns the normal \link{IRanges} object describing
  the set of integers obtained by removing the set of integers described
  by \code{x} from the interval specified by the \code{start} and
  \code{end} arguments.

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

\author{H. Pages}

\seealso{
  \link{Ranges-class},
  \link{IRanges-class},
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
  y <- x[width(x) != 0]
  narrow(y, start=4, end=-2)
  narrow(y, start=-4, end=-2)
  narrow(y, end=5, width=3)
  narrow(y, start = c(3, 4, 2, 3), end = c(12, 5, 7, 4))

  x <- IRanges(start=c(-2L, 6L, 9L, -4L, 1L, 0L, -6L, 10L),
               width=c( 5L, 0L, 6L,  1L, 4L, 3L,  2L,  3L))
  reduce(x)
  gaps(x)
  gaps(x, start=-6, end=20)  # Regions of the -6:20 range that are not masked by 'x'.
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
