\name{RangesList-utils}
\alias{RangesList-utils}
% utilities
\alias{shift,RangesList-method}
\alias{shift,CompressedIRangesList-method}
\alias{restrict,RangesList-method}
\alias{restrict,CompressedIRangesList-method}
\alias{narrow,RangesList-method}
\alias{narrow,CompressedIRangesList-method}
\alias{resize,RangesList-method}
\alias{resize,CompressedIRangesList-method}
\alias{flank,RangesList-method}
\alias{flank,CompressedIRangesList-method}
\alias{disjoin,RangesList-method}
\alias{reduce,RangesList-method}
\alias{reduce,CompressedIRangesList-method}
\alias{gaps,RangesList-method}
\alias{range,RangesList-method}

% set operations
\alias{union,RangesList,RangesList-method}
\alias{intersect,RangesList,RangesList-method}
\alias{setdiff,RangesList,RangesList-method}

\title{RangesList utility functions}

\description{
  Utility functions for manipulating \code{\linkS4class{RangesList}}
  objects.
}

\usage{
\S4method{shift}{RangesList}(x, shift, use.names=TRUE)
\S4method{restrict}{RangesList}(x, start=NA, end=NA, keep.all.ranges=FALSE, use.names=TRUE)
\S4method{narrow}{RangesList}(x, start=NA, end=NA, width=NA, use.names=TRUE)
\S4method{resize}{RangesList}(x, width, start=TRUE, use.names=TRUE)
\S4method{flank}{RangesList}(x, width, start=TRUE, both=FALSE, use.names=TRUE)
\S4method{disjoin}{RangesList}(x)
\S4method{reduce}{RangesList}(x, with.inframe.attrib=FALSE)
\S4method{gaps}{RangesList}(x, start=NA, end=NA)
\S4method{range}{RangesList}(x, ..., na.rm = FALSE)

# Set operations
\S4method{union}{RangesList,RangesList}(x, y)
\S4method{intersect}{RangesList,RangesList}(x, y)
\S4method{setdiff}{RangesList,RangesList}(x, y)
}

\arguments{
  \item{x, y}{A \code{RangesList}}
  \item{start, end}{
    A single integer or \code{NA} for all functions except \code{narrow},
    \code{resize}, and \code{flank}.

    For \code{narrow}, the supplied \code{start} and \code{end} arguments
    must be vectors of integers, eventually with NAs, that contain
    coordinates relative to the current ranges.

    For \code{resize} and \code{flank}, \code{start} is a logical indicating
    whether \code{x} should be flanked at the start (\code{TRUE}) or the end
    (\code{FALSE}). Recycled as necessary so that each element
    corresponds to a range in \code{x}.
  }
  \item{width}{
    For \code{narrow}, a vector of integers, eventually with NAs. See the SEW
    (Start/End/Width) interface for the details (\code{?solveUserSEW}).

    For \code{resize} and \code{flank}, the width of the resized or flanking
    regions. Note that if \code{both} is \code{TRUE}, this is effectively
    doubled. Recycled as necessary so that each element corresponds to a range
    in \code{x}.
  }
  \item{shift}{
    A single integer.
  }
  \item{both}{
    If \code{TRUE}, extends the flanking region \code{width} positions
    \emph{into} the range. The resulting range thus straddles the end
    point, with \code{width} positions on either side.
  }
  \item{use.names}{
    \code{TRUE} or \code{FALSE}. Should names be preserved?
  }
  \item{keep.all.ranges}{
    \code{TRUE} or \code{FALSE}. Should ranges that don't overlap with
    the interval specified by \code{start} and \code{end} be kept?
    Note that "don't overlap" means that they end strictly before
    \code{start - 1} or start strictly after \code{end + 1}.
    Ranges that end at \code{start - 1} or start at \code{end + 1}
    are always kept and their width is set to zero in the returned
    \link{RangesList} object.
  }
  \item{with.inframe.attrib}{
    \code{TRUE} or \code{FALSE}. For internal use.
  }
  \item{...}{
    Additional \code{RangesList} to consider.
  }
  \item{na.rm}{Ignored}
}

\details{
  The \code{shift} method shifts all the ranges in \code{x}.

  The \code{restrict} method restricts the ranges in \code{x} to the
  interval specified by the \code{start} and \code{end} arguments.

  The \code{narrow} method narrows the ranges in \code{x} i.e. each range
  in the returned \link{RangesList} object is a subrange of the corresponding
  range in \code{x}.

  The \code{resize} method resizes the ranges to the specified width where
  either the start or end is used as an anchor.

  The  \code{flank} method generates flanking ranges for each range in
  \code{x}.

  The \code{disjoin} method returns disjoint ranges by finding the 
  within element union of the end points of \code{x}.

  The \code{reduce} method merges (via \code{\link{reduce}}) each of the
  elements in the list and returns the result as a \code{RangesList}.

  The \code{gaps} method takes the complement (via \code{\link{gaps}})
  of each element in the list and returns the result as a
  \code{RangesList}.

  \code{range} finds the \code{\link[=range,Ranges-method]{range}}, i.e.
  a \code{Ranges} with one range, from the minimum start to the maximum
  end, on each element in \code{x} and returns the result as a
  \code{RangesList}. If there are additional \code{RangesList} objects
  in \code{...}, they are merged into \code{x} by name, if all objects
  have names, otherwise, if they are all of the same length, by
  position. Else, an exception is thrown.

  The \code{union} method performs elementwise \code{union} operations
  for two \code{RangesList} objects.

  The \code{intersect} method performs elementwise \code{intersect} operations
  for two \code{RangesList} objects.

  The \code{setdiff} method performs elementwise \code{setdiff} operations
  for two \code{RangesList} objects.
}

\value{
  A \code{RangesList} object. For \code{shift}, \code{restrict},
  \code{narrow}, \code{resize}, \code{flank}, \code{reduce},
  \code{gaps} and \code{range}, length is the same as that of \code{x}.
}

\author{Michael Lawrence, H. Pages, P. Aboyoun}

\seealso{
  \code{\linkS4class{RangesList}}, \code{\link{IRanges-utils}}
}

\examples{
  range1 <- IRanges(start=c(1,2,3), end=c(5,2,8))
  range2 <- IRanges(start=c(15,45,20,1), end=c(15,100,80,5))
  collection <- RangesList(one = range1, range2)

  shift(collection, shift=5)
  restrict(collection, start=2, end=8)
  resize(collection, width=200)
  flank(collection, width=10)
  disjoin(collection)

  # these three are the same
  reduce(collection)
  RangesList(one = reduce(range1), reduce(range2))
  do.call(RangesList, lapply(collection, reduce))

  # these three are the same
  gaps(collection)
  RangesList(one = gaps(range1), gaps(range2))
  do.call(RangesList, lapply(collection, gaps))

  # 'range'
  rl <- RangesList(a = IRanges(c(1,2),c(4,3)), b = IRanges(c(4,6),c(10,7)))
  rl2 <- RangesList(c = IRanges(c(0,2),c(4,5)), a = IRanges(c(4,5),c(6,7)))
  range(rl, rl2) # matched by names
  names(rl2) <- NULL
  range(rl, rl2) # now by position

  # set operations
  union(rl, rl2)
  intersect(rl, rl2)
  setdiff(rl, rl2)
}

\keyword{utilities}
