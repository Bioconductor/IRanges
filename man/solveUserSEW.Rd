\name{solveUserSEW}

\alias{solveUserSEW}

\title{The start/end/width interface}

\description{
  \code{solveUserSEW} is a utility function that solves a set of
  user-supplied start/end/width values.
}

\usage{
  solveUserSEW(refwidths, start=NA, end=NA, width=NA,
               if.non.positive.start.or.end="translate")
}

\arguments{
  \item{refwidths}{
    Vector of non-negative integers containing the reference widths.
  }
  \item{start, end, width}{
    Vectors of integers, eventually with NAs, containing the set of
    user-supplied start/end/width values.
  }
  \item{if.non.positive.start.or.end}{
    \code{"translate"}, \code{"keep"} or \code{"error"}
  }
}

\details{
  \code{start}, \code{end} and \code{width} must have the same number of
  elements as, or less elements than, \code{refwidths}. In the latter case,
  they are expanded cyclically to the length of \code{refwidths} (provided
  none are of zero length). After this expansion, each row in the 3-column
  matrix obtained by binding those 3 vectors together must contain at least
  one NA (otherwise an error is returned).

  Then each row is "solved" i.e. the 2 following transformations are
  performed (\code{i} is the indice of the row):
    (1) if \code{if.non.positive.start.or.end="translate"} then a
        non-positive value of \code{start[i]} or \code{end[i]} is
        considered to be \code{refwidths[i]+1}-based so \code{refwidths[i]+1}
        is added to it to make it 1-based;
    (2) the NAs in the row are treated as unknowns which values are deduced
        from the known values in the row and from \code{refwidths[i]}.

  The exact rules for (2) are the following. If the row contains at least
  2 NAs, then \code{width[i]} must be one of them (otherwise an error is
  returned), and if \code{start[i]} is one of them it is replaced by 1,
  and if \code{end[i]} is one of them it is replaced by \code{refwidths[i]},
  and finally \code{width[i]} is replaced by \code{end[i] - start[i] + 1}.
  If the row contains only 1 NA, then it is replaced by the solution of the
  \code{width[i] == end[i] - start[i] + 1} equation.

  Finally, the set of solved rows is returned as an \link{IRanges} object.
}

\value{
  An \link{IRanges} object (with the same number of elements as
  \code{refwidths}) representing the set of solved start/end/width values.
}

\author{H. Pages}

\seealso{
  \link{IRanges-class},
  \link{narrow}
}

\examples{
  refwidths <- c(5:3, 6:7)
  refwidths

  solveUserSEW(refwidths)
  solveUserSEW(refwidths, start=4)
  solveUserSEW(refwidths, end=3, width=2)
  solveUserSEW(refwidths, start=-3)
  solveUserSEW(refwidths, start=-3, width=2)
  solveUserSEW(refwidths, end=-4)

  ## The start/end/width arguments are expanded cyclically
  solveUserSEW(refwidths, start=c(3, -4, NA), end=c(-2, NA))
}

\keyword{utilities}
