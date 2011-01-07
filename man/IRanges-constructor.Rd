\name{IRanges-constructor}

\alias{IRanges-constructor}
\alias{IRanges}
\alias{solveUserSEW0}
\alias{solveUserSEW}


\title{The IRanges constructor and supporting functions}

\description{
  The \code{IRanges} function is a constructor that can be used
  to create IRanges instances.

  \code{solveUserSEW0} and \code{solveUserSEW} are utility functions that
  solve a set of user-supplied start/end/width values.
}

\usage{
## IRanges constructor:
IRanges(start=NULL, end=NULL, width=NULL, names=NULL)

## Supporting functions (not for the end user):
solveUserSEW0(start=NULL, end=NULL, width=NULL)
solveUserSEW(refwidths, start=NA, end=NA, width=NA,
             rep.refwidths=FALSE,
             translate.negative.coord=TRUE,
             allow.nonnarrowing=FALSE)
}

\arguments{
  \item{start, end, width}{
    For \code{IRanges} and \code{solveUserSEW0}: \code{NULL},
    or vector of integers (eventually with NAs).

    For \code{solveUserSEW}: vector of integers (eventually
    with NAs).
  }
  \item{names}{
    A character vector or \code{NULL}.
  }
  \item{refwidths}{
    Vector of non-NA non-negative integers containing the
    reference widths.
  }
  \item{rep.refwidths}{
    \code{TRUE} or \code{FALSE}.
    Use of \code{rep.refwidths=TRUE} is supported only when
    \code{refwidths} is of length 1.
  }
  \item{translate.negative.coord, allow.nonnarrowing}{
    \code{TRUE} or \code{FALSE}.
  }
}

\section{IRanges constructor}{
  Return the IRanges object containing the ranges specified by
  \code{start}, \code{end} and \code{width}.
  Input falls into one of two categories:
  \describe{
    \item{Category 1}{
      \code{start}, \code{end} and \code{width} are numeric vectors
      (or NULLs). If necessary they are recycled to the
      length of the longest (NULL arguments are filled with NAs).
      After this recycling, each row in the 3-column matrix obtained
      by binding those 3 vectors together is "solved" i.e. NAs are
      treated as unknown in the equation \code{end = start + width - 1}.
      Finally, the solved matrix is returned as an \link{IRanges}
      instance.
    }
    \item{Category 2}{
      The \code{start} argument is a logical vector or logical Rle object
      and \code{IRanges(start)} produces the same result as
      \code{as(start, "IRanges")}.
      Note that, in that case, the returned IRanges instance is guaranteed
      to be normal.
    }
  }
  Note that the \code{names} argument is never recycled (to remain
  consistent with what \code{`names<-`} does on standard vectors).
}

\section{Supporting functions}{
  \describe{
    \item{}{
      \code{solveUserSEW0(start=NULL, end=NULL, width=NULL)}:
    }
    \item{}{
      \code{solveUserSEW(refwidths, start=NA, end=NA, width=NA,
               rep.refwidths=FALSE,
               translate.negative.coord=TRUE,
               allow.nonnarrowing=FALSE)}:
      Use of \code{rep.refwidths=TRUE} is supported only when
      \code{refwidths} is of length 1.
      If \code{rep.refwidths=FALSE} (the default) then
      \code{start}, \code{end} and \code{width} are recycled to
      the length of \code{refwidths} (it's an error if one of them
      is longer than \code{refwidths}, or is of zero length while
      \code{refwidths} is not).
      If \code{rep.refwidths=TRUE} then \code{refwidths} is first
      replicated L times where L is the length of the longest of
      \code{start}, \code{end} and \code{width}. After this replication,
      \code{start}, \code{end} and \code{width} are recycled to
      the new length of \code{refwidths} (L) (it's an error if one
      of them is of zero length while L is != 0).

      From now, \code{refwidths}, \code{start}, \code{end} and
      \code{width} are integer vectors of equal lengths.
      Each row in the 3-column matrix obtained by binding those
      3 vectors together must contain at least one NA (otherwise
      an error is returned).
      Then each row is "solved" i.e. the 2 following transformations
      are performed (\code{i} is the indice of the row):
      (1) if \code{translate.negative.coord} is TRUE then a
          negative value of \code{start[i]} or \code{end[i]} is
          considered to be a \code{-refwidths[i]}-based coordinate so
          \code{refwidths[i]+1} is added to it to make it 1-based;
      (2) the NAs in the row are treated as unknowns which values are
          deduced from the known values in the row and from
          \code{refwidths[i]}.

      The exact rules for (2) are the following.
      Rule (2a): if the row contains at least 2 NAs, then \code{width[i]}
      must be one of them (otherwise an error is returned), and if
      \code{start[i]} is one of them it is replaced by 1, and if \code{end[i]}
      is one of them it is replaced by \code{refwidths[i]}, and finally
      \code{width[i]} is replaced by \code{end[i] - start[i] + 1}.
      Rule (2b): if the row contains only 1 NA, then it is replaced by the
      solution of the \code{width[i] == end[i] - start[i] + 1} equation.

      Finally, the set of solved rows is returned as an \link{IRanges}
      object of the same length as \code{refwidths} (after replication
      if \code{rep.refwidths=TRUE}).

      Note that an error is raised if either (1) the set of
      user-supplied start/end/width values is invalid or (2)
      \code{allow.nonnarrowing} is FALSE and the ranges represented by the
      solved start/end/width values are not narrowing
      the ranges represented by the user-supplied start/end/width values.
    }
  }
}

\author{H. Pages}

\seealso{
  \link{IRanges-class},
  \code{\link{narrow}}
}

\examples{
  ## ---------------------------------------------------------------------
  ## A. USING THE IRanges() CONSTRUCTOR
  ## ---------------------------------------------------------------------
  IRanges(start=11, end=rep.int(20, 5))
  IRanges(start=11, width=rep.int(20, 5))
  IRanges(-2, 20)  # only one range
  IRanges(start=c(2, 0, NA), end=c(NA, NA, 14), width=11:0)
  IRanges()  # IRanges instance of length zero
  IRanges(names=character())

  ## With logical input:
  x <- IRanges(c(FALSE, TRUE, TRUE, FALSE, TRUE))  # logical vector input
  isNormal(x)  # TRUE
  x <- IRanges(Rle(1:30) \%\% 5 <= 2)  # logical Rle input
  isNormal(x)  # TRUE

  ## ---------------------------------------------------------------------
  ## B. USING solveUserSEW()
  ## ---------------------------------------------------------------------
  refwidths <- c(5:3, 6:7)
  refwidths

  solveUserSEW(refwidths)
  solveUserSEW(refwidths, start=4)
  solveUserSEW(refwidths, end=3, width=2)
  solveUserSEW(refwidths, start=-3)
  solveUserSEW(refwidths, start=-3, width=2)
  solveUserSEW(refwidths, end=-4)

  ## The start/end/width arguments are recycled:
  solveUserSEW(refwidths, start=c(3, -4, NA), end=c(-2, NA))

  ## Using 'rep.refwidths=TRUE':
  solveUserSEW(10, start=-(1:6), rep.refwidths=TRUE)
  solveUserSEW(10, end=-(1:6), width=3, rep.refwidths=TRUE)
}

\keyword{utilities}
