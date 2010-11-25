\name{nearest}

\alias{nearest}
\alias{precede}
\alias{follow}
\alias{nearest,Ranges,RangesORmissing-method}
\alias{precede,Ranges,RangesORmissing-method}
\alias{follow,Ranges,RangesORmissing-method}

\title{Nearest neighbor finding}

\description{
  The \code{nearest}, \code{precede} and \code{follow} methods find
  nearest neighbors between \code{\linkS4class{Ranges}} instances.
}

\usage{
  nearest(x, subject, ...)
  precede(x, subject = x, ...)
  follow(x, subject = x, ...)
}

\arguments{
  \item{x}{The query \code{\linkS4class{Ranges}} instance.}
  \item{subject}{The subject \code{Ranges} instance, within which the
    nearest neighbors are found. Can be missing, in which case the
    query, \code{x}, is also the subject.
  }
  \item{...}{Additional arguments for methods}
}

\details{
  \code{nearest} is the conventional nearest neighbor finder and returns
  a integer vector containing the index of the nearest neighbor range in
  \code{subject} for each range in \code{x}. If there is no nearest
  neighbor (if \code{subject} is empty), NA's are returned.

  The algorithm is roughly as follows, for a range \code{xi} in \code{x}:
  \enumerate{
    \item Find the ranges in \code{subject} that overlap \code{xi}. If a
      single range \code{si} in \code{subject} overlaps \code{xi}, \code{si}
      is returned as the nearest neighbor of \code{xi}. If there are
      multiple overlaps, one of the overlapping ranges is chosen
      arbitrarily.
    \item If no ranges in \code{subject} overlap with \code{xi}, then
      the range in \code{subject} with the shortest distance from its end to
      the start \code{xi} or its start to the end of \code{xi} is
      returned.
  }

  For each range in \code{x}, \code{precede} returns the index of the
  interval in \code{subject} that is directly preceded by the query
  range. Note that any overlapping ranges are excluded. \code{NA} is
  returned when there are no qualifying ranges in \code{subject}.

  \code{follow} is the opposite of \code{precede}: it returns the index
  of the range in \code{subject} that a query range in \code{x} directly
  follows.
}

\author{M. Lawrence}

\seealso{
  \code{\link{findOverlaps}} for finding just the overlapping ranges.
}

\examples{
  query <- IRanges(c(1, 3, 9), c(2, 7, 10))
  subject <- IRanges(c(3, 5, 12), c(3, 6, 12))

  nearest(query, subject) # c(1L, 1L, 3L)
  nearest(query) # c(2L, 1L, 2L)

  query <- IRanges(c(1, 3, 9), c(3, 7, 10))
  subject <- IRanges(c(3, 2, 10), c(3, 13, 12))
  
  precede(query, subject) # c(3L, 3L, NA)
  precede(IRanges(), subject) # integer()
  precede(query, IRanges()) # rep(NA_integer_, 3)
  precede(query) # c(3L, 3L, NA)
  
  follow(query, subject) # c(NA, NA, 1L)
  follow(IRanges(), subject) # integer()
  follow(query, IRanges()) # rep(NA_integer_, 3)
  follow(query) # c(NA, NA, 2L)
}

\keyword{utilities}
