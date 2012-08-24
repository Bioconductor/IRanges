\name{nearest-methods}

\alias{nearest-methods}

\alias{class:RangesORmissing}
\alias{RangesORmissing-class}
\alias{RangesORmissing}

\alias{nearest}
\alias{precede}
\alias{follow}
\alias{distance}
\alias{distanceToNearest}
\alias{nearest,Ranges,RangesORmissing-method}
\alias{precede,Ranges,RangesORmissing-method}
\alias{follow,Ranges,RangesORmissing-method}
\alias{distance,Ranges,Ranges-method}
\alias{distanceToNearest,Ranges,RangesORmissing-method}

\title{Finding the nearest range neighbor}

\description{
  The \code{nearest}, \code{precede} and \code{follow} methods find
  nearest neighbors between \code{\linkS4class{Ranges}} instances.
}

\usage{
nearest(x, subject, ...)
\S4method{nearest}{Ranges,RangesORmissing}(x, subject,
select = c("arbitrary", "all"))
precede(x, subject = x, ...)
\S4method{precede}{Ranges,RangesORmissing}(x, subject,
select = c("first", "all"))
follow(x, subject = x, ...)
\S4method{follow}{Ranges,RangesORmissing}(x, subject,
select = c("last", "all"))
distanceToNearest(x, subject = x, ...)
\S4method{distanceToNearest}{Ranges,RangesORmissing}(x, subject,
select = c("arbitrary", "all"))
}

\arguments{
  \item{x}{The query \code{\linkS4class{Ranges}} instance.}
  \item{subject}{The subject \code{Ranges} instance, within which the
    nearest neighbors are found. Can be missing, in which case the
    query, \code{x}, is also the subject.
  }
  \item{select}{Logic for handling ties. By default, all of the methods
    here will select a single interval (arbitrary for \code{nearest},
    the first by order in \code{subject} for \code{precede}, and the
    last for \code{follow}). To get all matchings, as
    a \code{Hits} object, use \dQuote{all}.
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

  \code{distanceToNearest} returns the distance for each range
  in \code{x} to its nearest neighbor in \code{subject}.
}

\value{
  For \code{nearest}, \code{precede} and \code{follow}, an integer
  vector of indices in \code{subject}, or
  a \code{\linkS4class{Hits}} if \code{select} is
  \dQuote{all}.

  For \code{distanceToNearest}, a \code{DataFrame} with a column for
  the \code{query} index, \code{subject} index and \code{distance}
  between the pair. This may become more formal in the future.
}

\author{M. Lawrence}

\seealso{
  \code{\link{findOverlaps}} for finding just the overlapping ranges.

  The \link[GenomicRanges]{nearest-methods} man page in the GenomicRanges
  package for methods that operate on \link[GenomicRanges]{GenomicRanges}
  and other objects.
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
