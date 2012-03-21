\name{encodeOverlaps}

\alias{encodeOverlaps}
\alias{encodeOverlaps,ANY,ANY,Hits-method}
\alias{encodeOverlaps,RangesList,RangesList,missing-method}
\alias{encodeOverlaps,RangesList,Ranges,missing-method}
\alias{encodeOverlaps,Ranges,RangesList,missing-method}
\alias{encodeOverlaps,Ranges,Ranges,missing-method}

\alias{encodeOverlaps1}
\alias{RangesList_encodeOverlaps}


\title{Compute overlap encodings}

\description{
  The \code{encodeOverlaps} function computes the overlap encodings
  between a \code{query} and a \code{subject}, both list-like objects
  with top-level elements typically containing multiple ranges.
}

\usage{
encodeOverlaps(query, subject, hits=NULL)
}

\arguments{
  \item{query, subject}{
    List-like objects, usually of the same length, with top-level elements
    typically containing multiple ranges (e.g. \link{RangesList} objects).
    If the 2 objects don't have the same length, and if \code{hits} is not
    supplied, then the shortest is recycled to the length of the longest
    (the standard recycling rules apply).
  }
  \item{hits}{
    An optional \link{Hits} object that is typically the result of a call
    to \code{\link{findOverlaps}(query, subject)}.
    Supplying \code{hits} is a convenient way to do
    \code{encodeOverlaps(query[queryHits(hits)], subject[subjectHits(hits)])},
    that is,
    calling \code{encodeOverlaps(query, subject, hits)} will perform the
    above.
    In that case, \code{query} and \code{subject} are not expected to have
    the same length anymore.
  }
}

\details{
  See \code{?OverlapEncodings} for a short introduction to
  "overlap encodings".
}

\value{
  An \link{OverlapEncodings} object with the length of \code{query}
  and \code{subject} for \code{encodeOverlaps(query, subject)},
  or with the length of \code{hits} for
  \code{encodeOverlaps(query, subject, hits)}.
}

\author{H. Pages}

\seealso{
  \code{\link{OverlapEncodings}},
  \link[GenomicRanges]{encodeOverlaps,GRangesList,GRangesList-method},
  \link{Hits-class},
  \link{findOverlaps},
  \link{RangesList-class}
}

\examples{
## Between 2 RangesList objects:
read1 <- IRanges(c(7, 15, 22), c(9, 19, 23))
read2 <- IRanges(c(5, 15), c(9, 17))
read3 <- IRanges(c(16, 22), c(19, 24))
query <- IRangesList(read1, read2, read3)
tx <- IRanges(c(1, 4, 15, 22), c(2, 9, 19, 25))
subject <- IRangesList(tx)
ovenc <- encodeOverlaps(query, subject)
ovenc

## Between 2 GRangesList objects: encoding the overlaps between
## 2 GRangesList has useful applications in the context of an RNA-seq
## experiment for detecting hits between reads and transcripts that
## are compatible with the splicing of the transcript.
## See ?isCompatibleWithSplicing in the GenomicRanges package for
## more information.
}

\keyword{methods}
