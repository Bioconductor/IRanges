\name{encodeOverlaps}

\alias{encodeOverlaps}
\alias{encodeOverlaps,Vector,Vector,Hits-method}
\alias{encodeOverlaps,RangesList,RangesList,missing-method}
\alias{encodeOverlaps,RangesList,RangesList,Hits-method}
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
encodeOverlaps(query, subject, hits=NULL, ...)
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
  \item{...}{
    Additional arguments for methods.
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
  \link[GenomicRanges]{isCompatibleWithSplicing},
  \link{Hits-class},
  \link{findOverlaps},
  \link{RangesList-class}
}

\examples{
## ---------------------------------------------------------------------
## A. BETWEEN 2 RangesList OBJECTS
## ---------------------------------------------------------------------
## In the context of an RNA-seq experiment, encoding the overlaps
## between 2 GRangesList objects, one containing the reads (the query),
## and one containing the transcripts (the subject), can be used for
## detecting hits between reads and transcripts that are "compatible"
## with the splicing of the transcript. Here we illustrate this with 2
## RangesList objects, in order to keep things simple:

## 4 aligned reads in the query:
read1 <- IRanges(c(7, 15, 22), c(9, 19, 23))  # 2 gaps
read2 <- IRanges(c(5, 15), c(9, 17))  # 1 gap
read3 <- IRanges(c(16, 22), c(19, 24))  # 1 gap
read4 <- IRanges(c(16, 23), c(19, 24))  # 1 gap
query <- IRangesList(read1, read2, read3, read4)

## 1 transcript in the subject:
tx <- IRanges(c(1, 4, 15, 22, 38), c(2, 9, 19, 25, 47))  # 5 exons
subject <- IRangesList(tx)

## Encode the overlaps:
ovenc <- encodeOverlaps(query, subject)
ovenc
encoding(ovenc)

## Reads that are "compatible" with the transcript can be detected with
## a regular expression (the regular expression below assumes that
## reads have at most 2 gaps):
regex0 <- "(:[fgij]:|:[jg].:.[gf]:|:[jg]..:.g.:..[gf]:)"
grepl(regex0, encoding(ovenc))  # read4 is NOT "compatible"

## This was for illustration purpose only. In practise you don't need
## (and should not) use this regular expression, but use instead the
## isCompatibleWithSplicing() utility function defined in the
## GenomicRanges package. See '?isCompatibleWithSplicing' in the
## GenomicRanges package for more information.

## ---------------------------------------------------------------------
## B. BETWEEN 2 GRangesList OBJECTS
## ---------------------------------------------------------------------
## With real RNA-seq data, the reads and transcripts will typically be
## stored in GRangesList objects. See '?isCompatibleWithSplicing' in the
## GenomicRanges package for more information.
}

\keyword{methods}
