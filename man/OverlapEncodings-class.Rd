\name{OverlapEncodings-class}
\docType{class}

% OverlapEncodings objects:
\alias{class:OverlapEncodings}
\alias{OverlapEncodings-class}
\alias{OverlapEncodings}

\alias{length,OverlapEncodings-method}
\alias{Loffset}
\alias{Loffset,OverlapEncodings-method}
\alias{Roffset}
\alias{Roffset,OverlapEncodings-method}
\alias{encoding}
\alias{encoding,OverlapEncodings-method}
\alias{as.data.frame,OverlapEncodings-method}
\alias{show,OverlapEncodings-method}

% encodeOverlaps():
\alias{encodeOverlaps}
\alias{encodeOverlaps,ANY,ANY,Hits-method}
\alias{encodeOverlaps,RangesList,RangesList,missing-method}
\alias{encodeOverlaps,RangesList,Ranges,missing-method}
\alias{encodeOverlaps,Ranges,RangesList,missing-method}
\alias{encodeOverlaps,Ranges,Ranges,missing-method}

\alias{encodeOverlaps1}
\alias{RangesList_encodeOverlaps}


\title{OverlapEncodings objects}

\description{
  The OverlapEncodings class is a container for storing the
  "overlap encodings" returned by the \code{encodeOverlaps}
  function.
}

\usage{
## OverlapEncodings accessors:
\S4method{length}{OverlapEncodings}(x)
Loffset(x)
Roffset(x)
encoding(x)

## Coercing an OverlapEncodings object:
\S4method{as.data.frame}{OverlapEncodings}(x)

## Compute overlap encodings:
encodeOverlaps(query, subject, hits=NULL)
}

\arguments{
  \item{x}{
    An OverlapEncodings object.
  }
  \item{query, subject}{
    2 list-like objects, usually of the same length, where each top-level
    element contains multiple ranges. E.g. \link{RangesList} objects.
    If the 2 objects don't have the same length, the shortest is recycled
    to the length of the longest (the standard recycling rules apply).
  }
  \item{hits}{
    An optional \link{Hits} object that is typically the result of a call
    to \code{\link{findOverlaps}(query, subject)}.
    Supplying \code{hits} is a convenient way to do
    \code{encodeOverlaps(query[queryHits(hits)], subject[subjectHits(hits)])},
    that is,
    calling \code{encodeOverlaps(query, subject, hits)} will perform the
    above.
    In that case, \code{query} and \code{subject} are generally not expected
    to have the same length anymore.
  }
}

\details{
  Given 2 \link{RangesList} objects of the same length \code{query}
  and \code{subject}, or, more generally, given 2 list-like objects
  of the same length where each top-level element contains multiple
  ranges, the "overlap encoding" of the i-th element in \code{query}
  and i-th element in \code{subject} is a character string describing
  how the ranges in \code{query[[i]]} are qualitatively positioned
  relatively to the ranges in \code{subject[[i]]}.

  The \code{encodeOverlaps} function computes those overlap encodings
  and return them in an OverlapEncodings object of the same length
  as \code{query} and \code{subject}.
}

\section{OverlapEncodings accessors}{
  In the following code snippets, \code{x} is an OverlapEncodings object
  typically obtained by a call to \code{encodeOverlaps(query, subject)}.

  \describe{
    \item{}{
      \code{length(x)}:
      Get the number of elements (i.e. encodings) in \code{x}.
      This is equal to \code{length(query)} and \code{length(subject)}.
    }
    \item{}{
      \code{Loffset(x)}, \code{Roffset(x)}:
      Integer vectors of the same length as \code{x} containing the
      "left-offsets" and "right-offsets" of the encodings.

      Let's denote \code{Qi = query[[i]]}, \code{Si = subject[[i]]},
      and [q1,q2] the range covered by \code{Qi} i.e.
      \code{q1 = min(start(Qi))} and \code{q2 = max(end(Qi))},
      then \code{Loffset(x)[i]} is the number \code{L} of ranges at the
      \emph{head} of \code{Si} that are strictly to the left of all
      the ranges in \code{Qi} i.e. \code{L} is the greatest value such that
      \code{end(Si)[k] < q1 - 1} for all \code{k} in \code{seq_len(L)}.
      Similarly, \code{Roffset(x)[i]} is the number \code{R} of ranges at the
      \emph{tail} of \code{Si} that are strictly to the right of all
      the ranges in \code{Qi} i.e. \code{R} is the greatest value such that
      \code{start(Si)[length(Si) + 1 - k] > q2 + 1} for all \code{k}
      in \code{seq_len(L)}.
    }
    \item{}{
      \code{encoding(x)}:
      Factor of the same length as \code{x} where the i-th element is
      the encoding obtained by comparing each range in \code{Qi} with
      all the ranges in \code{tSi = Si[(1+L):(length(Si)-R)]} (\code{tSi}
      stands for "trimmed Si").
      More precisely, here is how this encoding is obtained:
      \enumerate{
        \item All the ranges in \code{Qi} are compared with \code{tSi[1]},
              then with \code{tSi[2]}, etc...
              At each step (one step per range in \code{tSi}), comparing
              all the ranges in \code{Qi} with \code{tSi[k]} is done with
              \code{rangeComparisonCodeToLetter(compare(Qi, tSi[k]))}.
              So at each step, we end up with a vector of \code{M}
              single letters (where \code{M} is \code{length(Qi)}).
        \item Each vector obtained previously (1 vector per range in
              \code{tSi}, all of them of length \code{M}) is turned
              into a single string by pasting its individual letters together.
        \item All the strings obtained previously (1 per range in \code{tSi})
              are pasted together into a single long string and separated
              by colons (\code{":"}). An additional colon is prepended to
              the long string and another one appended to it.
        \item Finally, the value of \code{M} is prepended to the long
              string. The final string is the encoding.
      }
    }
  }
}

\section{Coercing an OverlapEncodings object}{
  In the following code snippets, \code{x} is an OverlapEncodings object.

  \describe{
    \item{}{
      \code{as.data.frame(x)}:
      Return \code{x} as a data frame with columns \code{"Loffset"},
      \code{"Roffset"} and \code{"encoding"}.
    }
  }
}

\section{Compute overlap encodings}{
  In the following code snippets, \code{query} and \code{subject} are
  2 list-like objects where each top-level element contains multiple ranges.

  \describe{
    \item{}{
      \code{encodeOverlaps(query, subject)}:
      When used with the \code{query} and \code{subject} arguments only,
      \code{query} and \code{subject} are expected to have the same length.
      If not, the shortest is recycled to the length of the longest (the
      standard recycling rules apply).
      \code{encodeOverlaps(query, subject)} computes the overlap encodings
      and returns them in an OverlapEncodings object of the same length as
      the input.
    }
    \item{}{
      \code{encodeOverlaps(query, subject, hits)}:
      When used with 3 arguments, the 3rd argument \code{hits} must be
      a \link{Hits} object and is typically the result of a call to
      \code{\link{findOverlaps}(query, subject)}.
      \code{encodeOverlaps(query, subject, hits)} is equivalent to
      \code{encodeOverlaps(query[queryHits(hits)], subject[subjectHits(hits)])}.
      Note that here \code{query} and \code{subject} are generally not
      expected to have the same length anymore.
    }
  }
}

\author{H. Pages}

\seealso{
  \link{compare},
  \link{Hits-class},
  \link{findOverlaps}
}

\examples{
read1 <- IRanges(c(7, 15, 22), c(9, 19, 23))
read2 <- IRanges(c(5, 15), c(9, 17))
read3 <- IRanges(c(16, 22), c(19, 24))
query <- IRangesList(read1, read2, read3)
tx <- IRanges(c(1, 4, 15, 22), c(2, 9, 19, 25))
subject <- IRangesList(tx)
ovenc <- encodeOverlaps(query, subject)
ovenc

length(ovenc)
Loffset(ovenc)
Roffset(ovenc)
encoding(ovenc)
as.data.frame(ovenc)
}

\keyword{methods}
\keyword{classes}
