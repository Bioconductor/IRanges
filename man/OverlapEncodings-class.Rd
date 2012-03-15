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


\title{OverlapEncodings objects}

\description{
  The OverlapEncodings class is a container for storing the
  "overlap encodings" returned by the \code{encodeOverlaps}
  function.
}

\usage{
## OverlapEncodings accessors:
length(x)
Loffset(x)
Roffset(x)
encoding(x)

## Coercing an OverlapEncodings object:
as.data.frame(x)

## Compute overlap encodings:
encodeOverlaps(query, subject, hits=NULL)
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
              At each step, comparing all the ranges in \code{Qi} with
              \code{tSi[k]} is done with
              \code{\link{rangeComparisonCodeToLetter}(\link{compare}(Qi, tSi[k]))}.
              This returns a vector of single letters of length \code{M}
              where \code{M} is \code{length(Qi)}
              (see \code{?\link{compare}} for the meaning of those letters).
              This vector is then turned into a single string of \code{M}
              letters by pasting all the individual letters together.
        \item The words obtained at the previous step (1 word per range
              in \code{tSi} are pasted together into a single character
              string and separated by colons (\code{":"}). One colon is
              added at the beginning of the string and another one at the end.
        \item TODO
      }
    }
  }
}

\author{H. Pages}

\seealso{
  \link{compare},
  \link{Vector-class}
}

\keyword{methods}
\keyword{classes}
