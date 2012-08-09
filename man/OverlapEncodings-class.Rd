\name{OverlapEncodings-class}
\docType{class}

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
\alias{levels,OverlapEncodings-method}
\alias{flippedQuery}
\alias{flippedQuery,OverlapEncodings-method}

\alias{Lencoding}
\alias{Lencoding,character-method}
\alias{Lencoding,factor-method}
\alias{Lencoding,OverlapEncodings-method}
\alias{Rencoding}
\alias{Rencoding,character-method}
\alias{Rencoding,factor-method}
\alias{Rencoding,OverlapEncodings-method}

\alias{ngap,character-method}
\alias{ngap,factor-method}
\alias{ngap,OverlapEncodings-method}
\alias{Lngap}
\alias{Lngap,character-method}
\alias{Lngap,factor-method}
\alias{Lngap,OverlapEncodings-method}
\alias{Rngap}
\alias{Rngap,character-method}
\alias{Rngap,factor-method}
\alias{Rngap,OverlapEncodings-method}

\alias{as.data.frame,OverlapEncodings-method}
\alias{show,OverlapEncodings-method}


\title{OverlapEncodings objects}

\description{
  The OverlapEncodings class is a container for storing the
  "overlap encodings" returned by the \code{\link{encodeOverlaps}}
  function.
}

\usage{
## OverlapEncodings accessors:

\S4method{length}{OverlapEncodings}(x)
\S4method{Loffset}{OverlapEncodings}(x)
\S4method{Roffset}{OverlapEncodings}(x)
\S4method{encoding}{OverlapEncodings}(x)
\S4method{levels}{OverlapEncodings}(x)
\S4method{flippedQuery}{OverlapEncodings}(x)

\S4method{Lencoding}{OverlapEncodings}(x)
\S4method{Rencoding}{OverlapEncodings}(x)

\S4method{ngap}{OverlapEncodings}(x)
\S4method{Lngap}{OverlapEncodings}(x)
\S4method{Rngap}{OverlapEncodings}(x)

## Coercing an OverlapEncodings object:

\S4method{as.data.frame}{OverlapEncodings}(x, row.names=NULL, optional=FALSE, ...)

## Low-level related utilities:

\S4method{Lencoding}{character}(x)
\S4method{Rencoding}{character}(x)
\S4method{ngap}{character}(x)
\S4method{Lngap}{character}(x)
\S4method{Rngap}{character}(x)

\S4method{Lencoding}{factor}(x)
\S4method{Rencoding}{factor}(x)
\S4method{ngap}{factor}(x)
\S4method{Lngap}{factor}(x)
\S4method{Rngap}{factor}(x)
}

\arguments{
  \item{x}{
    An OverlapEncodings object. For the low-level utilities, \code{x} can also
    be a character vector or factor containing encodings.
  }
  \item{row.names}{
    \code{NULL} or a character vector.
  }
  \item{optional, ...}{
    Ignored.
  }
}

\details{
  Given a \code{query} and a \code{subject} of the same length, both
  list-like objects with top-level elements typically containing multiple
  ranges (e.g. \link{RangesList} objects), the "overlap encoding" of the
  i-th element in \code{query} and i-th element in \code{subject} is a
  character string describing how the ranges in \code{query[[i]]} are
  \emph{qualitatively} positioned relatively to the ranges in
  \code{subject[[i]]}.

  The \code{\link{encodeOverlaps}} function computes those overlap
  encodings and returns them in an OverlapEncodings object of the same
  length as \code{query} and \code{subject}.

  The topic of working with overlap encodings is covered in details
  in the "Overlap encodings" vignette in the GenomicRanges package.
}

\section{OverlapEncodings accessors}{
  In the following code snippets, \code{x} is an OverlapEncodings object
  typically obtained by a call to \code{\link{encodeOverlaps}(query, subject)}.

  \describe{
    \item{}{
      \code{length(x)}:
      Get the number of elements (i.e. encodings) in \code{x}.
      This is equal to \code{length(query)} and \code{length(subject)}.
    }
    \item{}{
      \code{Loffset(x)}, \code{Roffset(x)}:
      Get the "left offsets" and "right offsets" of the encodings,
      respectively. Both are integer vectors of the same length as \code{x}.

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
              into a single string (called "encoding block") by pasting
              its individual letters together.
        \item All the encoding blocks (1 per range in \code{tSi}) are pasted
              together into a single long string and separated by colons
              (\code{":"}). An additional colon is prepended to the long
              string and another one appended to it.
        \item Finally, a special block containing the value of \code{M} is
              prepended to the long string. The final string is the encoding.
      }
    }
    \item{}{
      \code{levels(x)}: Equivalent to \code{levels(encoding(x))}.
    }
    \item{}{
      \code{flippedQuery(x)}:
      Whether or not the top-level element in query used for computing the
      encoding was "flipped" before the encoding was computed.
      Note that this flipping generally affects the "left offset",
      "right offset", in addition to the encoding itself.
    }
    \item{}{
      \code{Lencoding(x)}, \code{Rencoding(x)}:
      Extract the "left encodings" and "right encodings" of paired-end
      encodings.

      Paired-end encodings are obtained by encoding paired-end overlaps
      i.e. overlaps between paired-end reads and transcripts (typically).
      The difference between a single-end encoding and a paired-end encoding
      is that all the blocks in the latter contain a \code{"--"} separator
      to mark the separation between the "left encoding" and the "right
      encoding".

      See the "Overlap encodings" vignette in the GenomicRanges package
      for examples of paired-end encodings.
    }
    \item{}{
      \code{ngap(x)}, \code{Lngap(x)}, \code{Rngap(x)}:
      Extract the number of gaps in each encoding by looking at their first
      block (aka special block).
      If an element \code{xi} in \code{x} is a paired-end encoding,
      then \code{Lngap(xi)}, \code{Rngap(xi)}, and \code{ngap(xi)},
      return \code{ngap(Lencoding(xi))}, \code{ngap(Rencoding(xi))},
      and \code{Lngap(xi) + Rngap(xi)}, respectively.
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

\author{H. Pages}

\seealso{
  \itemize{
    \item The "Overlap encodings" vignette in the GenomicRanges package.
    \item \code{\link{encodeOverlaps}}.
    \item \code{\link{compare}} for the interpretation of the string
      returned by \code{encoding}.
    \item The \link{RangesList} class.
  }
}

\examples{
example(encodeOverlaps)  # to make 'ovenc'

length(ovenc)
Loffset(ovenc)
Roffset(ovenc)
encoding(ovenc)
levels(ovenc)
nlevels(ovenc)
flippedQuery(ovenc)
ngap(ovenc)

as.data.frame(ovenc)
ngap(levels(ovenc))
}

\keyword{methods}
\keyword{classes}
