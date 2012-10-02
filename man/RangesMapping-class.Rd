\name{RangesMapping-class}
\docType{class}
\alias{RangesMapping-class}

% generic
\alias{map}
\alias{pmap}

% accessors
\alias{hits}
\alias{ranges,RangesMapping-method}
\alias{space,RangesMapping-method}
\alias{dim,RangesMapping-method}
\alias{length,RangesMapping-method}
\alias{queryHits,RangesMapping-method}
\alias{subjectHits,RangesMapping-method}

% coercion
\alias{coerce,RangesMapping,RangedData-method}

\title{Mapping of ranges to another sequence}

\description{
  The \code{map} generic converts a set of ranges to the equivalent
  ranges on another sequence, through some sort of alignment between
  sequences, and outputs a \code{RangesMapping} object. There are three
  primary components of that object: the transformed \code{ranges}, the
  \code{space} (destination sequence) for the ranges, and the
  \code{hits}, a \code{\linkS4class{Hits}} object of the same length
  that matches each input range to a destination sequence (useful when
  the alignment is one/many to many). The \code{pmap} function is
  simpler: it treats the two inputs as parallel vectors, maps each input
  range via the corresponding alignment, and returns the mapped
  ranges. There is one result per input element, instead of the
  many-to-many result from \code{map}.
}

\usage{
map(from, to, ...)
pmap(from, to, ...)
}

\arguments{
  \item{from}{Typically an object containing ranges to map.}
  \item{to}{Typically an object representing an alignment.}
  \item{...}{Arguments to pass to methods}
}

\value{
  A \code{RangesMapping} object, as documented here.
}

\section{\code{RangesMapping} Accessors}{
  \describe{
    \item{}{\code{ranges(x)}: Gets the mapped ranges.}
    \item{}{\code{space(x)}: Gets the destination spaces (sequence
      names).}
    \item{}{\code{hits(x)}: Gets the matching between the input ranges
      and the destination sequences (of which there may be more than
      one).}
    \item{}{\code{dim(x)}: Same as \code{dim(hits(x))}.}
    \item{}{\code{length(x)}: Same as \code{length(hits(x))}.}
    \item{}{\code{subjectHits(x)}: Same as \code{subjectHits(hits(x))}.}
    \item{}{\code{queryHits(x)}: Same as \code{queryHits(hits(x))}.}
  }
}

\section{\code{RangesMapping} Coercion}{
  \describe{
    \item{}{\code{as(from, "RangedData")}: Converts
      a \code{RangesMapping} into
      a \code{\linkS4class{RangedData}}. The \code{ranges}/\code{space}
      in the \code{RangedData} are the \code{ranges}/\code{space}
      of \code{from}, and the \code{values} result from the coercion of
      the \code{hits} to a \code{DataFrame}.
    }
  }
}

\author{ Michael Lawrence }

\seealso{
  Methods on the generic \code{map}, which generates an instance of
  this class, are defined in other packages, like GenomicRanges.
}

\keyword{methods}
\keyword{classes}
