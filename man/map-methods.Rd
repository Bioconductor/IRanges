\name{map-methods}
\docType{class}
\alias{map-methods}

% generics
\alias{map}
\alias{pmap}

\title{Mapping of ranges to another sequence}

\description{
  The \code{map} generic converts a set of ranges to the equivalent
  ranges on another sequence, through some sort of alignment between
  sequences, and outputs a \link[GenomicRanges]{GRangesMapping} object. 
  There are two components of that object: the transformed \code{granges}, 
  and the \code{hits}, a \code{\linkS4class{Hits}} object of the same length
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
  A \link[GenomicRanges]{GRangesMapping} object, which consists of a
  \code{Hits} instance and \link[GenomicRanges]{GRanges} instance.
}

\author{ Michael Lawrence }

\seealso{
  \itemize{
    \item See \code{?`\link[GenomicRanges]{GRangesMapping}`} in the
      \pkg{GenomicRanges} package for the GRangesMapping class
    \item See \code{?`\link[GenomicRanges]{map-methods}`} in the
      \pkg{GenomicRanges} package for the \code{map} method on 
      GRanges objects
    \item See \code{?`\link[GenomicAlignments]{map-methods}`} in the
      \pkg{GenomicAlignments} package for the \code{map} method on
      GAlignments objects
  }
}

\keyword{methods}
\keyword{classes}
