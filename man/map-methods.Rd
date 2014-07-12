\name{map-methods}
\docType{class}
\alias{map-methods}

% generics
\alias{map}
\alias{pmap}

\title{Mapping of ranges to another sequence}

\description{
  \code{map} and \code{pmap} are deprecated. Use \code{mapRanges}
  and \code{pmapRanges} instead. 

  The \code{map} generic converts a set of ranges to the equivalent
  ranges on another sequence, through some sort of alignment between
  sequences, and outputs a GRangesMapping object. 
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

\author{ Michael Lawrence }

\seealso{
  \itemize{
    \item See ?\code{mapCoords} and \code{pmapCoords}.
  }
}

\keyword{methods}
\keyword{classes}
