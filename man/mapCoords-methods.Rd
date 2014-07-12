\name{mapCoords-methods}
\docType{class}
\alias{mapCoords-methods}

% generics
\alias{mapCoords}
\alias{pmapCoords}

\title{Mapping of ranges to another sequence}

\description{
  The \code{mapCoords} generic converts a set of ranges to the equivalent
  ranges on another sequence through some sort of alignment between
  sequences. The output is an object of the same class as \code{x}
  and in general will contain the mapped ranges with the matching data
  as metadata. 

  Matching data are the result of calling \code{findOverlaps} with
  \code{type = "within"}. This operation matches each input range to a 
  destination sequence (useful when the alignment is one/many to many). 

  The \code{pmapCoords} function is simpler: it treats the two inputs as 
  parallel vectors, maps each input range via the corresponding alignment, 
  and returns the mapped ranges. There is one result per input element, 
  instead of the many-to-many result from \code{mapCoords}.
}

\usage{
mapCoords(x, to, ...)
pmapCoords(x, to, ...)
}

\arguments{
  \item{x}{Typically an object containing ranges to map.}
  \item{to}{Typically an object representing an alignment.}
  \item{...}{Arguments to pass to methods}
}

\value{
  An object the same class as \code{x}. The result of 
  \code{findOverlaps} with type = ``within`` are inlcuded as 
  metadata columns (`queryHits` and `subjectHits`).
}

\author{ Michael Lawrence }

\seealso{
  \itemize{
    \item See \code{?`\link[GenomicRanges]{mapCoords-methods}`} in the
      \pkg{GenomicRanges} package for the \code{mapCoords} method on 
      GRanges objects
    \item See \code{?`\link[GenomicAlignments]{mapCoords-methods}`} in the
      \pkg{GenomicAlignments} package for the \code{mapCoords} method on
      GAlignments objects
  }
}

\examples{
## See methods in GenomicRanges and GAlignmentsPackages.
}

\keyword{methods}
\keyword{classes}
