\name{NCList-class}
\docType{class}

% NCList objects:
\alias{class:NCList}
\alias{NCList-class}
\alias{NCList}

\alias{length,NCList-method}
\alias{names,NCList-method}
\alias{start,NCList-method}
\alias{end,NCList-method}
\alias{width,NCList-method}
\alias{coerce,Ranges,NCList-method}

% NCLists objects:
\alias{class:NCLists}
\alias{NCLists-class}
\alias{NCLists}

\alias{ranges,NCLists-method}
\alias{length,NCLists-method}
\alias{names,NCLists-method}
\alias{start,NCLists-method}
\alias{end,NCLists-method}
\alias{width,NCLists-method}
\alias{elementNROWS,NCLists-method}
\alias{coerce,NCLists,CompressedIRangesList-method}
\alias{coerce,NCLists,IRangesList-method}
\alias{coerce,RangesList,NCLists-method}


\title{Nested Containment List objects}

\description{
  The NCList class is a container for storing the Nested Containment List
  representation of a \link{Ranges} object. Preprocessing a \link{Ranges}
  object as a Nested Containment List allows efficient overlap-based
  operations like \code{\link{findOverlaps}}.

  The NCLists class is a container for storing a collection of NCList objects.
  An NCLists object is typically the result of preprocessing each list
  element of a \link{RangesList} object as a Nested Containment List.
  Like with NCList, the NCLists object can then be used for efficient
  overlap-based operations.

  To preprocess a \link{Ranges} or \link{RangesList} object, simply call
  the \code{NCList} or \code{NCLists} constructor function on it.
}

\usage{
NCList(x, circle.length=NA_integer_)
NCLists(x, circle.length=NA_integer_)
}

\arguments{
  \item{x}{
    The \link{Ranges} or \link{RangesList} object to preprocess.
  }
  \item{circle.length}{
    Use only if the space (or spaces if \code{x} is a \link{RangesList}
    object) on top of which the ranges in \code{x} are defined needs (need)
    to be considered circular. If that's the case, then use
    \code{circle.length} to specify the length(s) of the circular space(s).

    For \code{NCList}, \code{circle.length} must be a single positive
    integer (or NA if the space is linear).

    For \code{NCLists}, it must be an integer vector parallel to \code{x}
    (i.e. same length) and with positive or NA values (NAs indicate linear
    spaces). 
  }
}

\details{
  The \pkg{GenomicRanges} package also defines the
  \code{\link[GenomicRanges]{GNCList}} constructor and class for
  preprocessing and representing a vector of genomic ranges as a
  data structure based on Nested Containment Lists.

  Some important differences between the new findOverlaps/countOverlaps
  implementation based on Nested Containment Lists (BioC >= 3.1) and the
  old implementation based on Interval Trees (BioC < 3.1):
  \itemize{
    \item With the new implementation, the hits returned by
          \code{\link{findOverlaps}} are not \emph{fully} ordered (i.e. ordered
          by queryHits and subject Hits) anymore, but only \emph{partially}
          ordered (i.e. ordered by queryHits only). Other than that, and
          except for the 2 particular situations mentioned below, the 2
          implementations produce the same output. However, the new
          implementation is faster and more memory efficient.
    \item With the new implementation, either the query or the subject can
          be preprocessed with \code{NCList} for a \link{Ranges} object
          (replacement for \code{IntervalTree}), \code{NCLists}
          for a \link{RangesList} object (replacement for
          \code{IntervalForest}), and
          \code{\link[GenomicRanges]{GNCList}} for a
          \link[GenomicRanges]{GenomicRanges} object (replacement for
          \code{GIntervalTree}).
          However, for a one-time use, it is NOT advised to explicitely
          preprocess the input. This is because \code{\link{findOverlaps}}
          or \code{\link{countOverlaps}} will take care of it and do a better
          job at it (by preprocessing only what's needed when it's needed,
          and releasing memory as they go).
    \item With the new implementation, \code{\link{countOverlaps}} on
          \link{Ranges} or \link[GenomicRanges]{GenomicRanges} objects doesn't
          call \code{\link{findOverlaps}} in order to collect all the hits in
          a growing \link{Hits} object and count them only at the end. Instead,
          the counting happens at the C level and the hits are not kept. This
          reduces memory usage considerably when there is a lot of hits.
    \item When \code{minoverlap=0}, zero-width ranges are now interpreted
          as insertion points and considered to overlap with ranges that
          contain them. With the old alogrithm, zero-width ranges were always
          ignored. This is the 1st situation where the new and old
          implementations produce different outputs.
    \item When using \code{select="arbitrary"}, the new implementation will
          generally not select the same hits as the old implementation. This is
          the 2nd situation where the new and old implementations produce
          different outputs.
    \item The new implementation supports preprocessing of a
          \link[GenomicRanges]{GenomicRanges} object with ranges defined
          on circular sequences (e.g. on the mitochnodrial chromosome).
          See \link[GenomicRanges]{GNCList} in the \pkg{GenomicRanges}
          package for some examples.
    \item Objects preprocessed with \code{NCList}, \code{NCLists}, and
          \code{\link[GenomicRanges]{GNCList}} are serializable (with
          \code{save}) for later use. Not a typical thing to do though,
          because preprocessing is very cheap (i.e. very fast and memory
          efficient).
  }
}

\value{
  An NCList object for the \code{NCList} constructor and an NCLists object
  for the \code{NCLists} constructor.
}

\author{Hervé Pagès}

\references{
  Alexander V. Alekseyenko and Christopher J. Lee --
  Nested Containment List (NCList): a new algorithm for accelerating interval
  query of genome alignment and interval databases.
  Bioinformatics (2007) 23 (11): 1386-1393.
  doi: 10.1093/bioinformatics/btl647
}

\seealso{
  \itemize{
    \item The \code{\link[GenomicRanges]{GNCList}} constructor and class
          defined in the \pkg{GenomicRanges} package.

    \item \code{\link{findOverlaps}} for finding/counting interval overlaps
          between two \emph{range-based} objects.

    \item \link{Ranges} and \link{RangesList} objects.
  }
}

\examples{
## The example below is for illustration purpose only and does NOT
## reflect typical usage. This is because, for a one-time use, it is
## NOT advised to explicitely preprocess the input for findOverlaps()
## or countOverlaps(). These functions will take care of it and do a
## better job at it (by preprocessing only what's needed when it's
## needed, and release memory as they go).

query <- IRanges(c(1, 4, 9), c(5, 7, 10))
subject <- IRanges(c(2, 2, 10), c(2, 3, 12))

## Either the query or the subject of findOverlaps() can be preprocessed:

ppsubject <- NCList(subject)
hits1 <- findOverlaps(query, ppsubject)
hits1

ppquery <- NCList(query)
hits2 <- findOverlaps(ppquery, subject)
hits2

## Note that 'hits1' and 'hits2' contain the same hits but not in the
## same order.
stopifnot(identical(sort(hits1), sort(hits2)))
}

\keyword{classes}
\keyword{methods}
