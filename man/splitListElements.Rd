\name{splitListElements}

\alias{INCOMPATIBLE_SPLITTER_MSG}
\alias{splitListElements}
\alias{equisplit}

\title{Split the list elements of a list-like object}

\description{
  2 utilities for splitting the list elements of a list-like object.
}

\usage{
splitListElements(x, splitter, use.mcols=FALSE,
                  msg.if.incompatible=INCOMPATIBLE_SPLITTER_MSG)

equisplit(x, nchunk, chunksize, use.mcols=FALSE)
}

\arguments{
  \item{x}{
    The list-like object with list elements to be split.

    Can be any \link{List} derivative for \code{splitListElements}.
    Can also be an ordinary list if \code{splitListElements} is called
    with \code{use.mcols=TRUE}.

    Can be any \link{List} derivative that supports \code{relist()}
    for \code{equisplit}.
  }
  \item{splitter}{
    A \link{Partitioning} derivative describing how to split the list
    elements in \code{x}. It must be compatible with the \emph{cumulated
    length} of all the list elements in \code{x}.
  }
  \item{use.mcols}{
    Whether to propagate the metadata columns on \code{x} (if any) or not.

    Must be \code{TRUE} or \code{FALSE} (the default).
    If set to \code{FALSE}, instead of having the metadata columns propagated
    from \code{x}, the object returned by \code{splitListElements} has metadata
    columns \code{revmap} and \code{revmap2} and the object returned by
    \code{equisplit} has metadata column \code{revmap}. Note that this is the
    default.
  }
  \item{msg.if.incompatible}{
    The error message to use if \code{splitter} is not compatible with
    the \emph{cumulated length} of all the list elements in \code{x}.
  }
  \item{nchunk}{
    The number of chunks. Must be a single positive integer.
  }
  \item{chunksize}{
    The size of the chunks (last chunk might be smaller). Must be a single
    positive integer.
  }
}

\details{
  \code{splitListElements} is a low-level utility that splits the
  list elements of list-like object \code{x} according to \code{splitter}.

  \code{equisplit} splits list-like object \code{x} into a specified
  number of partitions with equal (total) width. This is useful for
  instance to ensure balanced loading of workers in parallel evaluation.
  For example, if \code{x} is a \link[GenomicRanges]{GRanges} object,
  each partition is also a \link[GenomicRanges]{GRanges} object and the
  set of all partitions is returned as a \link[GenomicRanges]{GRangesList}
  object.
}

\value{
  An object of the same class as \code{x} for \code{splitListElements}.

  An object of class \code{\link[S4Vectors]{relistToClass}(x)} for
  \code{equisplit}.
}

\author{Hervé Pagès}

\seealso{
  \itemize{
    \item \link{IRanges} and \link{IRangesList} objects.

    \item \link{Partitioning} objects.

    \item \link{IntegerList} objects.

    \item \code{\link{breakInChunks}} from breaking a vector-like object
          in chunks.

    \item \link[GenomicRanges]{GRanges} and \link[GenomicRanges]{GRangesList}
          objects defined in the \pkg{GenomicRanges} package.

    \item \link[S4Vectors]{List} objects defined in the \pkg{S4Vectors}
          package.

    \item \link{intra-range-methods} and \link{inter-range-methods}
          for intra range and inter range transformations.
  }
}

\examples{
## ---------------------------------------------------------------------
## SIMPLE EXAMPLES
## ---------------------------------------------------------------------

x <- IRanges(c(1, 101, 1001, 10001), width=c(10, 5, 0, 12),
             names=letters[1:4])
mcols(x)$label <- LETTERS[1:4]
x

splitter <- PartitioningByWidth(c(3, 9, 13, 0, 2))
splitListElements(x, splitter)
splitListElements(x, splitter, use.mcols=TRUE)

## equisplit() first calls breakInChunks() internally to create a
## PartitioningByWidth object that describes the geometry of the chunks,
## then splits the ranges in 'x' by calling splitListElements() on 'x'
## and on the splitter returned by breakInChunks(). Finally the IRanges
## object returned by splitListElements() is split into an IRangesList
## object where each list element corresponds to a chunk in the splitter.
equisplit(x, nchunk=2)
equisplit(x, nchunk=2, use.mcols=TRUE)

equisplit(x, chunksize=5)

x2 <- as(x, "IntegerList")
splitListElements(x2, splitter)

x3 <- as.list(x2)
splitListElements(x3, splitter, use.mcols=TRUE)

library(GenomicRanges)
gr <- GRanges(c("chr1", "chr2"), IRanges(1, c(100, 1e5)))
equisplit(gr, nchunk=2)
equisplit(gr, nchunk=1000)

## ---------------------------------------------------------------------
## SPLIT ALL LIST ELEMENTS INTO LENGTH-1 LIST ELEMENTS
## ---------------------------------------------------------------------

## First we construct a Partitioning object where all the partitions
## have a width of 1:
x_nobj <- nobj(PartitioningByWidth(x))  # Equivalent to length(unlist(x))
                                        # except that it doesn't unlist
                                        # 'x' so is much more efficient.
splitter1 <- PartitioningByEnd(seq_len(x_nobj))
splitter1

## Then we use it to split 'x':
splitListElements(x, splitter1)
splitListElements(x2, splitter1)
splitListElements(x3, splitter1, use.mcols=TRUE)

## ---------------------------------------------------------------------
## SPLIT THE PARTITIONS REPRESENTED BY A Partitioning OBJECT
## ---------------------------------------------------------------------

partitioning0 <- PartitioningByEnd(x2)  # same as PartitioningByEnd(x)
splitListElements(partitioning0, splitter)

## Note that when the 1st arg is a Partitioning derivative, then
## swapping the 1st and 2nd elements in the call to splitListElements()
## returns the same partitions:
splitListElements(splitter, partitioning0)

## ---------------------------------------------------------------------
## SANITY CHECKS
## ---------------------------------------------------------------------

## If 'splitter' is 'PartitioningByEnd(x)' or 'PartitioningByWidth(x)'
## and 'x' has no zero-length list elements, then
## 'splitListElements(x, splitter, use.mcols=TRUE)' is a no-op.
check_no_ops <- function(x) {
  splitter <- PartitioningByEnd(x)
  stopifnot(identical(
    splitListElements(x, splitter, use.mcols=TRUE),
    x
  ))
  splitter <- PartitioningByWidth(x)
  stopifnot(identical(
    splitListElements(x, splitter, use.mcols=TRUE),
    x
  ))
}

check_no_ops(x[lengths(x) != 0])
check_no_ops(x2[lengths(x2) != 0])
check_no_ops(x3[lengths(x3) != 0])
check_no_ops(gr)
}

\keyword{utilities}
