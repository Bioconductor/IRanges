\name{Hits-class-leftovers}
\docType{class}

\alias{Hits-examples}

% coercion
\alias{coerce,Hits,DataFrame-method}
\alias{as.data.frame,Hits-method}

\alias{coerce,SortedByQueryHits,PartitioningByEnd-method}
\alias{coerce,SortedByQueryHits,Partitioning-method}
\alias{coerce,SortedByQueryHits,Ranges-method}
\alias{coerce,SortedByQueryHits,IRanges-method}

\alias{coerce,SortedByQueryHits,CompressedIntegerList-method}
\alias{coerce,SortedByQueryHits,IntegerList-method}
\alias{coerce,SortedByQueryHits,List-method}
\alias{as.list,SortedByQueryHits-method}

\alias{coerce,Hits,CompressedIntegerList-method}
\alias{coerce,Hits,IntegerList-method}
\alias{coerce,Hits,List-method}
\alias{as.list,Hits-method}
\alias{coerce,Hits,Grouping}

\title{Examples of basic manipulation of Hits objects}

\description{
  IMPORTANT NOTE - 4/29/2014: This man page is being refactored. Most of
  the things that used to be documented here have been moved to the man
  page for \link[S4Vectors]{Hits} objects located in the \pkg{S4Vectors}
  package.
}

\details{
  The \code{as.data.frame} method coerces a \code{Hits} object to a two column
  \code{data.frame} with one row for each hit, where the value in the first
  column is the index of an element in the query and the value in the second
  column is the index of an element in the subject.
}

\section{Coercion}{
  In the code snippets below, \code{x} is a \code{Hits}
  object.
  
  \describe{
    \item{}{
      \code{as(x, "DataFrame")}: Creates a \code{DataFrame} by
      combining the result of \code{as.matrix(x)}
      with \code{mcols(from)}.
    }
    \item{}{\code{as.data.frame(x)}: Attempts to coerce the result of
      \code{as(from, "DataFrame")} to a \code{data.frame}.
    }
    \item{}{\code{as.list(x)}: Coerces \code{x} to a list of integers,
      grouping the the right node hits for each left node.
    }
    \item{}{\code{as(x, "List")}: Analogous to \code{as.list(x)}.
    }
    \item{}{\code{as(x, "Grouping")}: Returns roughly the same object as
      \code{as(x, "List")}, except it is a ManyToManyGrouping, i.e., it
      knows the number of right nodes.
    }
  }
}

\seealso{
  The \link[S4Vectors]{Hits} class defined and documented in the
  \pkg{S4Vectors} package.
}
\examples{
query <- IRanges(c(1, 4, 9), c(5, 7, 10))
subject <- IRanges(c(2, 2, 10), c(2, 3, 12))
hits <- findOverlaps(query, subject)

as.matrix(hits)
as.data.frame(hits)

as.table(hits) # hits per query
as.table(t(hits)) # hits per subject

## Turn a Hits object into an IntegerList object with one list element
## per element in the original query.
as(hits, "IntegerList")
as(hits, "List")  # same as as(hits, "IntegerList")

## Turn a Hits object into a PartitioningByEnd object that describes
## the grouping of hits by query.
as(hits, "PartitioningByEnd")
as(hits, "Partitioning")  # same as as(hits, "PartitioningByEnd")

## ---------------------------------------------------------------------
## remapHits()
## ---------------------------------------------------------------------

hits2 <- remapHits(hits,
                   Rnodes.remapping=factor(c("e", "e", "d"), letters[1:5]))
hits2

hits3 <- remapHits(hits,
                   Rnodes.remapping=c(5, 5, 4), new.nRnode=5)
hits3
stopifnot(identical(hits2, hits3))
}
\keyword{methods}
\keyword{classes}
