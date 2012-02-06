\name{Hits-class}
\docType{class}
\alias{Hits-class}
\alias{RangesMatching-class}

% accessors
\alias{matchMatrix}
\alias{matchMatrix,Hits-method}
\alias{dim,Hits-method}
\alias{ranges,Hits-method}
\alias{length,Hits-method}
\alias{subjectHits}
\alias{subjectHits,Hits-method}
\alias{queryHits}
\alias{queryHits,Hits-method}

% coercion
\alias{as.matrix,Hits-method}
\alias{as.data.frame,Hits-method}
\alias{as.table,Hits-method}
\alias{t,Hits-method}
\alias{as.list,Hits-method}
\alias{coerce,Hits,list-method}
\alias{coerce,Hits,List-method}
\alias{coerce,RangesMatching,list-method}
\alias{coerce,RangesMatching,List-method}

\title{Set of hits between 2 vector-like objects}
\description{The \code{Hits} class stores a set of "hits"
  between the elements in one vector-like object (called the "query")
  and the elements in another (called the "subject"). Currently,
  \code{Hits} are used to represent the result of a call to
  \code{\link{findOverlaps}}, though other operations producing "hits"
  are imaginable.}

\details{
  The \code{as.matrix} and \code{as.data.frame} methods coerce a \code{Hits}
  object to a two column \code{matrix} or \code{data.frame} with one row for
  each hit, where the value in the first column is the index of an element in
  the query and the value in the second column is the index of an element in
  the subject.

  The \code{as.table} method counts the number of hits for each
  query element and outputs the counts as a \code{table}.

  To transpose a \code{Hits} \code{x}, so that the subject and query
  are interchanged, call \code{t(x)}. This allows, for example, counting
  the number of hits for each subject element using \code{as.table}.

  When the Hits object is the result of a call to \code{\link{findOverlaps}},
  the actual regions of intersection between the overlapping ranges
  can be obtained with the \code{ranges} accessor.
}

\section{Coercion}{
  In the code snippets below, \code{x} is a \code{Hits}
  object.
  
  \describe{
    \item{}{\code{as.matrix(x)}: Coerces \code{x} to a two
      column integer matrix, with each row representing a hit
      between a query index (first column) and subject index (second
      column).
    }
    \item{}{\code{as.data.frame(x)}: Same as \code{as.matrix(x)} except that
      it returns a data frame instead of a matrix.
    }
    \item{}{\code{as.table(x)}: counts the number of hits for each
      query element in \code{x} and outputs the counts as a \code{table}.
    }
    \item{}{\code{t(x)}: Interchange the query and subject in \code{x},
      returns a transposed \code{Hits}.}
    \item{}{\code{as.list(x)}: Returns a list with an element for each
      query, where each element contains the indices of the subjects
      that have a hit with the corresponding query.
    }
    \item{}{\code{as(x, "List")}: Like \code{as.list}, above.
    }
  }
}

\section{Accessors}{
  \describe{
    \item{}{\code{queryHits(x)}: Equivalent to \code{as.data.frame(x)[[1]]}.}
    \item{}{\code{subjectHits(x)}: Equivalent to \code{as.data.frame(x)[[2]]}.}
    \item{}{\code{ranges(x, query, subject)}: returns a \code{Ranges}
      holding the intersection of the ranges in the
      \code{Ranges} objects \code{query} and \code{subject}, which
      should be the same subject and query used in the call to
      \code{findOverlaps} that generated \code{x}. Eventually, we might
      store the query and subject inside \code{x}, in which case the
      arguments would be redundant.
    }
    \item{}{\code{length(x)}: get the number of hits}
    \item{}{\code{nrow(x)}: get the number of elements in the query}
    \item{}{\code{ncol(x)}: get the number of elements in the subject}
    \item{}{
      \code{dim(x)}: get a two-element integer vector, essentially
      \code{c(nrow(x), ncol(x))}.
    }
  }
}

\author{ Michael Lawrence }
\seealso{
\code{\link{findOverlaps}}, which generates an instance of this class.
}
\examples{
  query <- IRanges(c(1, 4, 9), c(5, 7, 10))
  subject <- IRanges(c(2, 2, 10), c(2, 3, 12))
  tree <- IntervalTree(subject)
  matchings <- findOverlaps(query, tree)

  as.matrix(matchings)
  as.data.frame(matchings)

  as.table(matchings) # hits per query
  as.table(t(matchings)) # hits per subject
}
\keyword{methods}
\keyword{classes}
