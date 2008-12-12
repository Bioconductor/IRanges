\name{RangesMatchingList-class}
\docType{class}
\alias{RangesMatchingList-class}

% coercion
\alias{as.matrix,RangesMatchingList-method}
\alias{as.table,RangesMatchingList-method}
\alias{t,RangesMatchingList-method}

% accessors
\alias{space,RangesMatchingList-method}
\alias{ranges,RangesMatchingList-method}

\title{List of Matchings between Ranges}
\description{The \code{RangesMatchingList} class stores a set of
  matchings, represented as \code{\linkS4class{RangesMatching}} instances,
  between the ranges in one \code{\linkS4class{RangesList}} instance and the
  ranges in another.}

\details{
  Roughly the same set of utilities are provided for
  \code{RangesMatchingList} as for \code{RangesMatching}:
  
  The \code{as.matrix} method coerces a \code{RangesMatchingList} in a
  similar way to \code{RangesMatching}, except a column is prepended
  that indicates which space (or element in the query \code{RangesList})
  to which the row corresponds.

  The \code{as.table} method flattens or unlists the list, counts the
  number of matchings for each query range and outputs the counts as a
  \code{table}, which has the same shape as from a single \code{RangesMathing}.

  To transpose a \code{RangesMatchingList} \code{x}, so that the subject
  and query in each space are interchanged, call \code{t(x)}. This
  allows, for example, counting the number of subjects that matched
  using \code{as.table}.

  To get the actual regions of intersection between the overlapping
  ranges, use the \code{ranges} accessor.
}

\section{Coercion}{
  In the code snippets below, \code{x} is a \code{RangesMatchingList}
  object.
  
  \describe{
    \item{}{\code{as.matrix(x)}: Coerces \code{x} to a three
      column integer matrix, with each row representing a matching
      between a query index (second column) and subject index (third
      column). The first column indicates the space in the query
      \code{RangesList}.
    }
    \item{}{\code{as.table(x)}: counts the number of matchings for each
      query range in \code{x} and outputs the counts as a \code{table}.
    }
    \item{}{\code{t(x)}: Interchange the query and subject in each space
      of \code{x}, returns a transposed \code{RangesMatchingList}.}
  }
}

\section{Accessors}{
  \describe{
    \item{}{\code{space(x)}: gets the character vector naming the space
      in the query \code{RangesList} for each match, or \code{NULL} if the
      query did not have any names.
    }
    \item{}{\code{ranges(x, query, subject)}: returns a \code{RangesList}
      holding the intersection of the ranges in the
      \code{RangesList} instances \code{query} and \code{subject}, which
      should be the same subject and query used to generate
      \code{x}. Eventually, we might store the query and subject inside
      \code{x}, in which case the arguments would be redundant.
    }
  }
}

\note{This class is highly experimental. It has not been well tested and
  may disappear at any time.}

\author{ Michael Lawrence }
\seealso{
\code{\link{overlap}}, which generates an instance of this class.
}
\examples{
}
\keyword{methods}
\keyword{classes}
