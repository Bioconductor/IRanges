\name{HitsList-class}
\docType{class}
\alias{HitsList-class}

% coercion
\alias{as.matrix,HitsList-method}
\alias{as.table,HitsList-method}
\alias{t,HitsList-method}

% accessors
\alias{space,HitsList-method}
\alias{ranges,HitsList-method}
\alias{subjectHits,HitsList-method}
\alias{queryHits,HitsList-method}

\title{List of Hits objects}
\description{The \code{HitsList} class stores a set of Hits objects.
  It's typically used to represent the result of \code{findOverlaps}
  on two \code{\linkS4class{RangesList}} objects.}

\details{
  Roughly the same set of utilities are provided for
  \code{HitsList} as for \code{Hits}:
  
  The \code{as.matrix} method coerces a \code{HitsList} in a
  similar way to \code{Hits}, except a column is prepended
  that indicates which space (or element in the query \code{RangesList})
  to which the row corresponds.

  The \code{as.table} method flattens or unlists the list, counts the
  number of hits for each query range and outputs the counts as a
  \code{table}, which has the same shape as from a single \code{Hits}
  object.

  To transpose a \code{HitsList} \code{x}, so that the subject
  and query in each space are interchanged, call \code{t(x)}. This
  allows, for example, counting the number of hits for each subject
  element using \code{as.table}.

  When the HitsList object is the result of a call to
  \code{\link{findOverlaps}} on two \code{\linkS4class{RangesList}} objects,
  the actual regions of intersection between the overlapping
  ranges can be obtained with the \code{ranges} accessor.
}

\section{Coercion}{
  In the code snippets below, \code{x} is a \code{HitsList}
  object.
  
  \describe{
    \item{}{\code{as.matrix(x)}: calls \code{as.matrix} on each
      \code{Hits}, combines them row-wise and offsets the
      indices so that they are aligned with the result of calling
      \code{unlist} on the query and subject.
    }
    \item{}{\code{as.table(x)}: counts the number of hits for each
      query element in \code{x} and outputs the counts as a \code{table},
      which is aligned with the result of calling \code{unlist}
      on the query.
    }
    \item{}{\code{t(x)}: Interchange the query and subject in each space
      of \code{x}, returns a transposed \code{HitsList}.}
  }
}

\section{Accessors}{
  \describe{
    \item{}{\code{queryHits(x)}: Equivalent to
      \code{unname(as.matrix(x)[,1])}.
    }
    \item{}{\code{subjectHits(x)}: Equivalent to
      \code{unname(as.matrix(x)[,2])}.
    }
    \item{}{\code{space(x)}: gets the character vector naming the space
      in the query \code{RangesList} for each hit, or \code{NULL} if the
      query did not have any names.
    }
    \item{}{\code{ranges(x, query, subject)}: returns a \code{RangesList}
      holding the intersection of the ranges in the
      \code{RangesList} objects \code{query} and \code{subject}, which
      should be the same subject and query used in the call to
      \code{findOverlaps} that generated \code{x}. Eventually, we might
      store the query and subject inside \code{x}, in which case the
      arguments would be redundant.
    }
  }
}

\note{This class is highly experimental. It has not been well tested and
  may disappear at any time.}

\author{ Michael Lawrence }
\seealso{
\code{\link{findOverlaps}}, which generates an instance of this class.
}
\keyword{methods}
\keyword{classes}
