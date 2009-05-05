\name{ListLike-class}
\docType{class}

\alias{class:ListLike}
\alias{ListLike-class}
\alias{ListLike}

\alias{[[,ListLike-method}
\alias{$,ListLike-method}
\alias{lapply,ListLike-method}
\alias{sapply}
\alias{sapply,ListLike-method}
\alias{as.list,ListLike-method}
\alias{isEmpty}
\alias{isEmpty,ANY-method}


\title{ListLike objects}

\description{
  The ListLike class is just an interface i.e. a virtual class with no slots.
  ListLike subclasses (i.e. classes that contain the ListLike class)
  must define the following minimal set of functions/operators:
  \code{length}, \code{"[["} and \code{names}.
  Then, instances of these subclasses can be considered to have the shape
  of a list, and the methods described below apply to them.

  Some direct subclasses of the ListLike class are: \link{Views},
  \link{MaskCollection}, \link[Biostrings]{XStringSet} (defined in the
  Biostrings package), etc...
}

\section{Methods}{
  In the code snippets below, \code{x} and \code{X} are ListLike objects.

  \describe{
    \item{}{
      \code{x$name}:
      Similar to \code{x[[name]]}, but \code{name} is taken
      literally as an element name.
    }
    \item{}{
      \code{lapply(X, FUN, ...)}:
      Like the standard \code{\link[base]{lapply}} function defined in the
      base package, the \code{lapply} method for ListLike objects returns
      a list of the same length as \code{X}, each element of
      which is the result of applying \code{FUN} to the corresponding element
      of \code{X}.
    }
    \item{}{
      \code{sapply(X, FUN, ..., simplify=TRUE, USE.NAMES=TRUE)}:
      Like the standard \code{\link[base]{sapply}} function defined in the
      base package, the \code{sapply} method for ListLike objects
      is a user-friendly version of \code{lapply} by default
      returning a vector or matrix if appropriate.
    }
    \item{}{
      \code{as.list(x, ...)}:
      Turns \code{x} into a standard list.
    }
    \item{}{
      \code{isEmpty(x)}:
      Here \code{x} can be an atomic, list or ListLike object, or any
      object for which an \code{isEmpty} method is defined.
      If \code{x} is atomic, returns \code{length(x) == 0L}.
      If \code{x} is a list or ListLike object, then it works
      elementwise and is defined recursively by
      \code{sapply(x, function(xx) all(isEmpty(xx)))}.
    }
  }
}

\author{H. Pages}

\seealso{
  \code{\link[base]{lapply}},
  \code{\link[base]{sapply}},
  \code{\link[base]{as.list}}.

  Some direct subclasses of the ListLike class:
  \link{TypedListV2-class},
  \link{Views-class},
  \link{MaskCollection-class},
  \link[Biostrings]{XStringSet-class}.
}

\examples{
  showClass("ListLike")  # shows (some of) the known subclasses
}

\keyword{methods}
\keyword{classes}
