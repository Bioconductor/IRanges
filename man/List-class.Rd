\name{List-class}
\docType{class}

% List class, functions and methods:
\alias{class:List}
\alias{List-class}
\alias{List}

\alias{elementType}
\alias{elementType,List-method}
\alias{elementType,vector-method}
\alias{elementLengths}
\alias{elementLengths,list-method}
\alias{elementLengths,List-method}
\alias{elementLengths,CompressedList-method}
\alias{isEmpty}
\alias{isEmpty,ANY-method}

\alias{[[,List-method}
\alias{$,List-method}

\alias{lapply,List-method}
\alias{sapply,List-method}
\alias{mapply}
\alias{mapply,List-method}
\alias{endoapply,List-method}
\alias{mendoapply,List-method}

\alias{coerce,List,list-method}
\alias{as.list,List-method}
\alias{as.env}
\alias{as.env,List-method}
\alias{stack,List-method}

\alias{Reduce,ANY,List-method}
\alias{Filter,ANY,List-method}
\alias{Find,ANY,List-method}
\alias{Map}
\alias{Map,List-method}
\alias{Position,ANY,List-method}

\alias{eval}
\alias{eval,expressionORlanguage,List-method}
\alias{with,List-method}

\title{List objects}

\description{
  List objects are \link{Vector} objects with a \code{"[["},
  \code{elementType} and \code{elementLengths} method.
  The List class serves a similar role as \link[base]{list} in base R.

  It adds one slot, the \code{elementType} slot, to the two slots shared by
  all \link{Vector} objects.

  The \code{elementType} slot is the preferred location for List
  subclasses to store the type of data represented in the sequence. It is
  designed to take a character of length 1 representing the class of the
  sequence elements. While the List class performs no validity checking
  based on \code{elementType}, if a subclass expects elements to be of a
  given type, that subclass is expected to perform the necessary validity
  checking. For example, the subclass \link{IntegerList} has
  \code{elementType = "integer"} and its validity method checks if this
  condition is TRUE.

  To be functional, a class that inherits from List must define at least
  a \code{"[["} method (in addition to the minimum set of \link{Vector}
  methods).
}

\section{Accessors}{
  In the following code snippets, \code{x} is a List object.
  
  \describe{
    \item{}{
      \code{elementType(x)}:
      Get the scalar string naming the class from which all elements must
      derive.
    }
    \item{}{
      \code{elementLengths(x)}:
      Get the 'length' of each of the elements.
    }
    \item{}{
      \code{isEmpty(x)}:
      Returns a logical indicating either if the sequence has no elements
      or if all its elements are empty.
    }
  }
}

\section{Element extraction (list style)}{
  In the code snippets below, \code{x} is a List object.

  \describe{
    \item{}{
      \code{x[[i]]}:
      If defined, return the selected element \code{i}, where \code{i} is an
      numeric or character vector of length 1.
    }
    \item{}{
      \code{x$name}:
      Similar to \code{x[[name]]}, but \code{name} is taken literally as an
      element name.
    }
  }
}

\section{Looping}{
  In the code snippets below, \code{x} is a List object.

  \describe{
    \item{}{
      \code{lapply(X, FUN, ...)}:
      Like the standard \code{\link[base]{lapply}} function defined in the
      base package, the \code{lapply} method for List objects returns
      a list of the same length as \code{X}, with each element being the
      result of applying \code{FUN} to the corresponding element of \code{X}.
    }
    \item{}{
      \code{sapply(X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE)}:
      Like the standard \code{\link[base:lapply]{sapply}} function defined in the
      base package, the \code{sapply} method for List objects is a
      user-friendly version of \code{lapply} by default returning a vector
      or matrix if appropriate.
    }
    \item{}{
      \code{mapply(FUN, ..., MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE)}:
      Like the standard \code{\link[base]{mapply}} function defined in the
      base package, the \code{mapply} method for List objects is a
      multivariate version of \code{sapply}.
    }
    \item{}{
      \code{endoapply(X, FUN, ...)}:
      Similar to \code{\link[base]{lapply}}, but performs an endomorphism,
      i.e. returns an object of \code{class(X)}.
    }
    \item{}{
      \code{mendoapply(FUN, ..., MoreArgs = NULL)}:
      Similar to \code{\link[base]{mapply}}, but performs an endomorphism
      across multiple objects, i.e. returns an object of
      \code{class(list(...)[[1]])}.
    }
  }
}

\section{Coercion}{
  In the code snippets below, \code{x} is a List object.

  \describe{
    \item{}{\code{as.env(x, enclos = parent.frame())}:
      Creates an environment from \code{x} with a symbol for each
      \code{names(x)}. The values are not actually copied into the
      environment. Rather, they are dynamically bound using
      \code{\link{makeActiveBinding}}. This prevents unnecessary copying
      of the data from the external vectors into R vectors. The values
      are cached, so that the data is not copied every time the symbol
      is accessed.
    }
    \item{}{
      \code{as.list(x, ...)}, \code{as(from, "list")}:
      Turns \code{x} into a standard list.
    }
    \item{}{\code{stack(x, indName = "space", valuesName = "values")}:
      As with \code{\link[utils:stack]{stack}} on a \code{list},
      constructs a \code{DataFrame} with two columns: one for the
      unlisted values, the other indicating the name of the element from
      which each value was obtained. \code{indName} specifies the column
      name for the index (source name) column and \code{valuesName}
      specifies the column name for the values.
    }
  }
}

\section{Functional Programming}{
  The R base package defines some Higher-Order functions that are commonly
  found in Functional Programming Languages. See \code{?\link[base]{Reduce}}
  for the details, and, in particular, for a description of their arguments.
  The IRanges package provides methods for List objects, so, in addition
  to be a vector, the \code{x} argument can also be a List object.

  \describe{
    \item{}{
      \code{Reduce(f, x, init, right = FALSE, accumulate = FALSE)}:
      Uses a binary function to successively combine the elements of \code{x}
      and a possibly given initial value.
      See \code{?\link[base]{Reduce}} (in the base package) for the details.
    }
    \item{}{
      \code{Filter(f, x)}:
      Extracts the elements of \code{x} for which function \code{f} is TRUE.
      See \code{?\link[base]{Filter}} (in the base package) for the details.
    }
    \item{}{
      \code{Find(f, x, right = FALSE, nomatch = NULL)}:
      Extracts the first or last such element in \code{x}.
      See \code{?\link[base]{Find}} (in the base package) for the details.
    }
    \item{}{
      \code{Map(f, ...)}:
      Applies a function to the corresponding elements of given List
      objects.
      See \code{?\link[base]{Map}} (in the base package) for the details.
    }
    \item{}{
      \code{Position(f, x, right = FALSE, nomatch = NA_integer_)}:
      Extracts the first or last such position in \code{x}.
      See \code{?\link[base]{Position}} (in the base package) for the details.
    }
  }
}

\section{Evaluating}{
  In the code snippets below, \code{envir} and \code{data} are List
  objects.

  \describe{
    \item{}{\code{eval(expr, envir, enclos = parent.frame())}:
      Converts the List object specified in \code{envir} to an
      environment using \code{as.env}, with \code{enclos} as its parent,
      and then evaluates \code{expr} within that environment.
    }
    \item{}{\code{with(data, expr, \dots)}:
      Equivalent to \code{eval(quote(expr), data, ...)}.
    }
  }
}

\author{P. Aboyoun and H. Pages}

\seealso{
  \linkS4class{Vector} for the parent class.

  \linkS4class{SimpleList} and \linkS4class{CompressedList} for direct
  extensions.

  \linkS4class{IRanges} and \linkS4class{CompressedLogicalList} for example
  implementations.
}

\examples{
  showClass("List")  # shows (some of) the known subclasses
}

\keyword{methods}
\keyword{classes}
