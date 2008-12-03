\name{TypedList-class}
\docType{class}
\alias{TypedList-class}

% accessors
\alias{elementClass}
\alias{elementClass,TypedList-method}
\alias{elements}
\alias{elements,TypedList-method}
\alias{length,TypedList-method}
\alias{names,TypedList-method}
\alias{names<-,TypedList-method}

% subsetting
\alias{[,TypedList,ANY,ANY,ANY-method}
\alias{[<-,TypedList,ANY,ANY,ANY-method} % hidden
\alias{[[,TypedList-method}
\alias{[[<-,TypedList-method}

% splitting and combining
\alias{append,TypedList,TypedList-method}
\alias{c,TypedList-method}

% coercion
\alias{as.list,TypedList-method}
\alias{coerce,TypedList,list-method}
\alias{unlist,TypedList-method}

% utilities
\alias{lapply,TypedList,function-method}

% show
\alias{show,TypedList-method}

\title{Typed Lists}
\description{The virtual class \code{TypedList} is an emulation of an ordinary
  \code{list}, except all of the elements must derive from a particular
  type. This is useful for validity checking and for implementing
  vectorized type-specific operations.}

\details{
  In general, a \code{TypedList} may be treated as any ordinary
  \code{list}, except with regard to the element type restriction.
  
  The required element type is indicated by the \code{elementClass}
  slot, a scalar string naming the class from which all elements must
  derive. This slot should never be set after initialization.

  \code{TypedList} is a virtual class, so a subclass must be derived for
  a particular element type. This turns out to be useful in almost all
  cases, as the explicit class can be used as the type of a slot in a
  class that requires a homogeneous list of elements. Also, methods may
  be implemented for the subclass that, for example, perform a vectorized
  operation specific to the element type. Using this approach, the
  convention is for the prototype of the subclass to set the
  \code{elementClass} slot and to leave it unchanged.
}

\section{Subsetting}{
  In the following code snippets, \code{x} is a \code{TypedList} object.

  \describe{
    \item{}{\code{x[i]}: Get a subset of \code{x} containing the
      elements indexed by
      \code{i}, which may be numeric, character, logical, \code{NULL} or
      missing. The behavior is very similar to an ordinary \code{list},
      except operations that would insert \code{NULL} elements are only
      allowed if \code{NULL} is a valid element type.}
    \item{}{\code{x[[i]]}: Get the element in \code{x} indexed by
      \code{i}, which may be a scalar number or string. The behavior is
      nearly identical to that of an ordinary \code{list}.
    }
    \item{}{\code{x[[i]] <- value}: Replace the element at index
      \code{i} (a scalar number or string) with \code{value}. The behavior is
      very similar to that of an ordinary \code{list}, except
      \code{value} must be coercible (and is coerced) to the required
      element class.
    } 
  }
}

\section{Accessors}{
  In the following code snippets, \code{x} is a \code{TypedList} object.
  
  \describe{
    \item{}{\code{length(x)}: Get the number of elements in \code{x}}
    \item{}{\code{names(x)}, \code{names(x) <- value}: Get or set the
      names of the elements in the list. This behaves exactly the same as
      an ordinary \code{list}.
    }
    \item{}{\code{elementClass(x)}: Get the scalar string naming the
      class from which all elements must derive.
    }
    \item{}{\code{elements(x)}: Returns the internal \code{list} holding
      the elements. It is not recommended to access the elements this
      way, as for some subclasses of \code{TypedList} this may be an
      internal representation that is not consistent with what is
      extracted with, for example, the \code{[[} method.
    }
  }
}

\section{Splitting and Combining}{
  The following are methods for
  combining \code{TypedList} elements. In the signatures, \code{x} is a
  \code{TypedList} object. 

  \describe{
    \item{}{\code{append(x, values, after = length(x))}: Insert the
      \code{TypedList} \code{values} onto \code{x} at the position given
      by \code{after}. \code{values} must have an \code{elementClass}
      that extends that of \code{x}.
    }
    \item{}{\code{c(x, ..., recursive = FALSE)}: Appends the
      \code{TypedList} instances in \code{...} onto the end of
      \code{x}. All arguments must have an element class that extends that
      of \code{x}.
    }
  }

  Note that the default \code{\link{split}} method happens
  to work on \code{TypedList} objects. 
}

\section{Coercion}{
  In the following code snippets, \code{x} is a \code{TypedList} object.
  \describe{
    \item{}{\code{as.list(x)}, \code{as(from, "list")}: Coerces a
      \code{TypedList} to an ordinary \code{list}. Note that this is
      preferred over the \code{elements} accessor for getting a \code{list}
      of the elements.
    }
    \item{}{\code{unlist(x)}: Combines all of the elements in this list
      into a single element via the \code{c} function and returns the
      result. Will not work if the elements have no method for
      \code{c}. Returns \code{NULL} if there are no elements in \code{x},
      which may not be what is expected in many cases.
      Subclasses should implement their own logic.
    }
  }
}

\section{Applying}{
  \describe{
    \item{}{\code{lapply(X, FUN, ...)}: Applies the \code{function}
      \code{FUN} over the \code{TypedList} \code{X}, with arguments in
      \code{...} passed on to \code{FUN}. Returns a \code{list}, with
      each element resulting from invoking \code{FUN} on the corresponding
      element of \code{X}. Same semantics as the default \code{\link{lapply}}.
    }
  }
}

\author{Michael Lawrence}
\seealso{\code{\linkS4class{RangesList}} for an example implementation}

\examples{
  ## demonstrated on RangesList, as TypedList is virtual

  range1 <- IRanges(start=c(1,2,3), end=c(5,2,8))
  range2 <- IRanges(start=c(15,45,20,1), end=c(15,100,80,5))
  collection <- RangesList(range1, range2)

  ## names
  names(collection) <- c("one", "two")
  names(collection)
  names(collection) <- NULL # clear names
  names(collection)
  names(collection) <- "one"
  names(collection) # c("one", NA)

  ## extraction
  collection[[1]] # range1
  collection[["1"]] # NULL, does not exist
  collection[["one"]] # range1
  collection[[NA_integer_]] # NULL

  ## subsetting
  collection[numeric()] # empty
  collection[NULL] # empty
  collection[] # identity
  collection[c(TRUE, FALSE)] # first element
  collection[2] # second element
  collection[c(2,1)] # reversed
  collection[-1] # drop first

  ## combining
  col1 <- RangesList(one = range1, range2)
  col2 <- RangesList(two = range2, one = range1)
  col3 <- RangesList(range2)
  append(col1, col2, 1)
  append(col1, col2, -5)
  c(col1, col2, col3)

  ## get the starts of each Ranges
  lapply(col1, start)
}
\keyword{methods}
\keyword{classes}
