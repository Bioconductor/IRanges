\name{TypedList-class}
\docType{class}
\alias{TypedList-class}

% accessors
\alias{elementClass}
\alias{elementClass,TypedList-method}
\alias{elementLengths}
\alias{elementLengths,TypedList-method}
\alias{length,TypedList-method}
\alias{names,TypedList-method}
\alias{names<-,TypedList-method}
\alias{isEmpty,TypedList-method}

% subsetting
\alias{[,TypedList-method}
\alias{[<-,TypedList-method} % hidden
\alias{[[,TypedList-method}
\alias{[[<-,TypedList-method}
\alias{$,TypedList-method}
\alias{$<-,TypedList-method}

% splitting and combining
\alias{append,TypedList,TypedList-method}
\alias{c,TypedList-method}

% coercion
\alias{as.list,TypedList-method}
\alias{coerce,TypedList,list-method}
\alias{unlist,TypedList-method}

% utilities
\alias{lapply,TypedList,function-method}
\alias{lapply,TypedList,character-method}

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
    \item{}{
      \code{x$name}: similar to above, where \code{name} is taken
      literally as an element name.
    }
    \item{}{\code{x[[i]] <- value}: Replace the element at index
      \code{i} (a scalar number or string) with \code{value}. The behavior is
      very similar to that of an ordinary \code{list}, except
      \code{value} must be coercible (and is coerced) to the required
      element class.
    }
    \item{}{
      \code{x$name <- value}: similar to above, where \code{name} is taken
      literally as an element name.
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
    \item{}{\code{elementLengths(x)}: Get the 'length' of each of the
      elements.
    }
    \item{}{\code{isEmpty(x)}: Gets a logical vector indicating
      which elements are empty (length zero).
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
      \code{TypedList} objects in \code{...} onto the end of
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
  ## demonstrated on IntegerList objects, as TypedList is virtual

  int1 <- c(1L,2L,3L,5L,2L,8L)
  int2 <- c(15L,45L,20L,1L,15L,100L,80L,5L)
  collection <- IntegerList(int1, int2)

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
  collection$one

  ## replacement
  collection$one <- int2
  collection[[2]] <- int1

  ## combining
  col1 <- IntegerList(one = int1, int2)
  col2 <- IntegerList(two = int2, one = int1)
  col3 <- IntegerList(int2)
  append(col1, col2)
  append(col1, col2, 0)
  c(col1, col2, col3)

  ## get the mean for each element
  lapply(col1, mean)
}
\keyword{methods}
\keyword{classes}
