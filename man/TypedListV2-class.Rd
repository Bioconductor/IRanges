\name{TypedListV2-class}
\docType{class}
\alias{TypedListV2}
\alias{TypedListV2-class}
\alias{CompressedTypedList}
\alias{CompressedTypedList-class}
\alias{SimpleTypedList}
\alias{SimpleTypedList-class}

% accessors
\alias{elementType}
\alias{elementType,TypedListV2-method}
\alias{metadata}
\alias{metadata,TypedListV2-method}
\alias{elementMetadata}
\alias{elementMetadata,TypedListV2-method}
\alias{metadata<-}
\alias{metadata<-,TypedListV2,list-method}
\alias{elementMetadata<-}
\alias{elementMetadata<-,TypedListV2,XDataFrameORNULL-method}
\alias{elementLengths}
\alias{elementLengths,CompressedTypedList-method}
\alias{elementLengths,SimpleTypedList-method}
\alias{length,CompressedTypedList-method}
\alias{length,SimpleTypedList-method}
\alias{names,CompressedTypedList-method}
\alias{names,SimpleTypedList-method}
\alias{names<-,CompressedTypedList-method}
\alias{names<-,SimpleTypedList-method}

% subsetting
\alias{[[,TypedListV2-method}
\alias{$,TypedListV2-method}
\alias{[[<-,CompressedTypedList-method}
\alias{[[<-,SimpleTypedList-method}
\alias{$<-,TypedListV2-method}
\alias{[,CompressedTypedList-method}
\alias{[,SimpleTypedList-method}
\alias{[<-,TypedListV2-method} % hidden

% splitting and combining
\alias{append,CompressedTypedList,CompressedTypedList-method}
\alias{append,SimpleTypedList,SimpleTypedList-method}
\alias{c,CompressedTypedList-method}
\alias{c,SimpleTypedList-method}

% looping
\alias{lapply,CompressedTypedList-method}
\alias{lapply,SimpleTypedList-method}
\alias{sapply,TypedListV2-method}

% coercion
\alias{coerce,TypedListV2,list-method}
\alias{as.list,CompressedTypedList-method}
\alias{as.list,SimpleTypedList-method}
\alias{unlist,TypedListV2-method}

% show
\alias{show,TypedListV2-method}

\title{Typed Lists (Version 2)}
\description{The virtual class \code{TypedListV2} is an emulation of an ordinary
  \code{list}, except all of the elements must derive from a particular type.
  This is useful for validity checking and for implementing vectorized
  type-specific operations.}

\details{
  In general, a \code{TypedListV2} may be treated as any ordinary \code{list},
  except with regard to the element type restriction.
  
  The required element type is indicated by the \code{elementType} slot, a
  scalar string naming the class from which all elements must derive. This slot
  should never be set after initialization.

  \code{TypedListV2} is a virtual class, so a subclass must be derived for a
  particular element type. This turns out to be useful in almost all cases, as
  the explicit class can be used as the type of a slot in a class that requires
  a homogeneous list of elements. Also, methods may be implemented for the
  subclass that, for example, perform a vectorized operation specific to the
  element type. Using this approach, the convention is for the prototype of the
  subclass to set the \code{elementType} slot and to leave it unchanged.

  There are two virtual subclasses of \code{TypedListV2} (\code{SimpleTypedList}
  and \code{CompressedTypedList}) that use different methods for storing the
  elements of the typed list. Classes derived from \code{SimpleTypedList}
  represent their data using an ordinary \code{list} object while classes
  derived from \code{CompressedTypedList} represent their data in a compressed
  ``unlisted" form whose partitioning is described in a \code{PartitioningByEnd}
  object. The latter form is useful when many of the elements store zero length
  objects (e.g. a list containing mostly length 0 integer vectors) or to avoid
  the overhead of storing multiple S4 objects (e.g. a list containing thousands
  of IRanges objects).

  In addition to storing data, the \code{TypedListV2} class supports the storage
  of global and element-wise metadata. In particular, the \code{metadata} slot
  contains a list of metadata pertaining to the whole object and the
  \code{elementMetadata} slot contains an \code{\linkS4class{XDataFrame}} (or
  \code{NULL}) for element-wise metadata with a row for each element and a
  column for each metadata variable.
}

\section{Subsetting}{
  In the following code snippets, \code{x} is a \code{TypedListV2} object.

  \describe{
    \item{}{\code{x[i]}: Get a subset of \code{x} containing the elements
      indexed by \code{i}, which may be numeric, character, logical, \code{NULL}
      or missing. The behavior is very similar to an ordinary \code{list},
      except operations that would insert \code{NULL} elements are only allowed
      if \code{NULL} is a valid element type.}
    \item{}{\code{x[[i]]}: Get the element in \code{x} indexed by \code{i},
      which may be a scalar number or string. The behavior is nearly identical
      to that of an ordinary \code{list}.
    }
    \item{}{
      \code{x$name}: similar to above, where \code{name} is taken literally as
      an element name.
    }
    \item{}{\code{x[[i]] <- value}: Replace the element at index \code{i} (a
      scalar number or string) with \code{value}. The behavior is very similar
      to that of an ordinary \code{list}, except \code{value} must be coercible
      (and is coerced) to the required element class.
    }
    \item{}{
      \code{x$name <- value}: similar to above, where \code{name} is taken
      literally as an element name.
    }
  }
}

\section{Accessors}{
  In the following code snippets, \code{x} is a \code{TypedListV2} object.
  
  \describe{
    \item{}{\code{length(x)}: Get the number of elements in \code{x}}
    \item{}{\code{names(x)}, \code{names(x) <- value}: Get or set the names of
      the elements in the list. This behaves exactly the same as an ordinary
      \code{list}.
    }
    \item{}{\code{elementType(x)}: Get the scalar string naming the class from
      which all elements must derive.
    }
    \item{}{\code{elementLengths(x)}: Get the 'length' of each of the elements.
    }
    \item{}{\code{isEmpty(x)}: Gets a logical vector indicating which elements
      are empty.
    }
    \item{}{\code{metadata(x)}, \code{metadata(x) <- value}: Get or set the list
      holding arbitrary R objects as annotations. May be, and often is, empty.
    }
    \item{}{\code{elementMetadata(x), elementMetadata(x) <- value}:
      Get or set the \code{\linkS4class{XDataFrame}} holding local metadata on
      each element. The rows are named according to the names of the elements.
      Optional, may be \code{NULL}.
    }
  }
}

\section{Splitting and Combining}{
  The following are methods for combining \code{TypedListV2} elements. In the
  signatures, \code{x} is a \code{TypedListV2} object. 

  \describe{
    \item{}{\code{append(x, values, after = length(x))}: Insert the
      \code{TypedListV2} \code{values} onto \code{x} at the position given by
      \code{after}. \code{values} must have an \code{elementType} that extends
      that of \code{x}.
    }
    \item{}{\code{c(x, ..., recursive = FALSE)}: Appends the \code{TypedListV2}
      objects in \code{...} onto the end of \code{x}. All arguments must have an
      element class that extends that of \code{x}.
    }
  }

  Note that the default \code{\link{split}} method happens to work on
  \code{TypedListV2} objects. 
}

\section{Coercion}{
  In the following code snippets, \code{x} is a \code{TypedListV2} object.
  \describe{
    \item{}{\code{as.list(x)}, \code{as(from, "list")}: Coerces a
      \code{TypedListV2} to an ordinary \code{list}. Note that this is
      preferred over the \code{elements} accessor for getting a \code{list} of
      the elements.
    }
    \item{}{\code{unlist(x)}: Combines all of the elements in this list into a
      single element via the \code{c} function and returns the result. Will not
      work if the elements have no method for \code{c}. Returns \code{NULL} if
      there are no elements in \code{x}, which may not be what is expected in
      many cases. Subclasses should implement their own logic.
    }
  }
}

\section{Looping}{
  \describe{
    \item{}{
      \code{lapply(X, FUN, ...)}:
      Like the standard \code{\link[base]{lapply}} function defined in the
      base package, the \code{lapply} method for TypedListV2 objects returns
      a list of the same length as \code{X}, each element of which is the
      result of applying \code{FUN} to the corresponding element of \code{X}.
    }
    \item{}{
      \code{sapply(X, FUN, ..., simplify=TRUE, USE.NAMES=TRUE)}:
      Like the standard \code{\link[base]{sapply}} function defined in the
      base package, the \code{sapply} method for TypedListV2 objects is a
      user-friendly version of \code{lapply} by default returning a vector
      or matrix if appropriate.
    }
  }
}

\author{Michael Lawrence, Patrick Aboyoun}
\seealso{\code{\linkS4class{ListLike}},
         \code{\linkS4class{IntegerList}} and
         \code{\linkS4class{RangesList}} for example implementations}

\examples{
  ## demonstrated on IntegerList objects, as TypedListV2 is virtual

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
