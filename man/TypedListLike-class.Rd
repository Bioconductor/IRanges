\name{TypedListLike-class}
\docType{class}
\alias{TypedListLike}
\alias{TypedListLike-class}
\alias{CompressedTypedListLike}
\alias{CompressedTypedListLike-class}
\alias{SimpleTypedListLike}
\alias{SimpleTypedListLike-class}

\alias{AnnotatedTypedListLike}
\alias{AnnotatedTypedListLike-class}
\alias{AnnotatedCompressedTypedListLike}
\alias{AnnotatedCompressedTypedListLike-class}
\alias{AnnotatedSimpleTypedListLike}
\alias{AnnotatedSimpleTypedListLike-class}

% accessors
\alias{elementType}
\alias{elementType,TypedListLike-method}
\alias{elementMetadata}
\alias{elementMetadata,AnnotatedTypedListLike-method}
\alias{elementMetadata<-}
\alias{elementMetadata<-,AnnotatedTypedListLike,DataFrameORNULL-method}
\alias{elementLengths}
\alias{elementLengths,CompressedTypedListLike-method}
\alias{elementLengths,SimpleTypedListLike-method}
\alias{length,CompressedTypedListLike-method}
\alias{length,SimpleTypedListLike-method}
\alias{names,CompressedTypedListLike-method}
\alias{names,SimpleTypedListLike-method}
\alias{names<-,CompressedTypedListLike-method}
\alias{names<-,SimpleTypedListLike-method}

% subsetting
\alias{[[,TypedListLike-method}
\alias{$,TypedListLike-method}
\alias{[[<-,CompressedTypedListLike-method}
\alias{[[<-,SimpleTypedListLike-method}
\alias{$<-,TypedListLike-method}
\alias{[,CompressedTypedListLike-method}
\alias{[,SimpleTypedListLike-method}
\alias{[,AnnotatedCompressedTypedListLike-method}
\alias{[,AnnotatedSimpleTypedListLike-method}
\alias{[<-,TypedListLike-method} % hidden

% splitting and combining
\alias{append,CompressedTypedListLike,CompressedTypedListLike-method}
\alias{append,SimpleTypedListLike,SimpleTypedListLike-method}
\alias{append,AnnotatedCompressedTypedListLike,AnnotatedCompressedTypedListLike-method}
\alias{append,AnnotatedSimpleTypedListLike,AnnotatedSimpleTypedListLike-method}
\alias{c,CompressedTypedListLike-method}
\alias{c,SimpleTypedListLike-method}
\alias{c,AnnotatedCompressedTypedListLike-method}
\alias{c,AnnotatedSimpleTypedListLike-method}

% looping
\alias{lapply,CompressedTypedListLike-method}
\alias{lapply,SimpleTypedListLike-method}
\alias{sapply,TypedListLike-method}

% coercion
\alias{coerce,TypedListLike,list-method}
\alias{as.list,CompressedTypedListLike-method}
\alias{as.list,SimpleTypedListLike-method}
\alias{unlist,TypedListLike-method}

% show
\alias{show,TypedListLike-method}

\title{Typed List-Like and Annotated Typed List-Like Classes}
\description{The virtual class \code{TypedListLike} is an extension of the
  ListLike virtual class with all of the elements deriving from a particular
  type. This is useful for validity checking and for implementing vectorized
  type-specific operations. The virtual class \code{AnnotatedTypeListLike}
  extends \code{TypedListLike} by including annotation information.}

\details{
  The required element type is indicated by the \code{elementType} slot, a
  scalar string naming the class from which all elements must derive. This slot
  should never be set after initialization.

  \code{TypedListLike} is a virtual class, so a subclass must be derived for a
  particular element type. This turns out to be useful in almost all cases, as
  the explicit class can be used as the type of a slot in a class that requires
  a homogeneous list of elements. Also, methods may be implemented for the
  subclass that, for example, perform a vectorized operation specific to the
  element type. Using this approach, the convention is for the prototype of the
  subclass to set the \code{elementType} slot and to leave it unchanged.

  Two virtual subclasses of \code{TypedListLike}, \code{SimpleTypedListLike} and
  \code{CompressedTypedListLike}, are used to represent different methods for
  storing the elements of the typed list. Classes derived from
  \code{SimpleTypedListLike} represent their data using an ordinary \code{list}
  object while classes derived from \code{CompressedTypedListLike} represent
  their data in a compressed ``unlisted" form whose partitioning is described in
  a \code{PartitioningByEnd} object. The latter form is useful when many of the
  elements store zero length objects (e.g. a list containing mostly length 0
  integer vectors) or to avoid the overhead of storing multiple S4 objects (e.g.
  a list containing thousands of IRanges objects).

  The \code{AnnotatedTypedListLike} class extends \code{TypedListLike} by
  supporting  the storage of global and element-wise metadata. In particular,
  the \code{metadata} slot contains a list of metadata pertaining to the whole
  object and the \code{elementMetadata} slot contains an
  \code{\linkS4class{DataFrame}} (or \code{NULL}) for element-wise metadata
  with a row for each element and a column for each metadata variable. As with
  the \code{TypedListLike} virtual class, two virtual subclasses of
  \code{AnnotatedTypedListLike}, \code{AnnotatedSimpleTypedListLike} and
  \code{AnnotatedCompressedTypedListLike}, are used to represent different
  methods for storing the elements of the annotated typed list.
}

\section{Subsetting}{
  In the following code snippets, \code{x} is a \code{TypedListLike} object.

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
  In the following code snippets, \code{x} is a \code{TypedListLike} object,
  unless otherwise noted.
  
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
    \item{}{\code{metadata(x)}, \code{metadata(x) <- value}:
      FOR \code{AnnotatedTypedListLike} OBJECTS ONLY.
      Get or set the list holding arbitrary R objects as annotations. May be,
      and often is, empty.
    }
    \item{}{\code{elementMetadata(x), elementMetadata(x) <- value}:
      FOR \code{AnnotatedTypedListLike} OBJECTS ONLY.
      Get or set the \code{\linkS4class{DataFrame}} holding local metadata on
      each element. The rows are named according to the names of the elements.
      Optional, may be \code{NULL}.
    }
  }
}

\section{Splitting and Combining}{
  The following are methods for combining \code{TypedListLike} elements. In the
  signatures, \code{x} is a \code{TypedListLike} object. 

  \describe{
    \item{}{\code{append(x, values, after = length(x))}: Insert the
      \code{TypedListLike} \code{values} onto \code{x} at the position given by
      \code{after}. \code{values} must have an \code{elementType} that extends
      that of \code{x}.
    }
    \item{}{\code{c(x, ..., recursive = FALSE)}: Appends the
      \code{TypedListLike} objects in \code{...} onto the end of \code{x}. All
      arguments must have an element class that extends that of \code{x}.
    }
  }

  Note that the default \code{\link{split}} method happens to work on
  \code{TypedListLike} objects. 
}

\section{Coercion}{
  In the following code snippets, \code{x} is a \code{TypedListLike} object.
  \describe{
    \item{}{\code{as.list(x)}, \code{as(from, "list")}: Coerces a
      \code{TypedListLike} to an ordinary \code{list}. Note that this is
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
      base package, the \code{lapply} method for TypedListLike objects returns
      a list of the same length as \code{X}, each element of which is the
      result of applying \code{FUN} to the corresponding element of \code{X}.
    }
    \item{}{
      \code{sapply(X, FUN, ..., simplify=TRUE, USE.NAMES=TRUE)}:
      Like the standard \code{\link[base]{sapply}} function defined in the
      base package, the \code{sapply} method for TypedListLike objects is a
      user-friendly version of \code{lapply} by default returning a vector
      or matrix if appropriate.
    }
  }
}

\author{Michael Lawrence, Patrick Aboyoun}
\seealso{\code{\linkS4class{ListLike}},
         \code{\linkS4class{Annotated}},
         \code{\linkS4class{IntegerList}} and
         \code{\linkS4class{RangesList}} for example implementations}

\examples{
  ## demonstrated on IntegerList objects, as TypedListLike is virtual

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
