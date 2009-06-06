\name{Sequence-class}
\docType{class}

% Sequence class, functions and methods:
\alias{class:Sequence}
\alias{Sequence-class}
\alias{Sequence}

\alias{[,Sequence-method}
\alias{[<-,Sequence-method}
\alias{[[,Sequence-method}
\alias{$,Sequence-method}
\alias{!=,Sequence,Sequence-method}
\alias{aggregate,Sequence-method}
\alias{aggregate,vector-method}
\alias{append,Sequence,Sequence-method}
\alias{as.list,Sequence-method}
\alias{c,Sequence-method}
\alias{coerce,Sequence,list-method}
\alias{elementType}
\alias{elementType,Sequence-method}
\alias{elementLengths}
\alias{elementLengths,list-method}
\alias{elementLengths,Sequence-method}
\alias{elementLengths,CompressedList-method}
\alias{elementMetadata}
\alias{elementMetadata,Sequence-method}
\alias{elementMetadata<-}
\alias{elementMetadata<-,Sequence,DataTableORNULL-method}
\alias{head,Sequence-method}
\alias{isEmpty}
\alias{isEmpty,ANY-method}
\alias{lapply,Sequence-method}
\alias{rep,Sequence-method}
\alias{rev,Sequence-method}
\alias{sapply}
\alias{sapply,Sequence-method}
\alias{seqextract}
\alias{seqextract,Sequence-method}
\alias{seqextract,vector-method}
\alias{shiftApply}
\alias{shiftApply,Sequence,Sequence-method}
\alias{shiftApply,vector,vector-method}
\alias{subset,Sequence-method}
\alias{tail,Sequence-method}
\alias{window,Sequence-method}
\alias{window,vector-method}


\title{Sequence objects}

\description{
  The Sequence virtual class serves as the heart of the IRanges package
  and has over 80 subclasses. It serves a similar role as \link[base]{vector}
  in base R. The Sequence class includes three slots: \code{elementType},
  \code{metadata} (via extension of the \linkS4class{Annotated} class), and
  \code{elementMetadata}. Their purpose is defined below.

  The \code{elementType} slot is the preferred location for Sequence
  subclasses to store the type of data represented in the sequence. It is
  designed to take a character of length 1 representing the class of the
  sequence elements. While the Sequence class performs no validity checking
  based on \code{elementType}, if a subclass expects elements to be of a
  given type, that subclass is expected to perform the necessary validity
  checking. For example, the subclass \link{IntegerList} has
  \code{elementType = "integer"} and its validity method checks if this
  condition is TRUE.

  The Sequence class supports the storage of global and element-wise metadata
  with its \code{metadata} and \code{elementMetadata} slots. The
  \code{metadata} slot can store a list of metadata pertaining to the whole
  object and the \code{elementMetadata} slot can store a
  \linkS4class{DataTable} (or \code{NULL}) for element-wise metadata with a
  row for each element and a column for each metadata variable.

  To be functional, a class that inherits from Sequence must define a
  \code{length} and \code{names} methods as well as one or both of the
  subscript methods \code{"[["} and \code{"["}.
}

\section{Accessors}{
  In the following code snippets, \code{x} is a Sequence object.
  
  \describe{
    \item{}{\code{length(x)}: Get the number of elements in \code{x}}
    \item{}{\code{names(x)}, \code{names(x) <- value}: Get or set the names of
      the elements in the Sequence.
    }
    \item{}{\code{elementType(x)}: Get the scalar string naming the class from
      which all elements must derive.
    }
    \item{}{\code{elementLengths(x)}: Get the 'length' of each of the elements.
    }
    \item{}{\code{isEmpty(x)}: Returns a logical indicating either if the
      sequence has no elements or if all its elements are empty.
    }
    \item{}{\code{metadata(x)}, \code{metadata(x) <- value}:
      Get or set the list holding arbitrary R objects as annotations. May be,
      and often is, empty.
    }
    \item{}{\code{elementMetadata(x), elementMetadata(x) <- value}:
      Get or set the \linkS4class{DataTable} holding local metadata on each
      element. The rows are named according to the names of the elements.
      Optional, may be \code{NULL}.
    }
  }
}

\section{Combining}{
  In the code snippets below, \code{x} is a Sequence object.

  \describe{
    \item{}{\code{append(x, values, after = length(x))}: Insert the
      \code{Sequence} \code{values} onto \code{x} at the position given by
      \code{after}. \code{values} must have an \code{elementType} that extends
      that of \code{x}.
    }
    \item{}{
      \code{c(x, ...)}:
      Combine \code{x} and the Sequence objects in \code{...} together.
      Any object in \code{...} must belong to the same class as \code{x},
      or to one of its subclasses, or must be \code{NULL}.
      The result is an object of the same class as \code{x}.
    }
  }
}

\section{Subsetting}{
  In the code snippets below, \code{x} is a Sequence object or regular R vector
  object. The R vector object methods for \code{window} and \code{seqextract}
  are defined in this package and the remaining methods are defined in base R.

  \describe{
    \item{}{
      \code{x[i, drop=TRUE]}:
      If defined, return a new Sequence object made of the selected elements
      (subscript \code{i} must be an NA-free numeric vector specifying the
      positions of the elements to select). The \code{drop} argument specifies
      whether or not to coerce the returned sequence to a standard vector.
    }
    \item{}{
      \code{x[[i]]}:
      If defined, return the selected element \code{i}, where \code{i} is an
      integer or character vector of length 1.
    }
    \item{}{
      \code{x$name}:
      Similar to \code{x[[name]]}, but \code{name} is taken literally as an
      element name.
    }
    \item{}{
      \code{head(x, n = 6L)}:
      If \code{n} is non-negative, returns the first n elements of the Sequence
      object.
      If \code{n} is negative, returns all but the last \code{abs(n)} elements
      of the Sequence object.
    }
    \item{}{
      \code{rep(x, times)}:
      Return a new Sequence object made of the repeated elements.
    }
    \item{}{
      \code{rev(x)}:
      Return a new Sequence object made of the original elements in the reverse
      order.
    }
    \item{}{
      \code{seqextract(x, start=NULL, end=NULL, width=NULL)}:
      Like \code{window}, except that multiple subsequences can be requested.
      The requested subsequences are concatenated. If the concatenation is
      undesirable, consider using \code{\link{Views}}.
    }
    \item{}{
      \code{subset(x, subset)}:
      Return a new Sequence object made of the subset using logical vector
      \code{subset}, where missing values are taken as FALSE.
    }
    \item{}{
      \code{tail(x, n = 6L)}:
      If \code{n} is non-negative, returns the last n elements of the Sequence
      object.
      If \code{n} is negative, returns all but the first \code{abs(n)} elements
      of the Sequence object.
    }
    \item{}{
      \code{window(x, start = NULL, end = NULL, width = NULL, frequency = NULL, delta = NULL, ...)}:
      Extract the subsequence window from the Sequence object using:
      \describe{
        \item{\code{start}, \code{end}, \code{width}}{The start, end, or width
          of the window. Two of the three are required.}
        \item{\code{frequency}, \code{delta}}{Optional arguments that specify
          the sampling frequency and increment within the window.}
      }
      In general, this is more efficient than using \code{"["} operator.
    }
  }
}

\section{Coersion}{
  In the code snippets below, \code{x} is a Sequence object.

  \describe{
    \item{}{
      \code{as.list(x, ...)}, \code{as(from, "list")}:
      Turns \code{x} into a standard list.
    }
  }
}

\section{Looping}{
  In the code snippets below, \code{x} is a Sequence object.

  \describe{
    \item{}{
      \code{aggregate(x, by, FUN, start = NULL, end = NULL, width = NULL,
                      frequency = NULL, delta = NULL, ..., simplify = TRUE))}:
      Generates summaries on the specified windows and returns the result in a
      convenient form:
      \describe{
        \item{\code{by}}{An object with \code{start}, \code{end}, and
          \code{width} methods.}
        \item{\code{FUN}}{The function, found via \code{match.fun}, to be
          applied to each window of \code{x}.}
        \item{\code{start}, \code{end}, \code{width}}{the start, end, or width
          of the window. If \code{by} is missing, then must supply two of the
          three.}
        \item{\code{frequency}, \code{delta}}{Optional arguments that specify
          the sampling frequency and increment within the window.}
        \item{\dots}{Further arguments for \code{FUN}.}
        \item{\code{simplify}}{A logical value specifying whether or not the
          result should be simplified to a vector or matrix if possible.}
      }
    }
    \item{}{
      \code{lapply(X, FUN, ...)}:
      Like the standard \code{\link[base]{lapply}} function defined in the
      base package, the \code{lapply} method for Sequence objects returns
      a list of the same length as \code{X}, with each element being the
      result of applying \code{FUN} to the corresponding element of \code{X}.
    }
    \item{}{
      \code{sapply(X, FUN, ..., simplify=TRUE, USE.NAMES=TRUE)}:
      Like the standard \code{\link[base]{sapply}} function defined in the
      base package, the \code{sapply} method for Sequence objects is a
      user-friendly version of \code{lapply} by default returning a vector
      or matrix if appropriate.
    }
    \item{}{
      \code{shiftApply(SHIFT, X, Y, FUN, ..., OFFSET = 0L, simplify = TRUE, verbose = FALSE)}:
      Let \code{i} be the indices in \code{SHIFT},
      \code{X_i = window(X, 1 + OFFSET, length(X) - SHIFT[i])}, and
      \code{Y_i = window(Y, 1 + SHIFT[i], length(Y) - OFFSET)}. Calculates
      the set of \code{FUN(X_i, Y_i, ...)} values and return the results in a
      convenient form:
      \describe{
        \item{\code{SHIFT}}{A non-negative integer vector of shift values.}
        \item{\code{X}, \code{Y}}{The Sequence or R vector objects to shift.}
        \item{\code{FUN}}{The function, found via \code{match.fun}, to be
          applied to each set of shifted vectors.}
        \item{\dots}{Further arguments for \code{FUN}.}
        \item{OFFSET}{A non-negative integer offset to maintain throughout
          the shift operations.}
        \item{\code{simplify}}{A logical value specifying whether or not the
          result should be simplified to a vector or matrix if possible.}
        \item{\code{verbose}}{A logical value specifying whether or not to
          print the \code{i} indices to track the iterations.}
      }
    }
  }
}

\author{P. Aboyoun}

\seealso{
  \linkS4class{Annotated},
  \linkS4class{DataTable},
  \linkS4class{SimpleList},
  \linkS4class{Ranges},
  \linkS4class{Rle},
  \linkS4class{XSequence} for example implementations
}

\examples{
  showClass("Sequence")  # shows (some of) the known subclasses
}

\keyword{methods}
\keyword{classes}
