\name{AtomicList}
\docType{class}

% AtomicList classes
\alias{class:AtomicList}
\alias{AtomicList-class}
\alias{AtomicList}

\alias{class:CompressedAtomicList}
\alias{CompressedAtomicList-class}
\alias{CompressedAtomicList}

\alias{class:SimpleAtomicList}
\alias{SimpleAtomicList-class}
\alias{SimpleAtomicList}


\alias{class:LogicalList}
\alias{LogicalList-class}
\alias{LogicalList}

\alias{class:CompressedLogicalList}
\alias{CompressedLogicalList-class}
\alias{CompressedLogicalList}

\alias{class:SimpleLogicalList}
\alias{SimpleLogicalList-class}
\alias{SimpleLogicalList}

\alias{class:IntegerList}
\alias{IntegerList-class}
\alias{IntegerList}

\alias{class:CompressedIntegerList}
\alias{CompressedIntegerList-class}
\alias{CompressedIntegerList}

\alias{class:SimpleIntegerList}
\alias{SimpleIntegerList-class}
\alias{SimpleIntegerList}

\alias{class:NumericList}
\alias{NumericList-class}
\alias{NumericList}

\alias{class:CompressedNumericList}
\alias{CompressedNumericList-class}
\alias{CompressedNumericList}

\alias{class:SimpleNumericList}
\alias{SimpleNumericList-class}
\alias{SimpleNumericList}

\alias{class:ComplexList}
\alias{ComplexList-class}
\alias{ComplexList}

\alias{class:CompressedComplexList}
\alias{CompressedComplexList-class}
\alias{CompressedComplexList}

\alias{class:SimpleComplexList}
\alias{SimpleComplexList-class}
\alias{SimpleComplexList}

\alias{class:CharacterList}
\alias{CharacterList-class}
\alias{CharacterList}

\alias{class:CompressedCharacterList}
\alias{CompressedCharacterList-class}
\alias{CompressedCharacterList}

\alias{class:SimpleCharacterList}
\alias{SimpleCharacterList-class}
\alias{SimpleCharacterList}

\alias{class:RawList}
\alias{RawList-class}
\alias{RawList}

\alias{class:CompressedRawList}
\alias{CompressedRawList-class}
\alias{CompressedRawList}

\alias{class:SimpleRawList}
\alias{SimpleRawList-class}
\alias{SimpleRawList}

\alias{class:RleList}
\alias{RleList-class}
\alias{RleList}

\alias{class:CompressedRleList}
\alias{CompressedRleList-class}
\alias{CompressedRleList}

\alias{class:SimpleRleList}
\alias{SimpleRleList-class}
\alias{SimpleRleList}

\alias{class:FactorList}
\alias{FactorList-class}
\alias{FactorList}

\alias{class:CompressedFactorList}
\alias{CompressedFactorList-class}
\alias{CompressedFactorList}

\alias{class:SimpleFactorList}
\alias{SimpleFactorList-class}
\alias{SimpleFactorList}

% coercion
\alias{as.list,CompressedAtomicList-method}
\alias{coerce,CompressedAtomicList,list-method}
\alias{as.vector,AtomicList-method}
\alias{coerce,vector,AtomicList-method}
\alias{as.matrix,AtomicList-method}
\alias{lapply,CompressedAtomicList-method}

\alias{coerce,vector,CompressedLogicalList-method}
\alias{coerce,vector,SimpleLogicalList-method}
\alias{coerce,vector,CompressedIntegerList-method}
\alias{coerce,vector,SimpleIntegerList-method}
\alias{coerce,vector,CompressedNumericList-method}
\alias{coerce,vector,SimpleNumericList-method}
\alias{coerce,vector,CompressedComplexList-method}
\alias{coerce,vector,SimpleComplexList-method}
\alias{coerce,vector,CompressedCharacterList-method}
\alias{coerce,vector,SimpleCharacterList-method}
\alias{coerce,vector,CompressedRawList-method}
\alias{coerce,vector,SimpleRawList-method}
\alias{coerce,vector,CompressedRleList-method}
\alias{coerce,vector,SimpleRleList-method}

\alias{coerce,AtomicList,LogicalList-method}
\alias{coerce,AtomicList,IntegerList-method}
\alias{coerce,AtomicList,NumericList-method}
\alias{coerce,AtomicList,ComplexList-method}
\alias{coerce,AtomicList,CharacterList-method}
\alias{coerce,AtomicList,RawList-method}
\alias{coerce,AtomicList,RleList-method}
\alias{RleList,AtomicList,RleList-method}
\alias{coerce,AtomicList,RleViews}

\alias{unlist,SimpleFactorList-method}
\alias{unlist,SimpleRleList-method}

\alias{unique,RleList-method}
\alias{unique,CompressedList-method}
\alias{table,AtomicList-method}
\alias{drop,AtomicList-method}
\alias{duplicated,CompressedList-method}
\alias{duplicated,CompressedAtomicList-method}

\alias{sort,List-method}
\alias{order,List-method}
\alias{rank,List-method}

\alias{runLength,RleList-method}
\alias{runValue,RleList-method}
\alias{runLength,CompressedRleList-method}
\alias{runValue,CompressedRleList-method}
\alias{runValue<-,CompressedRleList-method}
\alias{runValue<-,SimpleRleList-method}

\alias{ranges,RleList-method}
\alias{ranges,CompressedRleList-method}

\alias{show,AtomicList-method}
\alias{show,RleList-method}

\title{Lists of Atomic Vectors in Natural and Rle Form}
\description{An extension of \code{\linkS4class{List}} that holds
  only atomic vectors in either a natural or run-length encoded form.}

\details{
  The lists of atomic vectors are \code{LogicalList}, \code{IntegerList},
  \code{NumericList}, \code{ComplexList}, \code{CharacterList}, and
  \code{RawList}. There is also an \code{RleList} class for
  run-length encoded versions of these atomic vector types.

  Each of the above mentioned classes is virtual with Compressed* and Simple*
  non-virtual representations.
}

\section{Constructors}{
  \describe{
    \item{}{\code{LogicalList(..., compress = TRUE)}: Concatenates the
      \code{logical} vectors in \code{...} into a new \code{LogicalList}.
      If \code{compress}, the internal storage of the data is compressed.}
    \item{}{\code{IntegerList(..., compress = TRUE)}: Concatenates the
      \code{integer} vectors in \code{...} into a new \code{IntegerList}.
      If \code{compress}, the internal storage of the data is compressed.}
    \item{}{\code{NumericList(..., compress = TRUE)}: Concatenates the
      \code{numeric} vectors in \code{...} into a new \code{NumericList}.
      If \code{compress}, the internal storage of the data is compressed.}
    \item{}{\code{ComplexList(..., compress = TRUE)}: Concatenates the
      \code{complex} vectors in \code{...} into a new \code{ComplexList}.
      If \code{compress}, the internal storage of the data is compressed.}
    \item{}{\code{CharacterList(..., compress = TRUE)}: Concatenates the
      \code{character} vectors in \code{...} into a new \code{CharacterList}.
      If \code{compress}, the internal storage of the data is compressed.}
    \item{}{\code{RawList(..., compress = TRUE)}: Concatenates the
      \code{raw} vectors in \code{...} into a new \code{RawList}.
      If \code{compress}, the internal storage of the data is compressed.}
    \item{}{\code{RleList(..., compress = TRUE)}: Concatenates the
      run-length encoded atomic vectors in \code{...} into a new
      \code{RleList}.
      If \code{compress}, the internal storage of the data is compressed.}
    \item{}{\code{FactorList(..., compress = TRUE)}: Concatenates the
      \code{factor} objects in \code{...} into a new \code{FactorList}.
      If \code{compress}, the internal storage of the data is compressed.}
  }
}

\section{Coercion}{
  \describe{
    \item{}{
      \code{as(from, "CompressedSplitDataFrameList")},
      \code{as(from, "SimpleSplitDataFrameList")}: Creates a
      \linkS4class{CompressedSplitDataFrameList}/\linkS4class{SimpleSplitDataFrameList}
      instance from an AtomicList instance.
    }
    \item{}{
      \code{as(from, "IRangesList")}, \code{as(from, "CompressedIRangesList")},
      \code{as(from, "SimpleIRangesList")}: Creates a
      \linkS4class{CompressedIRangesList}/\linkS4class{SimpleIRangesList}
      instance from a LogicalList or logical RleList instance. Note that the
      elements of this instance are guaranteed to be normal.
    }
    \item{}{
      \code{as(from, "NormalIRangesList")},
      \code{as(from, "CompressedNormalIRangesList")},
      \code{as(from, "SimpleNormalIRangesList")}: Creates a
      \linkS4class{CompressedNormalIRangesList}/\linkS4class{SimpleNormalIRangesList}
      instance from a LogicalList or logical RleList instance.
    }
    \item{}{\code{as(from, "CharacterList")},
      \code{as(from, "ComplexList")},
      \code{as(from, "IntegerList")},
      \code{as(from, "LogicalList")},
      \code{as(from, "NumericList")},
      \code{as(from, "RawList")},
      \code{as(from, "RleList")}:
      Coerces an \code{AtomicList} \code{from} to another derivative of
      \code{AtomicList}.
    }
    \item{}{\code{as(from, "AtomicList")}: If \code{from} is a vector,
      converts it to an \code{AtomicList} of the appropriate type.
    }
    \item{}{\code{drop(x)}: Checks if every element of \code{x} is of
      length one, and, if so, unlists \code{x}. Otherwise, an error is
      thrown.
    }
    \item{}{\code{as(from, "RleViews")}: Creates an RleViews where each
      view corresponds to an element of \code{from}. The subject is
      \code{unlist(from)}.
    }
    \item{}{\code{as.matrix(x, col.names=NULL)}: Maps the elements of
      the list to rows of a matrix. The column mapping depends on
      whether there are inner names (either on the object or provided
      via \code{col.names} as a List object). If there are no inner
      names, each row is padded with NAs to reach the length of the
      longest element. If there are inner names, there is a column for
      each unique name and the mapping is by name. To provide inner
      names, the \code{col.names} argument should be a List, usually a
      CharacterList or FactorList (which is particularly efficient). If
      \code{col.names} is a character vector, it names the columns of
      the result, but does not imply inner names.
    }
  }
}

\section{Compare, Order, Tabulate}{
  The following methods are provided for element-wise comparison of
  2 AtomicList objects, and ordering or tabulating of each list element
  of an AtomicList object: \code{is.na}, \code{duplicated}, \code{unique},
  \code{match}, \code{\%in\%}, \code{table}, \code{order}, \code{sort}.
}

\section{RleList Methods}{
  RleList has a number of methods that are not shared by other
  AtomicList derivatives.

  \describe{
    \item{}{\code{runLength(x)}: Gets the run lengths of each element of the
      list, as an IntegerList.
    }
    \item{}{\code{runValue(x)}, \code{runValue(x) <- value}: Gets or
      sets the run values of each element of the list, as an AtomicList.
    }
    \item{}{\code{ranges(x)}: Gets the run ranges as a
      \code{RangesList}.
    }
  }
}

\author{P. Aboyoun}

\seealso{
  \itemize{
    \item \link{AtomicList-utils} for common operations on AtomicList objects.

    \item \link[S4Vectors]{List} objects in the \pkg{S4Vectors} package for
          the parent class.
  }
}

\examples{
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
col123 <- c(col1, col2, col3)
col123

## revElements
revElements(col123)
revElements(col123, 4:5)
}
\keyword{methods}
\keyword{classes}
