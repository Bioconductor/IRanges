\name{AtomicList}
\docType{class}

% AtomicList classes
\alias{AtomicList-class}
\alias{CompressedAtomicList-class}
\alias{SimpleAtomicList-class}
\alias{LogicalList}
\alias{LogicalList-class}
\alias{CompressedLogicalList}
\alias{CompressedLogicalList-class}
\alias{SimpleLogicalList}
\alias{SimpleLogicalList-class}
\alias{IntegerList}
\alias{IntegerList-class}
\alias{CompressedIntegerList}
\alias{CompressedIntegerList-class}
\alias{SimpleIntegerList}
\alias{SimpleIntegerList-class}
\alias{NumericList}
\alias{NumericList-class}
\alias{CompressedNumericList}
\alias{CompressedNumericList-class}
\alias{SimpleNumericList}
\alias{SimpleNumericList-class}
\alias{ComplexList}
\alias{ComplexList-class}
\alias{CompressedComplexList}
\alias{CompressedComplexList-class}
\alias{SimpleComplexList}
\alias{SimpleComplexList-class}
\alias{CharacterList}
\alias{CharacterList-class}
\alias{CompressedCharacterList}
\alias{CompressedCharacterList-class}
\alias{SimpleCharacterList}
\alias{SimpleCharacterList-class}
\alias{RawList}
\alias{RawList-class}
\alias{CompressedRawList}
\alias{CompressedRawList-class}
\alias{SimpleRawList}
\alias{SimpleRawList-class}
\alias{RleList}
\alias{RleList-class}
\alias{CompressedRleList}
\alias{CompressedRleList-class}
\alias{SimpleRleList}
\alias{SimpleRleList-class}

\alias{Ops,CompressedAtomicList,CompressedAtomicList-method}
\alias{Ops,SimpleAtomicList,SimpleAtomicList-method}
\alias{Ops,SimpleAtomicList,CompressedAtomicList-method}
\alias{Ops,CompressedAtomicList,SimpleAtomicList-method}
\alias{Ops,AtomicList,atomic-method}
\alias{Ops,atomic,AtomicList-method}


\title{Lists of Atomic Vectors in Natural and Rle Form}
\description{An extension of \code{\linkS4class{Sequence}} that holds
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
  }
}

\section{Group Generics}{
  AtomicList objects have support for S4 group generic functionality to operate
  within elements across objects:
  \describe{
    \item{\code{Arith}}{\code{"+"}, \code{"-"}, \code{"*"}, \code{"^"},
      \code{"\%\%"}, \code{"\%/\%"}, \code{"/"}}
    \item{\code{Compare}}{\code{"=="}, \code{">"}, \code{"<"}, \code{"!="}, 
      \code{"<="}, \code{">="}}
    \item{\code{Logic}}{\code{"&"}, \code{"|"}}
    \item{\code{Ops}}{\code{"Arith"}, \code{"Compare"}, \code{"Logic"}}
  }
  See \link[methods]{S4groupGeneric} for more details.
}

\author{P. Aboyoun}
\seealso{\code{\linkS4class{Sequence}} for the applicable methods.}

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
  c(col1, col2, col3)

  ## get the mean for each element
  lapply(col1, mean)
}
\keyword{methods}
\keyword{classes}
