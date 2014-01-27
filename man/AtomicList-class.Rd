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

% coercion
\alias{as.list,CompressedAtomicList-method}
\alias{coerce,CompressedAtomicList,list-method}
\alias{as.vector,AtomicList-method}
\alias{coerce,vector,AtomicList-method}
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
\alias{coerce,CompressedRleList,CompressedIRangesList-method}

\alias{Ops,CompressedAtomicList,CompressedAtomicList-method}
\alias{Ops,SimpleAtomicList,SimpleAtomicList-method}
\alias{Ops,SimpleAtomicList,CompressedAtomicList-method}
\alias{Ops,CompressedAtomicList,SimpleAtomicList-method}
\alias{Ops,AtomicList,atomic-method}
\alias{Ops,atomic,AtomicList-method}
\alias{Ops,CompressedAtomicList,atomic-method}
\alias{Ops,atomic,CompressedAtomicList-method}
\alias{Ops,SimpleAtomicList,atomic-method}
\alias{Ops,atomic,SimpleAtomicList-method}
\alias{Math,CompressedAtomicList-method}
\alias{Math,SimpleAtomicList-method}
\alias{Math2,CompressedAtomicList-method}
\alias{Math2,SimpleAtomicList-method}
\alias{Summary,AtomicList-method}
\alias{Summary,CompressedRleList-method}
\alias{Complex,CompressedAtomicList-method}
\alias{Complex,SimpleAtomicList-method}
\alias{sum,CompressedIntegerList-method}
\alias{sum,CompressedLogicalList-method}
\alias{sum,CompressedNumericList-method}

\alias{unique,CompressedRleList-method}
\alias{unique.CompressedRleList}
\alias{unique,SimpleRleList-method}
\alias{unique.SimpleRleList}
\alias{table,SimpleAtomicList-method}
\alias{table,CompressedAtomicList-method}
\alias{drop,AtomicList-method}

\alias{which,CompressedLogicalList-method}
\alias{which,SimpleLogicalList-method}
\alias{which,CompressedRleList-method}
\alias{which,SimpleRleList-method}
\alias{which.max,CompressedRleList-method}
\alias{which.min,CompressedRleList-method}
\alias{all,CompressedRleList-method}

\alias{diff,IntegerList-method}
\alias{diff,NumericList-method}
\alias{diff,RleList-method}

\alias{pmax,IntegerList-method}
\alias{pmax,NumericList-method}
\alias{pmax,RleList-method}
\alias{pmin,IntegerList-method}
\alias{pmin,NumericList-method}
\alias{pmin,RleList-method}
\alias{pmax.int,IntegerList-method}
\alias{pmax.int,NumericList-method}
\alias{pmax.int,RleList-method}
\alias{pmin.int,IntegerList-method}
\alias{pmin.int,NumericList-method}
\alias{pmin.int,RleList-method}

\alias{mean,AtomicList-method}
\alias{var,AtomicList,missing-method}
\alias{var,AtomicList,AtomicList-method}
\alias{cov,AtomicList,AtomicList-method}
\alias{cor,AtomicList,AtomicList-method}
\alias{sd,AtomicList-method}
\alias{median,AtomicList-method}
\alias{quantile,AtomicList-method}
\alias{mad,AtomicList-method}
\alias{IQR,AtomicList-method}
\alias{cumsum,CompressedAtomicList-method}
\alias{cumprod,CompressedAtomicList-method}
\alias{cummin,CompressedAtomicList-method}
\alias{cummax,CompressedAtomicList-method}

\alias{smoothEnds,CompressedIntegerList-method}
\alias{smoothEnds,SimpleIntegerList-method}
\alias{smoothEnds,NumericList-method}
\alias{smoothEnds,RleList-method}
\alias{runmed,CompressedIntegerList-method}
\alias{runmed,SimpleIntegerList-method}
\alias{runmed,NumericList-method}
\alias{runmed,RleList-method}
\alias{runmean,RleList-method}
\alias{runsum,RleList-method}
\alias{runwtsum,RleList-method}
\alias{runq,RleList-method}

\alias{nchar,CompressedCharacterList-method}
\alias{nchar,SimpleCharacterList-method}
\alias{nchar,CompressedRleList-method}
\alias{nchar,SimpleRleList-method}
\alias{chartr,ANY,ANY,CompressedCharacterList-method}
\alias{chartr,ANY,ANY,SimpleCharacterList-method}
\alias{chartr,ANY,ANY,CompressedRleList-method}
\alias{chartr,ANY,ANY,SimpleRleList-method}
\alias{tolower,CompressedCharacterList-method}
\alias{tolower,SimpleCharacterList-method}
\alias{tolower,CompressedRleList-method}
\alias{tolower,SimpleRleList-method}
\alias{toupper,CompressedCharacterList-method}
\alias{toupper,SimpleCharacterList-method}
\alias{toupper,CompressedRleList-method}
\alias{toupper,SimpleRleList-method}
\alias{sub,ANY,ANY,CompressedCharacterList-method}
\alias{sub,ANY,ANY,SimpleCharacterList-method}
\alias{sub,ANY,ANY,CompressedRleList-method}
\alias{sub,ANY,ANY,SimpleRleList-method}
\alias{gsub,ANY,ANY,CompressedCharacterList-method}
\alias{gsub,ANY,ANY,SimpleCharacterList-method}
\alias{gsub,ANY,ANY,CompressedRleList-method}
\alias{gsub,ANY,ANY,SimpleRleList-method}
\alias{unstrsplit,CharacterList-method}

\alias{runLength,RleList-method}
\alias{runValue,RleList-method}
\alias{runLength,CompressedRleList-method}
\alias{runValue,CompressedRleList-method}

\alias{ranges,RleList-method}
\alias{ranges,CompressedRleList-method}

\alias{show,AtomicList-method}
\alias{show,RleList-method}
\alias{showAsCell,AtomicList-method}


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
    \item{\code{Math}}{\code{"abs"}, \code{"sign"}, \code{"sqrt"},
      \code{"ceiling"}, \code{"floor"}, \code{"trunc"}, \code{"cummax"},
      \code{"cummin"}, \code{"cumprod"}, \code{"cumsum"}, \code{"log"},
      \code{"log10"}, \code{"log2"}, \code{"log1p"}, \code{"acos"},
      \code{"acosh"}, \code{"asin"}, \code{"asinh"}, \code{"atan"},
      \code{"atanh"}, \code{"exp"}, \code{"expm1"}, \code{"cos"},
      \code{"cosh"}, \code{"sin"}, \code{"sinh"}, \code{"tan"}, \code{"tanh"},
      \code{"gamma"}, \code{"lgamma"}, \code{"digamma"}, \code{"trigamma"}}
    \item{\code{Math2}}{\code{"round"}, \code{"signif"}}
    \item{\code{Summary}}{\code{"max"}, \code{"min"}, \code{"range"},
      \code{"prod"}, \code{"sum"}, \code{"any"}, \code{"all"}}
    \item{\code{Complex}}{\code{"Arg"}, \code{"Conj"}, \code{"Im"},
      \code{"Mod"}, \code{"Re"}}
  }
  See \link[methods]{S4groupGeneric} for more details.
}

\section{Other Basic Methods}{
  The AtomicList objects also support a large number of basic methods.
  Like the group generics above, these methods perform the corresponding
  operation on each element of the list separately. The methods are: 

  \describe{
    \item{General}{\code{is.na},
                   \code{duplicated}, \code{unique},
                   \code{match}, \code{\%in\%}, \code{table},
                   \code{order}, \code{sort}}
    \item{Logical}{\code{!}, \code{which}, \code{which.max}, \code{which.min}}
    \item{Numeric}{\code{diff},
                   \code{pmax}, \code{pmax.int}, \code{pmin}, \code{pmin.int},
                   \code{mean}, \code{var}, \code{cov}, \code{cor}, \code{sd},
                   \code{median}, \code{quantile}, \code{mad}, \code{IQR}}
    \item{Running Window}{\code{smoothEnds}, \code{runmed}. \code{runmean},
                          \code{runsum}, \code{runwtsum}, \code{runq}}
    \item{Character}{\code{nchar}, \code{chartr}, \code{tolower},
                     \code{toupper}, \code{sub}, \code{gsub}}
  }
}

\section{RleList Methods}{
  RleList has a number of methods that are not shared by other
  AtomicList derivatives.

  \describe{
    \item{}{\code{runLength(x)}: Gets the run lengths of each element of the
      list, as an IntegerList.
    }
    \item{}{\code{runValue(x)}: Gets the run values of each element of the
      list, as an AtomicList.
    }
    \item{}{\code{ranges(x)}: Gets the run ranges as a
      \code{RangesList}.
    }
  }
}

\section{Specialized Methods}{
  \describe{
    \item{}{\code{drop(x)}: Checks if every element of \code{x} is of
      length one, and, if so, unlists \code{x}. Otherwise, an error is
      thrown.
    }
    \item{}{\code{unstrsplit(x, sep="")}}: A fast
    \code{sapply(x, paste0, collapse=sep)}. See \code{?\link{unstrsplit}}
    for the details.
  }
}

\author{P. Aboyoun}
\seealso{\code{\linkS4class{List}} for the applicable methods.}

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

## group generics
2 * col1
col1 + col1
col1 > 2
sum(col1)  # equivalent to (but faster than) 'sapply(col1, sum)'
mean(col1)  # equivalent to 'sapply(col1, mean)'
}
\keyword{methods}
\keyword{classes}
