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

\alias{as.vector,AtomicList,ANY-method}
\alias{as.vectorORfactor,AtomicList-method}
\alias{as.logical,AtomicList-method}
\alias{as.integer,AtomicList-method}
\alias{as.numeric,AtomicList-method}
\alias{as.complex,AtomicList-method}
\alias{as.character,AtomicList-method}
\alias{as.raw,AtomicList-method}
\alias{as.factor,AtomicList-method}
\alias{as.data.frame,AtomicList-method}
\alias{as.list,CompressedAtomicList-method}
\alias{coerce,AtomicList,vector-method}
\alias{coerce,AtomicList,logical-method}
\alias{coerce,AtomicList,integer-method}
\alias{coerce,AtomicList,numeric-method}
\alias{coerce,AtomicList,complex-method}
\alias{coerce,AtomicList,character-method}
\alias{coerce,AtomicList,raw-method}
\alias{coerce,AtomicList,factor-method}
\alias{coerce,AtomicList,data.frame-method}
\alias{coerce,CompressedAtomicList,list-method}

\alias{lapply,CompressedAtomicList-method}

\alias{coerce,LogicalList,IRangesList-method}
\alias{coerce,LogicalList,CompressedIRangesList-method}
\alias{coerce,LogicalList,SimpleIRangesList-method}
\alias{coerce,LogicalList,NormalIRangesList-method}
\alias{coerce,LogicalList,CompressedNormalIRangesList-method}
\alias{coerce,LogicalList,SimpleNormalIRangesList-method}
\alias{coerce,RleList,IRangesList-method}
\alias{coerce,RleList,CompressedIRangesList-method}
\alias{coerce,RleList,SimpleIRangesList-method}
\alias{coerce,RleList,NormalIRangesList-method}
\alias{coerce,RleList,CompressedNormalIRangesList-method}
\alias{coerce,RleList,SimpleNormalIRangesList-method}

\alias{coerce,vector,AtomicList-method}

\alias{seqselect<-,CompressedAtomicList-method}
\alias{seqselect<-,SimpleAtomicList-method}

\alias{Ops,CompressedAtomicList,CompressedAtomicList-method}
\alias{Ops,SimpleAtomicList,SimpleAtomicList-method}
\alias{Ops,SimpleAtomicList,CompressedAtomicList-method}
\alias{Ops,CompressedAtomicList,SimpleAtomicList-method}
\alias{Ops,AtomicList,atomic-method}
\alias{Ops,atomic,AtomicList-method}
\alias{Ops,CompressedAtomicList,atomic-method}
\alias{Ops,atomic,CompressedAtomicList-method}
\alias{Math,CompressedAtomicList-method}
\alias{Math,SimpleAtomicList-method}
\alias{Math2,CompressedAtomicList-method}
\alias{Math2,SimpleAtomicList-method}
\alias{Summary,AtomicList-method}
\alias{Summary,CompressedRleList-method}
\alias{Complex,CompressedAtomicList-method}
\alias{Complex,SimpleAtomicList-method}

\alias{\%in\%,CompressedAtomicList,atomic-method}
\alias{\%in\%,SimpleAtomicList,atomic-method}
\alias{\%in\%,CompressedRleList,atomic-method}
\alias{\%in\%,SimpleRleList,atomic-method}
\alias{\%in\%,CompressedAtomicList,AtomicList-method}
\alias{\%in\%,SimpleAtomicList,AtomicList-method}
\alias{\%in\%,CompressedRleList,AtomicList-method}
\alias{\%in\%,SimpleRleList,AtomicList-method}
\alias{is.na,CompressedAtomicList-method}
\alias{is.na,SimpleAtomicList-method}
\alias{is.na,CompressedRleList-method}
\alias{is.na,SimpleRleList-method}
\alias{match,CompressedAtomicList,atomic-method}
\alias{match,SimpleAtomicList,atomic-method}
\alias{match,CompressedRleList,atomic-method}
\alias{match,SimpleRleList,atomic-method}
\alias{match,CompressedAtomicList,AtomicList-method}
\alias{match,SimpleAtomicList,AtomicList-method}
\alias{match,CompressedRleList,AtomicList-method}
\alias{match,SimpleRleList,AtomicList-method}
\alias{sort,AtomicList-method}
\alias{table,SimpleAtomicList-method}
\alias{table,CompressedAtomicList-method}
\alias{unique,AtomicList-method}
\alias{unique,CompressedRleList-method}
\alias{unique,SimpleRleList-method}

\alias{!,CompressedLogicalList-method}
\alias{!,SimpleLogicalList-method}
\alias{!,CompressedRleList-method}
\alias{!,SimpleRleList-method}
\alias{which,CompressedLogicalList-method}
\alias{which,SimpleLogicalList-method}
\alias{which,CompressedRleList-method}
\alias{which,SimpleRleList-method}
\alias{which.max,CompressedRleList-method}
\alias{which.min,CompressedRleList-method}

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

\alias{mean,LogicalList-method}
\alias{mean,IntegerList-method}
\alias{mean,NumericList-method}
\alias{mean,RleList-method}
\alias{var,LogicalList,missing-method}
\alias{var,LogicalList,AtomicList-method}
\alias{var,IntegerList,missing-method}
\alias{var,IntegerList,AtomicList-method}
\alias{var,NumericList,missing-method}
\alias{var,NumericList,AtomicList-method}
\alias{var,RleList,missing-method}
\alias{var,RleList,AtomicList-method}
\alias{cov,LogicalList,AtomicList-method}
\alias{cov,IntegerList,AtomicList-method}
\alias{cov,NumericList,AtomicList-method}
\alias{cov,RleList,AtomicList-method}
\alias{cor,LogicalList,AtomicList-method}
\alias{cor,IntegerList,AtomicList-method}
\alias{cor,NumericList,AtomicList-method}
\alias{cor,RleList,AtomicList-method}
\alias{sd,LogicalList-method}
\alias{sd,IntegerList-method}
\alias{sd,NumericList-method}
\alias{sd,RleList-method}
\alias{median,LogicalList-method}
\alias{median,IntegerList-method}
\alias{median,NumericList-method}
\alias{median,RleList-method}
\alias{quantile,LogicalList-method}
\alias{quantile,IntegerList-method}
\alias{quantile,NumericList-method}
\alias{quantile,RleList-method}
\alias{mad,LogicalList-method}
\alias{mad,IntegerList-method}
\alias{mad,NumericList-method}
\alias{mad,RleList-method}
\alias{IQR,LogicalList-method}
\alias{IQR,IntegerList-method}
\alias{IQR,NumericList-method}
\alias{IQR,RleList-method}

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
    \item{}{\code{RleList(..., compress = FALSE)}: Concatenates the
      run-length encoded atomic vectors in \code{...} into a new
      \code{RleList}.
      If \code{compress}, the internal storage of the data is compressed.}
  }
}

\section{Coercion}{
  \describe{
    \item{}{
      \code{as.vector(x, mode = "any")}, \code{as(from, "vector")}:
      Creates a vector based on the values contained in \code{x}. The
      vector will be coerced to the requested \code{mode}, unless
      \code{mode} is "any", in which case the most appropriate type is
      chosen.
    }
    \item{}{
      \code{as.vectorORfactor(x)}: Creates an atomic vector or factor,
      based on the type of values contained in \code{x}. This is the
      most general way to flatten the list to a native R data structure.
    }
    \item{}{
      \code{as.logical(x)}, \code{as(from, "logical")}: Creates a logical
      vector based on the values contained in \code{x}.
    }
    \item{}{
      \code{as.integer(x)}, \code{as(from, "integer")}: Creates an integer
      vector based on the values contained in \code{x}.
    }
    \item{}{
      \code{as.numeric(x)}, \code{as(from, "numeric")}: Creates a numeric
      vector based on the values contained in \code{x}.
    }
    \item{}{
      \code{as.complex(x)}, \code{as(from, "complex")}: Creates a complex
      vector based on the values contained in \code{x}.
    }
    \item{}{
      \code{as.character(x)}, \code{as(from, "character")}: Creates a character
      vector based on the values contained in \code{x}.
    }
    \item{}{
      \code{as.raw(x)}, \code{as(from, "raw")}: Creates a raw
      vector based on the values contained in \code{x}.
    }
    \item{}{
      \code{as.factor(x)}, \code{as(from, "factor")}: Creates a factor object
      based on the values contained in \code{x}.
    }
    \item{}{\code{as.data.frame(x, row.names = NULL, optional = FALSE)},
      \code{as(from, "data.frame")}: Creates a \code{data.frame} object based
      on the values contained in \code{x}. Essentially the same as calling
      \code{data.frame(space=rep(names(x), elementLengths(x)), as.vector(x))}.
    }
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
    \item{General}{\code{\%in\%}, \code{is.na}, \code{match}, \code{sort},
                   \code{table}, \code{unique}}
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
  c(col1, col2, col3)

  ## group generics
  2 * col1
  col1 + col1
  col1 > 2
  log(col1)
  sum(col1)

  ## get the mean for each element
  sapply(col1, mean)
}
\keyword{methods}
\keyword{classes}
