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

\alias{as.vector,AtomicList,missing-method}
\alias{as.logical,AtomicList-method}
\alias{as.integer,AtomicList-method}
\alias{as.numeric,AtomicList-method}
\alias{as.complex,AtomicList-method}
\alias{as.character,AtomicList-method}
\alias{as.raw,AtomicList-method}
\alias{as.factor,AtomicList-method}
\alias{coerce,AtomicList,vector-method}
\alias{coerce,AtomicList,logical-method}
\alias{coerce,AtomicList,integer-method}
\alias{coerce,AtomicList,numeric-method}
\alias{coerce,AtomicList,complex-method}
\alias{coerce,AtomicList,character-method}
\alias{coerce,AtomicList,raw-method}
\alias{coerce,AtomicList,factor-method}
\alias{coerce,AtomicList,SimpleSplitDataFrameList-method}
\alias{coerce,AtomicList,CompressedSplitDataFrameList-method}

\alias{coerce,LogicalList,IRangesList-method}
\alias{coerce,LogicalList,CompressedIRangesList-method}
\alias{coerce,LogicalList,SimpleIRangesList-method}
\alias{coerce,RleList,IRangesList-method}
\alias{coerce,RleList,CompressedIRangesList-method}
\alias{coerce,RleList,SimpleIRangesList-method}

\alias{seqselect<-,CompressedAtomicList-method}
\alias{seqselect<-,SimpleAtomicList-method}

\alias{Ops,CompressedAtomicList,CompressedAtomicList-method}
\alias{Ops,SimpleAtomicList,SimpleAtomicList-method}
\alias{Ops,SimpleAtomicList,CompressedAtomicList-method}
\alias{Ops,CompressedAtomicList,SimpleAtomicList-method}
\alias{Ops,AtomicList,atomic-method}
\alias{Ops,atomic,AtomicList-method}
\alias{!,SimpleLogicalList-method}
\alias{!,CompressedLogicalList-method}
\alias{Math,CompressedAtomicList-method}
\alias{Math,SimpleAtomicList-method}
\alias{Math2,CompressedAtomicList-method}
\alias{Math2,SimpleAtomicList-method}
\alias{Summary,AtomicList-method}
\alias{Complex,CompressedAtomicList-method}
\alias{Complex,SimpleAtomicList-method}

\alias{runmean,RleList-method}
\alias{runmed,RleList-method}
\alias{runsum,RleList-method}
\alias{runwtsum,RleList-method}
\alias{runq,RleList-method}


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
    \item{}{\code{RleList(..., compress = FALSE)}: Concatenates the
      run-length encoded atomic vectors in \code{...} into a new
      \code{RleList}.
      If \code{compress}, the internal storage of the data is compressed.}
  }
}

\section{Coercion}{
  \describe{
    \item{}{
      \code{as.vector(x)}, \code{as(from, "vector")}: Creates a vector based
      on the values contained in \code{x}.
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

\section{Fixed Width Running Window Summaries}{
  RleList objects have support for the following fixed width running window
  summary methods:
  \describe{
    \item{}{
      \code{runmean(x, k, endrule = c("drop", "constant"))}:
      Calculates the means for fixed width running windows across \code{x}.
      \describe{
        \item{\code{x}}{An RleList object containing integer or numeric
          Rle elements.}
        \item{\code{k}}{An integer indicating the fixed width of the running
          window. Must be odd when \code{endrule == "constant"}.}
        \item{endrule}{A character string indicating how the values at the
          beginning and the end (of the data) should be treated.
          \describe{
            \item{\code{"drop"}}{do not extend the running statistics to
              be the same length as the underlying vectors;}
            \item{\code{"constant"}}{copies running statistic to the
              first values and analogously for the last ones making the
              smoothed ends \emph{constant};}
          }
        }
      }
    }
    \item{}{
      \code{runmed(x, k, endrule = c("median", "keep", "constant"))}:
      Calculates the medians for fixed width running windows across \code{x}.
      \describe{
        \item{\code{x}}{An RleList object containing integer or numeric
          Rle elements.}
        \item{\code{k}}{An integer indicating the fixed width of the running
          window. Must be odd when \code{endrule != "drop"}.}
        \item{endrule}{A character string indicating how the values at the
          beginning and the end (of the data) should be treated.
          \describe{
            \item{\code{"drop"}}{do not extend the running statistics to
              be the same length as the underlying vectors;}
            \item{\code{"keep"}}{keeps the first and last \eqn{k_2}{k2}
              values at both ends, where \eqn{k_2}{k2} is the half-bandwidth
              \code{k2 = k \%/\% 2}, i.e., \code{y[[i]][j] = x[[i]][j]} for
              \eqn{j \in \{1,\ldots,k_2; n-k_2+1,\ldots,n\}}
              {j = 1,..,k2 and (n-k2+1),..,n};}
            \item{\code{"constant"}}{copies the running statistic to the
              first values and analogously for the last ones making the
              smoothed ends \emph{constant};}
            \item{\code{"median"}}{the default, smooths the ends by using
              symmetrical medians of subsequently smaller bandwidth, but
              for the very first and last value where Tukey's robust
              end-point rule is applied, see \code{\link[stats]{smoothEnds}}.}
          }
        }
      }
    }
    \item{}{
      \code{runsum(x, k, endrule = c("drop", "constant"))}:
      Calculates the sums for fixed width running windows across \code{x}.
      \describe{
        \item{\code{x}}{An RleList object containing integer or numeric
          Rle elements.}
        \item{\code{k}}{An integer indicating the fixed width of the
          running window. Must be odd when \code{endrule == "constant"}.}
        \item{endrule}{A character string indicating how the values at the
          beginning and the end (of the data) should be treated.
          \describe{
            \item{\code{"drop"}}{do not extend the running statistics to
              be the same length as the underlying vectors;}
            \item{\code{"constant"}}{copies running statistic to the
              first values and analogously for the last ones making the
              smoothed ends \emph{constant};}
          }
        }
      }
    }
    \item{}{
      \code{runwtsum(x, k, wt, endrule = c("drop", "constant"))}:
      Calculates the sums for fixed width running windows across \code{x}.
      \describe{
        \item{\code{x}}{An RleList object containing integer or numeric
          Rle elements.}
        \item{\code{k}}{An integer indicating the fixed width of the
          running window. Must be odd when \code{endrule == "constant"}.}
        \item{\code{wt}}{A numeric vector of length \code{k} that
          provides the weights to use.}
        \item{endrule}{A character string indicating how the values at the
          beginning and the end (of the data) should be treated.
          \describe{
            \item{\code{"drop"}}{do not extend the running statistics to
              be the same length as the underlying vectors;}
            \item{\code{"constant"}}{copies running statistic to the
              first values and analogously for the last ones making the
              smoothed ends \emph{constant};}
          }
        }
      }
    }
    \item{}{
      \code{runq(x, k, i, endrule = c("drop", "constant"))}:
      Calculates the order statistic for fixed width running windows across
      \code{x}.
      \describe{
        \item{\code{x}}{An RleList object containing integer or numeric
          Rle elements.}
        \item{\code{k}}{An integer indicating the fixed width of the
          running window. Must be odd when \code{endrule == "constant"}.}
        \item{\code{i}}{An integer indicating which order statistic to
          calculate.}
        \item{endrule}{A character string indicating how the values at the
          beginning and the end (of the data) should be treated.
          \describe{
            \item{\code{"drop"}}{do not extend the running statistics to
              be the same length as the underlying vectors;}
            \item{\code{"constant"}}{copies running statistic to the
              first values and analogously for the last ones making the
              smoothed ends \emph{constant};}
          }
        }
      }
    }
  }
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
