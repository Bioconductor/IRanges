\name{SimpleList-class}
\docType{class}
\alias{SimpleList}
\alias{SimpleList-class}
\alias{CompressedList}
\alias{CompressedList-class}

% accessors
\alias{elementLengths}
\alias{elementLengths,CompressedList-method}
\alias{elementLengths,SimpleList-method}
\alias{length,CompressedList-method}
\alias{length,SimpleList-method}
\alias{names,CompressedList-method}
\alias{names,SimpleList-method}
\alias{names<-,CompressedList-method}
\alias{names<-,SimpleList-method}

% subsetting
\alias{[,CompressedList-method}
\alias{[,SimpleList-method}
\alias{[[,CompressedList-method}
\alias{[[,SimpleList-method}
\alias{[[<-,CompressedList-method}
\alias{[[<-,SimpleList-method}
\alias{$<-,CompressedList-method}
\alias{$<-,SimpleList-method}

% splitting and combining
\alias{append,CompressedList,CompressedList-method}
\alias{append,SimpleList,SimpleList-method}
\alias{c,CompressedList-method}
\alias{c,SimpleList-method}

% looping
\alias{lapply,CompressedList-method}
\alias{lapply,SimpleList-method}

% coercion
\alias{as.list,CompressedList-method}
\alias{as.list,SimpleList-method}
\alias{unlist,CompressedList-method}
\alias{unlist,SimpleList-method}

% show
\alias{show,CompressedList-method}
\alias{show,SimpleList-method}


\title{Simple and Compressed List Classes}
\description{The SimpleList and CompressedList virtual classes extend the
  \linkS4class{Sequence} virtual class.}

\details{
  The SimpleList and CompressedList classes provide an implementation that
  subclasses can easily extend. The underlying storage in a SimpleList
  subclass is a list object. The underlying storage in a CompressedList
  object is a virtually partitioned vector-like object. For more information
  on the available methods, see the \linkS4class{Sequence} man page.
}
\section{Additional Accessors}{
  In the following code snippets, \code{x} is a \code{SimpleList} or
  \code{CompressedList} object.
  
  \describe{
    \item{}{\code{elementLengths(x)}: Get the 'length' of each of the elements.
    }
  }
}

\author{P. Aboyoun}
\seealso{\linkS4class{Sequence},
         \linkS4class{IntegerList} and
         \linkS4class{RangesList} for example implementations}

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
