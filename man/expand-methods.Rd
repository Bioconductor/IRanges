\name{expand}

\alias{expand}
\alias{expand,DataFrame-method}

\title{The expand method for uncompressing compressed data columns}
\description{
  Expand an object with compressed columns such that all compressed
  values are represented as separate rows.
}

\usage{
\S4method{expand}{DataFrame}(x, colnames, keepEmptyRows, ...)
}

\arguments{
  \item{x}{
    A \code{DataFrame} containing some columns that are compressed (e.g.,
    \code{CompressedCharacterList}).
  }
  \item{colnames}{
    A \code{character} or \code{numeric} vector containing the names 
    or indices of the compressed columns to expand. The order of expansion 
    is controlled by the column order in this vector.
  }
  \item{keepEmptyRows}{
    A \code{logical} indicating if rows containing empty values in the 
    specified \code{colnames} should be retained or dropped. When \code{TRUE},
    empty values are set to NA and all rows are kept. When \code{FALSE},
    rows with empty values in the \code{colnames} columns are dropped.
  }
  \item{\dots}{Arguments passed to other methods.
  }
}

\value{
  A \code{DataFrame} that has been expanded row-wise to match the dimension
  of the uncompressed columns.
}

\author{Herve Pages and Marc Carlson}

\seealso{
  \link{DataFrame-class}
}

\examples{
aa <- CharacterList("a", paste0("d", 1:2), paste0("b", 1:3), c(), "c")
bb <- CharacterList(paste0("sna", 1:2),"foo", paste0("bar",1:3),c(),"hica")
df <- DataFrame(aa=aa, bb=bb, cc=11:15)

## expand the aa column only, and keep rows adjacent to empty values
expand(df, colnames="aa", keepEmptyRows=TRUE)

## expand the aa column only but do not keep rows
expand(df, colnames="aa", keepEmptyRows=FALSE)

## expand the aa and then the bb column, but
## keeping rows next to empty compressed values
expand(df, colnames=c("aa","bb"),  keepEmptyRows=TRUE)

## expand the bb and then the aa column, but don't keep rows adjacent to
## empty values from bb and aa
expand(df, colnames=c("aa","bb"), keepEmptyRows=FALSE)

}

\keyword{methods}
