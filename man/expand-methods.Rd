\name{expand}

\alias{expand}
\alias{expand,DataFrame-method}

\title{The expand method for uncompressing compressed data columns}
\description{
  The expand method takes an object like a \code{DataFrame} and expands
  it row-wise such that columns which are compressed can each be
  represented as a separate row.
}

\usage{
  expand(x, colnames, keepEmptyRows)
}


\arguments{
  \item{x}{
    A DataFrame containing some columns that are compressed.  Such as a
    \code{CompressedCharacterList}.
  }
  \item{colnames}{
    A character vector containing the names of the comnpressed columns
    that need to be expanded.  The result will be expanded for each
    named column name in turn, so you can control the order in which the
    columns are expanded by the ordering of this character vector.
  }
  \item{keepEmptyRows}{
    TRUE or FALSE value to indicate whether or not empty values in the
    colnames columns should be set to NA and the adjacent data retained.
    If set to TRUE, and then you expanded based on a collumn which
    contains some empty elements, then any data next to those empty
    elements will NOT be dropped from the final result.  If set to
    false, then data adjacent to empty elements in expanded columns will
    be dropped.
  }
}

\value{
  A \code{DataFrame} that has been expanded row-wise to allow expansion
  of the \code{CharacterList} columns specified.
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
