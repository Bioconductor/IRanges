\name{DataFrame-utils}

\alias{DataFrame-utils}

\alias{relistToClass,DataFrame-method}
\alias{relistToClass,data.frame-method}

% splitting
\alias{mstack,DataFrame-method}


\title{Common operations on DataFrame objects}

\description{
  Common operations on \link{DataFrame} objects.
}

\section{Splitting}{
  In the following code snippets, \code{x} is a \code{DataFrame}.
  
  \describe{
    \item{}{\code{split(x, f, drop = FALSE)}:
      Splits \code{x} into a \code{\linkS4class{CompressedSplitDataFrameList}},
      according to \code{f}, dropping elements corresponding to
      unrepresented levels if \code{drop} is \code{TRUE}.
    }
    \item{}{
      \code{mstack(..., .index.var = "name")}: Stacks the data frames
      passed as through \dots, using \code{.index.var} as the index
      column name. See \code{\link{stack}}.
    }
  }
}

\author{ Michael Lawrence }
\seealso{
  \code{\linkS4class{DataTable}},
  \code{\linkS4class{Vector}}, and
  \code{\linkS4class{RangedData}}, which makes heavy use of this class.
}
\examples{
## split

sw <- DataFrame(swiss)
swsplit <- split(sw, sw[["Education"]])
  
## rbind

do.call(rbind, as.list(swsplit))

## cbind

cbind(DataFrame(score), DataFrame(counts))
}
\keyword{methods}
