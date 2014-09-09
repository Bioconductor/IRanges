\name{DataFrame-utils}

\alias{DataFrame-utils}

% splitting and combining
\alias{cbind,DataFrame-method}
\alias{rbind,DataFrame-method}
\alias{mstack,DataFrame-method}


\title{Common operations on DataFrame objects}

\description{
  Common operations on \link{DataFrame} objects.
}

\section{Splitting and Combining}{
  In the following code snippets, \code{x} is a \code{DataFrame}.
  
  \describe{
    \item{}{\code{split(x, f, drop = FALSE)}:
      Splits \code{x} into a \code{\linkS4class{CompressedSplitDataFrameList}},
      according to \code{f}, dropping elements corresponding to
      unrepresented levels if \code{drop} is \code{TRUE}.
    }
    \item{}{
      \code{rbind(...)}: Creates a new \code{DataFrame} by
      combining the rows of the \code{DataFrame} objects in
      \code{...}. Very similar to \code{\link{rbind.data.frame}}, except
      in the handling of row names. If all elements have row names, they
      are concatenated and made unique. Otherwise, the result does not
      have row names. Currently, factors are not handled well (their
      levels are dropped). This is not a high priority until there is an
      \code{XFactor} class.
    }
    \item{}{
      \code{cbind(...)}: Creates a new \code{DataFrame} by
      combining the columns of the \code{DataFrame} objects in
      \code{...}. Very similar to \code{\link{cbind.data.frame}}, except
      row names, if any, are dropped. Consider the \code{DataFrame}
      as an alternative that allows one to specify row names.
    }
    \item{}{
      \code{mstack(..., .index.var = "name")}: Stacks the data frames
      passed as through \dots, using \code{.index.var} as the index
      column name. See \code{\link{stack}}.
    }
  }
}

\section{Aggregation}{
  In the following code snippets, \code{data} is a \code{DataFrame}.
  \describe{
    \item{}{\code{aggregate(x, data, FUN, ..., subset, na.action =
        na.omit)}:
      Aggregates the \code{DataFrame} \code{data} according to the
      formula \code{x} and the aggregating
      function \code{FUN}. See \code{\link{aggregate}} and its method
      for \code{formula}.
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
