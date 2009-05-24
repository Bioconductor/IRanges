\name{DataFrame-stats}

\alias{xtabs,ANY,DataFrame-method}

\title{Statistical modeling with DataFrame}

\description{
  A number of wrappers are implemented for performing statistical
  procedures, such as model fitting, with \code{\linkS4class{DataFrame}}
  objects.
}

\section{Regression}{
}

\section{Tabulation}{
  \describe{
    \item{}{\code{xtabs(formula = ~., data, subset, na.action, 
        exclude = c(NA, NaN), drop.unused.levels = FALSE)}:
      Like the original \code{\link{xtabs}}, except \code{data} is a
      \code{DataFrame}.
    }
  }
}

\seealso{
  \code{\linkS4class{DataFrame}} for general manipulation,
  \code{\linkS4class{XDataFrame}} for an implementation that mimics
  \code{data.frame}.
}

\examples{
  xdf <- XDataFrame(as.data.frame(UCBAdmissions))
  xtabs(Freq ~ Gender + Admit, xdf)
}