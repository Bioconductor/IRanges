\name{DataTable-stats}
\alias{DataTable-stats}
\alias{xtabs,DataTable-method}

\title{Statistical modeling with DataTable}

\description{
  A number of wrappers are implemented for performing statistical
  procedures, such as model fitting, with \code{\linkS4class{DataTable}}
  objects.
}

\section{Tabulation}{
  \describe{
    \item{}{\code{xtabs(formula = ~., data, subset, na.action, 
        exclude = c(NA, NaN), drop.unused.levels = FALSE)}:
      Like the original \code{\link{xtabs}}, except \code{data} is a
      \code{DataTable}.
    }
  }
}

\seealso{
  \code{\linkS4class{DataTable}} for general manipulation,
  \code{\linkS4class{DataFrame}} for an implementation that mimics
  \code{data.frame}.
}

\examples{
  df <- DataFrame(as.data.frame(UCBAdmissions))
  xtabs(Freq ~ Gender + Admit, df)
}
