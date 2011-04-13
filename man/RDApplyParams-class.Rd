\name{rdapply}
\alias{rdapply}
\alias{rdapply,RDApplyParams-method}
\alias{RDApplyParams-class}

% constructor
\alias{RDApplyParams}

% accessors
\alias{applyFun}
\alias{applyFun,RDApplyParams-method}
\alias{applyFun<-}
\alias{applyFun<-,RDApplyParams-method}
\alias{applyParams}
\alias{applyParams,RDApplyParams-method}
\alias{applyParams<-}
\alias{applyParams<-,RDApplyParams-method}
\alias{filterRules}
\alias{filterRules,RDApplyParams-method}
\alias{filterRules<-}
\alias{filterRules<-,RDApplyParams-method}
\alias{rangedData}
\alias{rangedData,RDApplyParams-method}
\alias{rangedData<-}
\alias{rangedData<-,RDApplyParams-method}
\alias{reducerFun}
\alias{reducerFun,RDApplyParams-method}
\alias{reducerFun<-}
\alias{reducerFun<-,RDApplyParams-method}
\alias{reducerParams}
\alias{reducerParams,RDApplyParams-method}
\alias{reducerParams<-}
\alias{reducerParams<-,RDApplyParams-method}
\alias{simplify}
\alias{simplify,RDApplyParams-method}
\alias{simplify<-}
\alias{simplify<-,RDApplyParams-method}
\alias{iteratorFun}
\alias{iteratorFun,RDApplyParams-method}
\alias{iteratorFun<-}
\alias{iteratorFun<-,RDApplyParams-method}

\title{Applying over spaces}
\description{The \code{rdapply} function applies a user function over
  the spaces of a \code{\linkS4class{RangedData}}. The parameters to
  \code{rdapply} are collected into an instance of \code{RDApplyParams},
  which is passed as the sole parameter to \code{rdapply}.}

\usage{
rdapply(x, ...)
}
\arguments{
  \item{x}{The \code{RDApplyParams} instance, see below for how to
    make one.}
  \item{...}{Additional arguments for methods}
}
\value{
  By default a \code{list} holding the result of each invocation of the
  user function, but see details.
}
\details{
  The \code{rdapply} function is an attempt to facilitate the common
  operation of performing the same operation over each space
  (e.g. chromosome) in a \code{RangedData}. To facilitate a wide array
  of such tasks, the function takes a large number of options. The
  \code{RDApplyParams} class is meant to help manage this
  complexity. In particular, it facilitates experimentation through its
  support for incremental changes to parameter settings.
  
  There are two \code{RangedData} settings that are required: the
  user \code{function} object and the \code{RangedData} over which it is
  applied. The rest of the settings determine what is actually passed to
  the user function and how the return value is processed before
  relaying it to the user. The following is the description and
  rationale for each setting.

  \describe{
    \item{\code{rangedData}}{\strong{REQUIRED}.
      The \code{RangedData} instance over which \code{applyFun} is
      applied. 
    }
    \item{\code{applyFun}}{\strong{REQUIRED}.
      The user \code{function} to be applied to each space in the
      \code{RangedData}. The function must expect the \code{RangedData} as its
      first parameter and also accept the parameters specified in
      \code{applyParams}.
    }
    \item{\code{applyParams}}{
      The \code{list} of additional parameters to pass to
      \code{applyFun}. Usually empty.
    }
    \item{\code{filterRules}}{
      The instance of \code{\linkS4class{FilterRules}} that is used to
      filter each subset of the \code{RangedData} passed to the user
      function. This is an efficient and convenient means for performing
      the same operation over different subsets of the data on a
      space-by-space basis. In particular, this avoids the need to store
      subsets of the entire \code{RangedData}. A common workflow is to
      invoke \code{rdapply} with one set of active filters, enable
      different filters, reinvoke \code{rdapply}, and compare the
      results.
    }
    \item{\code{simplify}}{
      A scalar logical (\code{TRUE} or \code{FALSE}) indicating whether
      the \code{list} to be returned from \code{rdapply} should be
      simplified as by \code{\link{sapply}}. Defaults to \code{FALSE}.
    }
    \item{\code{reducerFun}}{
      The \code{function} that is used to convert the \code{list} that
      would otherwise be returned from \code{rdapply} to something more
      convenient. The function should take the list as its first
      parameter and also accept the parameters specified in
      \code{reducerParams}. This is an alternative to the
      primitive behavior of the \code{simplify} option (so
      \code{simplify} must be \code{FALSE} if this option is set). The
      aim is to orthogonalize the \code{applyFun} operation (i.e. the
      statistics) from the data structure of the result.
    }
    \item{\code{reducerParams}}{
      A \code{list} of additional parameters to pass to \code{reducerFun}.
      Can only be set if \code{reducerFun} is set. Usually empty.
    }
    \item{\code{iteratorFun}}{
      The function used for applying over the \code{RangedData}. By
      default, this is \code{lapply}, but it could also be a specialized
      function, like \code{mclapply}.
    }
  }
}


\section{Constructing an RDApplyParams object}{
  \describe{
    \item{}{\code{RDApplyParams(rangedData, applyFun, applyParams,
        filterRules, simplify, reducerFun, reducerParams)}:
      Constructs a \code{RDApplyParams} object with each setting
      specified by the argument of the same name. See the Details section
      for more information.
    }
  }
}

\section{Accessors}{
  In the following code snippets, \code{x} is an \code{RDApplyParams}
  object.

  \describe{
    \item{}{\code{rangedData(x)}, \code{rangedData(x) <- value}: Get or set the
      \code{RangedData} instance over which \code{applyFun} is applied.
    }
    \item{}{\code{applyFun(x)}, \code{applyFun(x) <- value}: Get or set the user
      \code{function} to be applied to each space in the \code{RangedData}.
    }
    \item{}{\code{applyParams(x)}, \code{applyParams(x) <- value}: Get
      or set the \code{list} of additional parameters to pass to
      \code{applyFun}.
    }
    \item{}{\code{filterRules(x)}, \code{filterRules(x) <- value}: Get
      or set the instance of \code{\linkS4class{FilterRules}} that is used to
      filter each subset of the \code{RangedData} passed to the user
      function. 
    }
    \item{}{\code{simplify(x)}, \code{simplify(x) <- value}: Get or set a
      a scalar logical (\code{TRUE} or \code{FALSE}) indicating whether
      the \code{list} to be returned from \code{rdapply} should be
      simplified as by \code{\link{sapply}}.
    }
    \item{}{\code{reducerFun(x)}, \code{reducerFun(x) <- value}: Get or set the
      \code{function} that is used to convert the \code{list} that would
      otherwise be returned from \code{rdapply} to something more convenient.
    }
    \item{}{\code{reducerParams(x)}, \code{reducerParams(x) <- value}:
      Get or set a \code{list} of additional parameters to pass to
      \code{reducerFun}.
    }
    \item{}{\code{iteratorFun(x)}, \code{iteratorFun(x) <- value}:
      Get or set the function used for applying over the \code{RangedData}.
    }
  }
}

\author{Michael Lawrence}
\seealso{
  \code{\linkS4class{RangedData}}, \code{\linkS4class{FilterRules}}
}
\examples{
  ranges <- IRanges(c(1,2,3),c(4,5,6))
  score <- c(2L, 0L, 1L)
  rd <- RangedData(ranges, score, space = c("chr1","chr2","chr1"))
  
  ## a single function
  countrows <- function(rd) nrow(rd)
  params <- RDApplyParams(rd, countrows)
  rdapply(params) # list(chr1 = 2L, chr2 = 1L)

  ## with a parameter
  params <- RDApplyParams(rd, function(rd, x) nrow(rd)*x, list(x = 2))
  rdapply(params) # list(chr1 = 4L, chr2 = 2L)

  ## add a filter
  cutoff <- 0
  rules <- FilterRules(filter = score > cutoff)
  params <- RDApplyParams(rd, countrows, filterRules = rules)
  rdapply(params) # list(chr1 = 2L, chr2 = 0L)
  rules <- FilterRules(list(fun = function(rd) rd[["score"]] < 2),
                       filter = score > cutoff)
  params <- RDApplyParams(rd, countrows, filterRules = rules)
  rdapply(params) # list(chr1 = 1L, chr2 = 0L)
  active(filterRules(params))["filter"] <- FALSE
  rdapply(params) # list(chr1 = 1L, chr2 = 1L)

  ## simplify
  params <- RDApplyParams(rd, countrows, simplify = TRUE)
  rdapply(params) # c(chr1 = 2L, chr2 = 1L)

  ## reducing
  params <- RDApplyParams(rd, countrows, reducerFun = unlist,
                          reducerParams = list(use.names = FALSE))
  rdapply(params) ## c(2L, 1L)
}
\keyword{classes}
\keyword{methods}
