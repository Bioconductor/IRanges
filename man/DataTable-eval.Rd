\name{DataTable-eval}
\alias{DataTable-eval}
\alias{eval,expressionORlanguage,DataTable-method}
\alias{with,DataTable-method}

\title{Evaluations of expressions on DataTable objects}

\description{
  Functions to evaluate expressions on \code{\linkS4class{DataTable}} objects.
}

\usage{
\S4method{eval}{expressionORlanguage,DataTable}(expr, envir, enclos = parent.frame())
\S4method{with}{DataTable}(data, expr, \dots)
}

\arguments{
  \item{expr}{The \code{expression}, \code{call}, or \code{name} to be
    evaluated.}
  \item{envir}{The \code{DataTable} object in which to evaluate
    \code{expr}.}
  \item{enclos}{The \code{environment} in which to look for symbols that
    do not exist in the environment formed from \code{DataTable}.}
  \item{data}{The \code{DataTable} object in which to evaluate
    \code{expr}.}
  \item{\dots}{arguments to be passed to future methods.}
}

\value{The result of expression evaluation.}

\details{  
  The \code{eval} method converts the \code{DataTable} object specified in
  \code{envir} to an \code{environmnent}, with \code{enclos} as its parent,
  and then evaluates \code{expr} within that environment. As when evaluating
  within an ordinary \code{data.frame}, the environment formed from an
  \code{DataTable} contains a symbol for each column name which refers
  to the object stored in that column.
  
  The objects are not actually copied into the environment. Rather, they are
  dynamically bound using \code{\link{makeActiveBinding}}. This prevents
  unnecessary copying of the data from the external vectors into R vectors.
  The values are cached, so that the data is not copied every time the symbol
  is accessed.
}

\author{Michael Lawrence}

\seealso{
  \code{\linkS4class{FilterRules}} objects, which can be evaluated on a
  \code{DataTable}, and the base \code{\link{eval}} function.
}

\examples{
  score <- c(10L, 2L, NA)
  x <- DataFrame(score)
  eval(quote(score > 3), x)
  with(x, score > 3)
}

\keyword{utilities}
