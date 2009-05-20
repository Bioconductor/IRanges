\name{DataFrame-eval}
\alias{DataFrame-eval}
\alias{eval,expressionORlanguage,DataFrame-method}
\alias{with,DataFrame-method}

\title{Evaluations of expressions on DataFrame objects}

\description{
  Functions to evaluate expressions on \code{\linkS4class{DataFrame}} objects.
}

\usage{
\S4method{eval}{expressionORlanguage,DataFrame}(expr, envir, enclos = parent.frame())
\S4method{with}{DataFrame}(data, expr, \dots)
}

\arguments{
  \item{expr}{The \code{expression}, \code{call}, or \code{name} to be
    evaluated.}
  \item{envir}{The \code{DataFrame} object in which to evaluate
    \code{expr}.}
  \item{enclos}{The \code{environment} in which to look for symbols that
    do not exist in the environment formed from \code{DataFrame}.}
  \item{data}{The \code{DataFrame} object in which to evaluate
    \code{expr}.}
  \item{\dots}{arguments to be passed to future methods.}
}

\value{The result of expression evaluation.}

\details{  
  The \code{eval} method converts the \code{DataFrame} object specified in
  \code{envir} to an \code{environmnent}, with \code{enclos} as its parent,
  and then evaluates \code{expr} within that environment. As when evaluating
  within an ordinary \code{data.frame}, the environment formed from an
  \code{DataFrame} contains a symbol for each column name which refers
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
  \code{DataFrame}, and the base \code{\link{eval}} function.
}

\examples{
  score <- c(10L, 2L, NA)
  x <- XDataFrame(score)
  eval(quote(score > 3), x)
  with(x, score > 3)
}

\keyword{utilities}
