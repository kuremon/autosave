\name{ls.test}
\alias{ls.test}
\title{List Objects passing a given test}
\usage{
  ls.test(test.fun, ask = TRUE, envir = parent.frame())
}
\arguments{
  \item{test.fun}{a function returning a boolean.}

  \item{ask}{should the user be asked to include elements
  in the list.}

  \item{envir}{the environment where \code{ls.test} is
  looking for objects.}
}
\value{
  A character vector.
}
\description{
  List Objects passing a given test
}
\examples{
\donttest{
ls.test(is.numeric) # produce a list of all the numeric objects present in envir
}
}

