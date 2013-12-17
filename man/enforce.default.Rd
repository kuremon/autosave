\name{enforce.default}
\alias{enforce.default}
\title{Enforce default values of a function}
\usage{
  enforce.default(fun, default,
    new.args.before.dots = TRUE, show.warning = FALSE)
}
\arguments{
  \item{fun}{function to modify}

  \item{default}{a named vector or list of new default
  values}

  \item{new.args.before.dots}{boolean indicating wether or
  not the new arguments should be placed before \code{...}.
  If the dots argument is not present or
  \code{new.args.before.dots} is set to \code{FALSE}, new
  arguments appear at the end.}
}
\value{
  The function \code{fun} with default values same as in
  \code{default}.
}
\description{
  Enforce default values of a function
}
\details{
  The default values of \code{fun} not appearing in
  \code{default} are left unchanged.
}
\examples{
\donttest{
fun=function(x=1:10,f,...,k=sum(x)){k}
fun()
fun1=enforce.default(fun,list(e=45,x=1))
fun2=enforce.default(fun,list(e=45))
fun3=enforce.default(fun,list(k=quote(var(x))))
fun4=enforce.default(fun,list(a=85),show.warning=TRUE) # should produce a warning
}
}

