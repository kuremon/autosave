\name{define_save_obj.ref}
\alias{define_save_obj.ref}
\title{Define a saving function for object reference}
\usage{
  define_save_obj.ref(test.fun, save.fun, match.args,
    format.default)
}
\arguments{
  \item{test.fun}{a function returning a boolean.}

  \item{save.fun}{the underlying saving function.}

  \item{match.args}{a character vector giving the names of
  the function \code{save.fun} arguments for the data to
  save and the name of the resulting file. The order is
  important.}

  \item{format.default}{the default format to use in the
  resulting function}
}
\value{
  A saving function working with object references (that is
  character strings). The resulting function has argument
  \code{obj.ref}, \code{folder}, \code{naming.fun} (which
  is by default \code{\link{naming.default}}),
  \code{format} (which is by default
  \code{format.default}), \code{...} additional arguments
  for the \code{save.fun}, \code{ask} which is passed to
  \code{\link{ls.test}}, \code{verbose} and \code{envir}.
}
\description{
  Define a saving function for object reference
}
\examples{
\donttest{
histogram_normal=ggplot(data.frame(x=rnorm(1000)))+geom_histogram(aes(x=x),binwidth=0.5)
histogram_log.normal=ggplot(data.frame(x=rlnorm(1000)))+geom_histogram(aes(x=x),binwidth=0.5)
save_ggplot_default=define_save_obj.ref(is.ggplot,ggsave,match.args=c("plot","filename"),format.default="png")
save_ggplot_default("histogram_normal")
save_ggplot_default()
naming_wide=define_naming(str.after="-wide")
save_ggplot_wide=enforce.default(save_ggplot_default,list(naming.fun=naming_wide,width=20,height=5))
save_ggplot_wide()
}
}

