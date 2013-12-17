\name{save_ggplot}
\alias{save_ggemf}
\alias{save_ggpdf}
\alias{save_ggplot}
\title{Save automatically ggplot objects}
\usage{
  save_ggplot(obj.ref, folder = ".",
    naming.fun = naming.default, format = format.default,
    ..., ask = FALSE, verbose = TRUE,
    envir = parent.frame())

  save_ggpdf(obj.ref, folder = ".",
    naming.fun = naming.default, format = "pdf", ...,
    ask = FALSE, verbose = TRUE, envir = parent.frame())

  save_ggemf(obj.ref, folder = ".",
    naming.fun = naming.default, format = format.default,
    ..., ask = FALSE, verbose = TRUE,
    envir = parent.frame())
}
\arguments{
  \item{obj.ref}{name(s) of the ggplot object(s).}

  \item{folder}{the folder where the result will be saved.}

  \item{naming.fun}{the naming function. By default
  \code{naming.fun=naming.default}.}

  \item{format}{a character string giving the format of the
  final saved plot.}

  \item{...}{additional arguments used in the underlying
  saving function.}

  \item{verbose}{used to able/disable message.}

  \item{envir}{the environment in which the ggplot objects
  should be looked for.}
}
\description{
  Save automatically ggplot objects
}
\examples{
\donttest{
histogram_normal=ggplot(data.frame(x=rnorm(1000)))+geom_histogram(aes(x=x),binwidth=0.5)
histogram_log.normal=ggplot(data.frame(x=rlnorm(1000)))+geom_histogram(aes(x=x),binwidth=0.5)
save_ggplot("histogram_normal")
save_ggpdf("histogram_log.normal")
library("devEMF")
save_ggemf()
}
}

