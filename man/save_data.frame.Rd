\name{save_data.frame}
\alias{save_data.frame}
\title{Save automatically data frame objects}
\usage{
  save_data.frame(obj.ref, folder = ".",
    naming.fun = naming.default, format = format.default,
    row.names = FALSE, ..., ask = FALSE, verbose = TRUE,
    envir = parent.frame())
}
\description{
  See documentation of \code{\link{save_ggplot}} for a
  description of the parameter. \code{save_data.frame} is
  constructed by \code{\link{define_save_obj.ref}}.
}
\details{
  By default, the data frames are saved without row names.
}
\examples{
\donttest{
data(mtcars,iris)
save_data.frame()
}
}
\seealso{
  \code{\link{save_data.frame.wb}}
}

