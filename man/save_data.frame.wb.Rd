\name{save_data.frame.wb}
\alias{save_data.frame.wb}
\title{Save automatically data frames in spreadsheets of a single workbook}
\usage{
  save_data.frame.wb(file = "all_dataframes.xlsx", ...,
    envir = parent.frame())
}
\arguments{
  \item{file}{The name of the resulting workbook. By
  default \code{file="all_dataframes.xlsx"}.}

  \item{...}{Additional arguments to pass on to
  \code{\link{writeWorksheet}}}

  \item{envir}{the environment in which the data frames
  should be looked for.}
}
\description{
  Save automatically data frames in spreadsheets of a
  single workbook
}
\details{
  By default, the data frames are saved without row names.
}
\examples{
\donttest{
data(mtcars,iris)
save_data.frame.wb()
}
}
\seealso{
  \code{\link{save_data.frame}}
}

