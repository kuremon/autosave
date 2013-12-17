\name{define_naming}
\alias{define_naming}
\alias{identity_naming}
\alias{naming.default}
\title{List Objects passing a given test}
\usage{
  define_naming(str.before = "", str.after = "")

  naming.default(folder = ".", name, format = "")

  identity_naming(folder, name, format)
}
\arguments{
  \item{test.fun}{a function returning a boolean.}

  \item{ask}{should the user be asked to include elements
  in the list.}

  \item{envir}{the environment where \code{ls.test} is
  looking for objects.}
}
\value{
  A naming function.
}
\description{
  List Objects passing a given test
}
\details{
  By default the returning function will transform all the
  dots in names by underscores. This feature is useful when
  used with saving functions as dots in names are
  misunderstood as extension when saving. For example if
  \code{naming.default("a.b")} gives \code{"a_b"}.
}
\examples{
\donttest{
naming_wide=define_naming(str.after="-wide")
naming_wide(name="this_plot",format="png")
}
}

