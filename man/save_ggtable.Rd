\name{save_ggtable}
\alias{save_ggtable}
\title{Save automatically gtable objects}
\usage{
  save_ggtable(obj.ref, folder = ".",
    naming.fun = naming.default, format = format.default,
    ..., ask = FALSE, verbose = TRUE,
    envir = parent.frame())
}
\description{
  See documentation of \code{\link{save_ggtable}} for a
  description of the parameter. \code{save_data.frame} is
  constructed by \code{\link{define_save_obj.ref}}.
}
\examples{
\donttest{
wt_per_cyl=ggplot(mtcars,aes(x=as.factor(cyl),y=wt))+geom_boxplot()+xlab("")
breaks=data.frame(cyl=c(4,6,8),type=c(rep("small cars",2),"big cars"))
breaks=get_x.info(breaks,"cyl","type")
wt_per_cyl=add_x.info(wt_per_cyl,breaks,y.level=-0.25)
save_ggtable("wt_per_cyl")
save_ggtable("wt_per_cyl",format="emf")
}
}

