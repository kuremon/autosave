save_all2=function(save.fun,all.objects,envir=parent.frame(),...){
  if(missing(all.objects)) all.objects=ls(envir=envir)
  Vectorize(FUN=save.fun,vectorize.args="obj.ref")(obj.ref=all.objects,envir=envir,...)
}

# add folder functionality... Allow for different environment ~...
#' @title Save automatically all ggplot objects in the local environment
#' @param folder the folder where the results will be saved.
#' @param format a character string giving the format of the final saved plots.
#' @param ... additional arguments to pass on to \code{\link{save_ggobject}}.
#' @return 1 if no error.
#' @seealso \code{\link{ggsave}}, \code{\link{save_ggobject}}
save_all=function(save,envir=parent.frame(),...){
  all.objects=ls(envir=envir);
  Vectorize(save,"obj.ref")(obj.ref=all.objects,envir=envir,...);
  return(1);
}