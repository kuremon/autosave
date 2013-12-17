is.valid.ggplot=function(g){
  if(!is.ggplot(g))return(FALSE);
  has_layers=length(g$layers)>0;
  return(has_layers);
}

# deal with identical name/pre existing plots...
#' @title Save automatically a ggplot object
#' @description 
#' This is function saves the ggplot given the name of the plot. 
#' By default, the filename is based on the variable name (see Details).
#' @param obj.ref variable name of the ggplot object.
#' @param name the name from which the final filename will be constructed. By default \code{name=obj.ref}.
#' @param envir the environment in which the ggplot should be looked for.
#' @param format a character string giving the format of the final saved plot.
#' @param folder the folder where the result will be saved.
#' @param info to able/disable message (useful when using \code{save_all.ggobject})
#' @param before.name a character string added before \code{name} when creating the file name.
#' @param after.name a character string added after \code{name} when creating the file name.
#' @param ... additional arguments used in \code{\link{ggsave}}.
#' @details
#' By default all the dots \code{"."} within the name of the variable are transformed to underscore \code{"."}. For example
#' if \code{gg.plot} is a ggplot object, \code{save_ggobject} will construct the filename from \code{"gg_plot"}.
#' @seealso \code{\link{ggsave}}, \code{\link{save_all.ggobject}}
save_ggobject=function(obj.ref,name=obj.ref,envir=parent.frame(),format="png",folder=".",info=T,
                       before.name=NULL,after.name=NULL,...){
  plot=get(obj.ref,envir);
  name=paste(paste0(before.name,gsub("\\.","_",name),after.name),format,sep=".");
  args=c(list(plot=plot,filename=file.path(folder,name)),list(...));
  do.call("ggsave",args);
  if(info) message(name," is saved in folder ",file_path_as_absolute(folder));
}

# add folder functionality... Allow for different environment ~...
#' @title Save automatically all ggplot objects in the local environment
#' @param folder the folder where the results will be saved.
#' @param format a character string giving the format of the final saved plots.
#' @param ... additional arguments to pass on to \code{\link{save_ggobject}}.
#' @return 1 if no error.
#' @seealso \code{\link{ggsave}}, \code{\link{save_ggobject}}
save_all.ggobject=function(folder=".",format="png",...){
  pf=parent.frame();
  all.objects=ls(envir=pf);
  gg.objects.index=NULL;
  for(object in all.objects){
    gg.objects.index=c(gg.objects.index,test_valid.ggplot(get(object,envir=parent.frame())));
  }
  gg.objects=all.objects[gg.objects.index];
  Vectorize(save_ggobject,"obj.ref")(obj.ref=gg.objects,envir=pf,format=format,folder=folder,...);
  return(1);
}