is.res.test=function(x){
  if(!is.list(x))return(FALSE);
  return(setequal(names(x),c("res.test","plot")));
}

# deal with identical name/pre existing plots...
#' @title Save automatically a res.test
#' @description 
#' This is function saves the res.test given the name of the list 
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
save_res.test=function(obj.ref,name=obj.ref,envir=parent.frame(),format="png",folder=".",info=T,
                       before.name=NULL,after.name=NULL,...){
  res.test=get(obj.ref,envir);
  name=paste0(before.name,gsub("\\.","_",name),after.name);
  
  ggsave(plot=res.test$plot,filename=file.path(folder,paste(name,format,sep=".")),...);
  write.xlsx2(res.test$res.test,file=file.path(folder,paste(name,"xlsx",sep=".")),row.names=F);
  if(info) message("The res.test files for ",name," are saved in folder ",file_path_as_absolute(folder));
}

# add folder functionality... Allow for different environment ~...
#' @title Save automatically all res.test in the local environment
#' @param folder the folder where the results will be saved.
#' @param format a character string giving the format of the final saved plots.
#' @param ... additional arguments to pass on to \code{\link{save_res.test}}.
#' @return 1 if no error.
#' @seealso \code{\link{ggsave}}, \code{\link{save_ggobject}}
save_all.res.test=function(folder=".",format="png",...){
  pf=parent.frame();
  all.objects=ls(envir=pf);
  res.test.objects.index=NULL;
  for(object in all.objects){
    res.test.objects.index=c(res.test.objects.index,is.res.test(get(object,envir=parent.frame())));
  }
  res.test.objects=all.objects[res.test.objects.index];
  Vectorize(save_res.test,"obj.ref")(obj.ref=res.test.objects,envir=pf,format=format,folder=folder,...);
  return(1);
}