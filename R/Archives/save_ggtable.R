#' @title Save automatically a gtable object 
save_gtable=function(obj.ref,name=obj.ref,envir=parent.frame(),format="png",folder=".",info=T,
                     before.name=NULL,after.name=NULL,...){
  obj=get(obj.ref,envir);
  if(inherits(obj,"gtable")){
    name=paste(paste0(before.name,gsub("\\.","_",name),after.name),format,sep=".");
    args=c(list(filename=file.path(folder,name)),list(...));
    do.call(format,args);
    grid.draw(obj);
    dev.off();
    if(info) message(name," is saved in folder ",file_path_as_absolute(folder));
  }
}

#' @title Save automatically a gtable object 
save_all.gtable=function(envir=parent.frame(),...){
  save_all(save=save_gtable,envir=envir,...)
}