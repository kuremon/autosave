save_data.frame=function(obj.ref,name=obj.ref,envir=parent.frame(),format="xlsx",folder=".",info=T,
                         before.name=NULL,after.name=NULL,...){
  data=get(obj.ref,envir)
  name=paste(paste0(before.name,gsub("\\.","_",name),after.name),format,sep=".")
  args=c(list(x=data,file=file.path(folder,name)),list(...))
  do.call("write.xlsx2",args)
  if(info) message(name," is saved in folder ",file_path_as_absolute(folder))
}

save_all.data.frame=function(all.objects,envir=parent.frame(),...){
  save_all(save_data.frame,all.objects,envir=parent.frame(),...)
}