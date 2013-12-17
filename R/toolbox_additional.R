#' @title Enforce default values of a function
#' @param fun function to modify
#' @param default a named vector or list of new default values
#' @param new.args.before.dots boolean indicating wether or not the new arguments should be placed before \code{...}.
#' If the dots argument is not present or \code{new.args.before.dots} is set to \code{FALSE}, new arguments
#' appear at the end.
#' @return The function \code{fun} with default values same as in \code{default}.
#' @details The default values of \code{fun} not appearing in \code{default} are left unchanged.
#' @export
#' @examples
#' \donttest{
#' fun=function(x=1:10,f,...,k=sum(x)){k}
#' fun()
#' fun1=enforce.default(fun,list(e=45,x=1))
#' fun2=enforce.default(fun,list(e=45))
#' fun3=enforce.default(fun,list(k=quote(var(x))))
#' fun4=enforce.default(fun,list(a=85),show.warning=TRUE) # should produce a warning
#' }
enforce.default=function(fun,default,new.args.before.dots=TRUE,show.warning=FALSE){
  default=as.pairlist(default)
  args=formals(fun)
  midx=match(names(default),names(args))
  
  existing.args=which(!is.na(midx))
  args[midx[existing.args]]=default[existing.args]
  
  new.args=which(is.na(midx))
  if(show.warning&&(length(new.args)>0)) warning("Introducing new arguments in the function.",call.=FALSE)
  
  dots.idx=which(names(args)=="...")
  if(new.args.before.dots&&(length(dots.idx)>0)){
    args=insert(args,default[new.args],dots.idx-1+seq(new.args))
  }else{
    args=c(args,default[new.args])
  }
  formals(fun)=args
  fun
}

#' @title List Objects passing a given test
#' @param test.fun a function returning a boolean.
#' @param ask should the user be asked to include elements in the list.
#' @param envir the environment where \code{ls.test} is looking for objects.
#' @return A character vector.
#' @export
#' @examples
#' \donttest{
#' ls.test(is.numeric) # produce a list of all the numeric objects present in envir
#' }
ls.test=function(test.fun,ask=TRUE,envir=parent.frame()){
  all.objects=ls(envir=envir)
  to.keep=sapply(all.objects,function(obj.ref)test.fun(get(obj.ref,envir=envir)))
  objects=all.objects[to.keep]
  if(ask&&(length(objects)>0)){
    ask.fun=function(obj.ref) readline(paste("Should",obj.ref,"be included? (y/n): "))=="y"
    objects=objects[sapply(objects,ask.fun)]
  }
  objects
}

#' @title List Objects passing a given test
#' @rdname naming
#' @param test.fun a function returning a boolean.
#' @param ask should the user be asked to include elements in the list.
#' @param envir the environment where \code{ls.test} is looking for objects.
#' @return A naming function.
#' @details
#' By default the returning function will transform all the dots in names by underscores. This feature is useful
#' when used with saving functions as dots in names are misunderstood as extension when saving. 
#' For example if \code{naming.default("a.b")} gives \code{"a_b"}.
#' @export
#' @examples
#' \donttest{
#' naming_wide=define_naming(str.after="-wide")
#' naming_wide(name="this_plot",format="png")
#' }
define_naming=function(str.before="",str.after=""){
  function(folder=".",name,format=""){
    name=paste0(str.before,gsub("\\.","_",name),str.after)
    if(nchar(format)>0) name=paste(name,format,sep=".")
    if(nchar(folder)>0) name=file.path(folder,name)
    name
  }
}

#' @rdname naming
#' @export
naming.default=define_naming()

#' @rdname naming
#' @export
identity_naming=function(folder,name,format) name

#' @title Save a ggplot object as an emf file
#' @param plot The ggplot object to write.
#' @param filename The name of the file.
#' @param ... The additional arguments to pass on to \code{\link{emf}}.
#' @export
ggsave.emf=function(plot,filename,...){
  emf(file=filename,...)
  print(plot)
  dev.off()
}

write.ggtable=function(plot,filename,...){
  do.call(file_ext(filename),list(filename,...))
  grid.draw(plot)
  dev.off()
}