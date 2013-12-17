#' @title Define a saving function for object reference
#' @param test.fun a function returning a boolean.
#' @param save.fun the underlying saving function.
#' @param match.args a character vector giving the names of the function \code{save.fun} arguments for the data 
#' to save and the name of the resulting file. The order is important.
#' @param format.default the default format to use in the resulting function
#' @return A saving function working with object references (that is character strings). The resulting function has
#' argument \code{obj.ref}, \code{folder}, \code{naming.fun} (which is by default \code{\link{naming.default}}),
#' \code{format} (which is by default \code{format.default}), \code{...} additional arguments for the \code{save.fun},
#' \code{ask} which is passed to \code{\link{ls.test}}, \code{verbose} and \code{envir}.
#' @export
#' @examples
#' \donttest{
#' histogram_normal=ggplot(data.frame(x=rnorm(1000)))+geom_histogram(aes(x=x),binwidth=0.5)
#' histogram_log.normal=ggplot(data.frame(x=rlnorm(1000)))+geom_histogram(aes(x=x),binwidth=0.5)
#' save_ggplot_default=define_save_obj.ref(is.ggplot,ggsave,match.args=c("plot","filename"),format.default="png")
#' save_ggplot_default("histogram_normal")
#' save_ggplot_default()
#' naming_wide=define_naming(str.after="-wide")
#' save_ggplot_wide=enforce.default(save_ggplot_default,list(naming.fun=naming_wide,width=20,height=5))
#' save_ggplot_wide()
#' }
define_save_obj.ref=function(test.fun,save.fun,match.args,format.default){
  save_obj.ref=function(obj.ref,folder=".",naming.fun=naming.default,format=format.default,...,
                        ask=FALSE,verbose=TRUE,envir=parent.frame()){
    args=c(as.list(environment()),list(...))
    args=args[!names(args)%in%c("obj.ref","folder","naming.fun","format","envir","verbose","ask")]
    
    if(missing(obj.ref)) obj.ref=ls.test(test.fun=test.fun,ask=ask,envir=envir)
    args=c(setNames(list(NULL,NULL),match.args),args)
    
    for(obj.ref.k in obj.ref){
      object=get(obj.ref.k,envir)
      name=naming.fun(folder,obj.ref.k,format)
      args[[1]]=object
      args[[2]]=name
      if(!dir.exists(dir.name<-dirname(name))){
        dir.create(dir.name,recursive=TRUE)
      }
      do.call(save.fun,args)
      if(verbose) message(name," is saved in folder ",file_path_as_absolute(folder))
    }
  }
}

#' @title Save automatically ggplot objects
#' @rdname autosave_ggplot
#' @param obj.ref name(s) of the ggplot object(s).
#' @param folder the folder where the result will be saved.
#' @param naming.fun the naming function. By default \code{naming.fun=naming.default}.
#' @param format a character string giving the format of the final saved plot.
#' @param ... additional arguments used in the underlying saving function.
#' @parm ask a boolean specifying if the user should be asked for selection.
#' @param verbose used to able/disable message.
#' @param envir the environment in which the ggplot objects should be looked for.
#' @export
#' @examples
#' \donttest{
#' histogram_normal=ggplot(data.frame(x=rnorm(1000)))+geom_histogram(aes(x=x),binwidth=0.5)
#' histogram_log.normal=ggplot(data.frame(x=rlnorm(1000)))+geom_histogram(aes(x=x),binwidth=0.5)
#' save_ggplot("histogram_normal")
#' save_ggpdf("histogram_log.normal")
#' library("devEMF")
#' save_ggemf()
#' }
save_ggplot=define_save_obj.ref(is.ggplot,ggsave,match.args=c("plot","filename"),format.default="png")

#' @rdname autosave_ggplot
#' @export
save_ggpdf=enforce.default(save_ggplot,list(format="pdf"))

#' @rdname autosave_ggplot
#' @export
save_ggemf=define_save_obj.ref(is.ggplot,ggsave.emf,match.args=c("plot","filename"),format.default="emf")

#' @title Save automatically gtable objects
#' @description
#' See documentation of \code{\link{save_ggtable}} for a description of the parameter. \code{save_data.frame}
#' is constructed by \code{\link{define_save_obj.ref}}.
#' @export
#' @examples
#' \donttest{
#' wt_per_cyl=ggplot(mtcars,aes(x=as.factor(cyl),y=wt))+geom_boxplot()+xlab("")
#' breaks=data.frame(cyl=c(4,6,8),type=c(rep("small cars",2),"big cars"))
#' breaks=get_x.info(breaks,"cyl","type")
#' wt_per_cyl=add_x.info(wt_per_cyl,breaks,y.level=-0.25)
#' save_ggtable("wt_per_cyl")
#' save_ggtable("wt_per_cyl",format="emf")
#' }
save_ggtable=define_save_obj.ref(is.grob,write.ggtable,match.args=c("plot","filename"),format.default="png")

#' @title Save automatically data frame objects
#' @description
#' See documentation of \code{\link{save_ggplot}} for a description of the parameter. \code{save_data.frame}
#' is constructed by \code{\link{define_save_obj.ref}}.
#' @details
#' By default, the data frames are saved without row names.
#' @seealso \code{\link{save_data.frame.wb}}
#' @export
#' @examples
#' \donttest{
#' data(mtcars,iris)
#' save_data.frame()
#' }
save_data.frame=enforce.default(
  define_save_obj.ref(is.data.frame,write.xlsx2,match.args=c("x","file"),format.default="xlsx"),
  list(row.names=FALSE))

write.in.wb=function(data,sheet,wb,...){
  createSheet(object = wb, name = sheet)
  writeWorksheet(object = wb, data = data, sheet = sheet, ...)
}

save_data.frame.wb.base=define_save_obj.ref(is.data.frame,write.in.wb,match.args=c("data","sheet"),format.default="")

#' @title Save automatically data frames in spreadsheets of a single workbook
#' @param file The name of the resulting workbook. By default \code{file="all_dataframes.xlsx"}.
#' @param ... Additional arguments to pass on to \code{\link{writeWorksheet}}
#' @param envir the environment in which the data frames should be looked for.
#' @details
#' By default, the data frames are saved without row names.
#' @seealso \code{\link{save_data.frame}}
#' @export
#' @examples
#' \donttest{
#' data(mtcars,iris)
#' save_data.frame.wb()
#' }
save_data.frame.wb=function(file="all_dataframes.xlsx",...,envir=parent.frame()){
  wb=loadWorkbook(file,create=TRUE)
  save_data.frame.wb.base(naming.fun=identity_naming,wb=wb,...,ask=TRUE,envir=envir)
  saveWorkbook(wb)
}