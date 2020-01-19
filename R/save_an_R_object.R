#
#
#


#  @title Save an \R object in Desktop or Working Directory
#  @description The author always \strong{forgets} how to save an \R object in desktop, so this is the reason why the he made this.
#  @param object an \R object which is saved
#  @param file_name name of file in which the \R object is saved.
# @param Working_Directory A logical,
# if \code{TRUE} then the object is saved  \strong{in the working directory}.
# if \code{FALSE}, then the \R object is saved \strong{in Desktop}.
#
# @return \code{NULL}
# @export
#
# @examples
# save_an_R_object  <- function(object, file_name , Working_Directory =FALSE) {
#
#   # if (class("file_name")=="character"){
#   #   file_name <-substitute(file_name)
#   # }
#   # if(!Working_Directory) save( object,file =paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"),"\\fit") )
#
# #  if(!Working_Directory) save( object,file =paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"),"\\",file_name) )
# # # browser()
# #   if(Working_Directory)save( object,file =paste0(file_name) )
# #
# #   return(NULL)
#
# }
