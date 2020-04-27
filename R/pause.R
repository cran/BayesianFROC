#' @title Pause for Demo
#' @description Pause if and only if \code{interactive() = TRUE}.
#' @param simple A logical. If false, then verbose.
# @examples
#'
#'  pause()
#'
#'@export pause

pause <- function(simple = FALSE) {
  ## Only run examples in interactive R sessions
  if (interactive()) {
if(simple==FALSE)  message("\n

* ", crayon::bgWhite$red$bold$italic$underline(" Hit Enter key  "), "from the R (R-studio) console. (not from a Graphic device if it exists).

* If a new graphic device appears, then please ", crayon::bgWhite$red$bold$italic$underline(" Hit Enter key  on the graphic device and get back the cursol in the R (R-studio) console"), " to hit Enter key. When the graphic device open then cursole move from R console to a graphic device and it bother me to execute this demo,... is there any methods to remain or automatically get back the cursol from graphic device to  R console in such case?
          ")


  message("Type  <Return>	 to start :")

  invisible(readline())

  }  ## Only run examples in interactive R sessions

  }
