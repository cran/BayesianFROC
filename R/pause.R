#' @title Pause for Demo
#' @description Pause for Demo
#
# @examples
#'
#'  pause()
#'
#'@export pause

pause <- function() {
  message("\n
* Hit Enter key from the R (R-studio) console. (not from a Graphic device if it exists).

* If a new graphic device appears, then please get back the cursol in the R (R-studio) console to hit Enter key. When the graphic device open then cursole move from R console and it bother me to execute this demo,... is there any methods to remain cursol in R console in such case?
          ")

  invisible(readline())


  }
