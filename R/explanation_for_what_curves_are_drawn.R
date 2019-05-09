
#' @title Print out about what curves are drawn
#' @description For package developer.
#' @param modalityID A vector.
#' @param readerID A  vector..
#'
#' @return Nothing
#' @export
#'
#' @examples
#'  \donttest{
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#================The first example======================================
#'
#'
#'   modalityID <-c(1,2)
#'   readerID <-c(1,2,3)
#'
#'   explanation_for_what_curves_are_drawn( modalityID, readerID )
#'
#'
#'#================The second example======================================
#'
#'
#'   modalityID <- 1
#'   readerID <-c(1,2,3)
#'
#'   explanation_for_what_curves_are_drawn( modalityID, readerID )
#'
#'
#'}# dottest
#'
#'
explanation_for_what_curves_are_drawn <-function( modalityID, readerID ){

  # modalityID <-c(1,2)
  # modalityID <- 1
  # readerID <-c(1,2,3)

  message(   crayon::silver("\n*       Curves for the specified modalities and readers. \n")   )
  for (md in modalityID) {
    for (qd in readerID) {

      message("\n*",  crayon::silver("A curve of the "),crayon::bold( crayon::bgBlack$white$bold(  md )),"-th modality",   crayon::silver( "and the "),crayon::bgBlack$white$bold( qd ) ,"-th reader", crayon::silver(" is drawn.")  )

    }
  }
  # message("\n")

}#function
