

#' @title Transfer From Vertical placement into Horizontal placement for casewise vectors

#'
#' @param vector a vector, such as hit vector, h or f counted by each cases.
#' @param NI Number of images, in other words, cases
#' @param M Number of modalities
#' @param Q Number of readers
#' @param C Number of confidence levels, in other words, ratings
#'
#' @return an array whose dimension is NI, M,Q,C
#' @export
#'
#' @examples
#'
#' h <-dcasewise$h
#' horizontal_from_vertical_in_each_case(h, 200,5,4,5)
#'
#' f <-dcasewise$f
#' horizontal_from_vertical_in_each_case(f, 200,5,4,5)
horizontal_from_vertical_in_each_case <- function(vector, NI, M,Q,C){

  h<- vector

  hhh <-array(0,c(NI,M,Q,C))

  for(case in 1:NI)
    for (mmm in 1:M) {

      for (qqq in 1:Q) {
        for (ccc in 1:C) {
          hhh[case,mmm,qqq,ccc] <-  h[ (case -1 )*M*Q*C + (mmm-1)*Q*C+(qqq-1)*C+(C+1-ccc)]
          # f[ (case -1 )*M*Q*C + (mmm-1)*Q*C+(qqq-1)*C+(C+1-ccc)] <-fff[case,mmm,qqq,ccc]

        }}}

  return(hhh)

}




#' @title Transfer From Horizontal placement into Vertical  placement for casewise vectors

#'
#' @param array a array, such as hit array, hhh or fff counted for each cases.
#' @param NI Number of images, in other words, cases
#' @param M Number of modalities
#' @param Q Number of readers
#' @param C Number of confidence levels, in other words, ratings
#'
#' @return an array whose dimension is NI, M,Q,C
#' @export
#'
# @examples
#'

vertical_from_horizontal_in_each_case <- function(array, NI, M,Q,C){

  h<- array(0,NI*M*Q*C)

  hhh <-array

  for(case in 1:NI)
    for (mmm in 1:M) {

      for (qqq in 1:Q) {
        for (ccc in 1:C) {
          h[ (case -1 )*M*Q*C + (mmm-1)*Q*C+(qqq-1)*C+(C+1-ccc)] <- hhh[case,mmm,qqq,ccc]
          # f[ (case -1 )*M*Q*C + (mmm-1)*Q*C+(qqq-1)*C+(C+1-ccc)] <-fff[case,mmm,qqq,ccc]

        }}}

  return(h)

}








