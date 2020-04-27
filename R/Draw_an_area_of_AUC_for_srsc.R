# # This is a function on ge
#
# theme_black = function(base_size = 12, base_family = "") {
#
#   theme_grey(base_size = base_size, base_family = base_family) %+replace%
#
#     theme(
#       # Specify axis options
#       axis.line = element_blank(),
#       axis.text.x = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),
#       axis.text.y = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),
#       axis.ticks = element_line(color = "white", size  =  0.2),
#       axis.title.x = element_text(size = base_size, color = "white", margin = margin(0, 10, 0, 0)),
#       axis.title.y = element_text(size = base_size, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),
#       axis.ticks.length = unit(0.3, "lines"),
#       # Specify legend options
#       legend.background = element_rect(color = NA, fill = "black"),
#       legend.key = element_rect(color = "white",  fill = "black"),
#       legend.key.size = unit(1.2, "lines"),
#       legend.key.height = NULL,
#       legend.key.width = NULL,
#       legend.text = element_text(size = base_size*0.8, color = "white"),
#       legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),
#       legend.position = "right",
#       legend.text.align = NULL,
#       legend.title.align = NULL,
#       legend.direction = "vertical",
#       legend.box = NULL,
#       # Specify panel options
#       panel.background = element_rect(fill = "black", color  =  NA),
#       panel.border = element_rect(fill = NA, color = "white"),
#       panel.grid.major = element_line(color = "grey35"),
#       panel.grid.minor = element_line(color = "grey20"),
#       # panel.margin = unit(0.5, "lines"),
#       # Specify facetting options
#       strip.background = element_rect(fill = "grey30", color = "grey10"),
#       strip.text.x = element_text(size = base_size*0.8, color = "white"),
#       strip.text.y = element_text(size = base_size*0.8, color = "white",angle = -90),
#       # Specify plot options
#       plot.background = element_rect(color = "black", fill = "black"),
#       plot.title = element_text(size = base_size*1.2, color = "white"),
#       plot.margin = unit(rep(1, 4), "lines")
#
#     )
#
# }

#' @title Draw a Region of the area under the AFROC curve
#'
#'@inheritParams fit_Bayesian_FROC
#'@inheritParams DrawCurves_MRMC_pairwise_BlackWhite
#'
#' @return None
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  fit <- fit_Bayesian_FROC(dataList.Chakra.1)
#'
#'  Draw_an_area_of_AUC_for_srsc(fit)
#'
#'
#'
#'}# dottest

Draw_an_area_of_AUC_for_srsc <- function( StanS4class){
  fit <-StanS4class
  x <-fit@plotdata$x.AFROC
  y <-fit@plotdata$y.AFROC

  x <-fit@plotdata$x.AFROC[1:( length(x) %/% 10 )]
  y <-fit@plotdata$y.AFROC[1:( length(y) %/% 10 )]



  # df$z  <- sqrt(abs(x))*sign(x)

  df <- data.frame(x,y)
# df$z  <- sqrt(abs(x))*sign(x)

  ggplot2::ggplot(df, ggplot2::aes(x=x, y=y)) +
    ggplot2::geom_line() +
    ggplot2::geom_area(ggplot2::aes(group=x, color=x))
  # +
  # theme_black()
# +
 # scale_color_gradient(low = 'blue', high = 'red')



}
