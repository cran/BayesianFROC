# # @title stan_trace() is modified to be dark theme
# # @inheritParams DrawCurves
# # @param pars
# # @param include
# # @param unconstrain
# # @param inc_warmup
# # @param nrow
# # @param ncol
# # @param ...
# # @param window
# #
# # @return none
# # @export
# #
# # @examples
# #
# #
# stan_traCe_dark_theme <- function (StanS4class,
#                                    pars = name_of_param_whose_Rhat_is_maximal(StanS4class),
#                                    include = TRUE, unconstrain = FALSE,
#           inc_warmup = FALSE, nrow = NULL, ncol = NULL, ..., window = NULL)
# {
#
#   object <- stanfit_from_its_inherited_class(StanS4class)
#   rstan:::.check_object(object, unconstrain)
#   plot_data <- rstan:::.make_plot_data(object, pars, include, inc_warmup,
#                                unconstrain)
#     thm <- rstan:::rstanvis_theme()
#
#
#   clrs <- rep_len(rstan:::rstanvis_aes_ops("chain_colors"), plot_data$nchains)
#   base <- ggplot2::ggplot(plot_data$samp, ggplot2::aes_string(x = "iteration",
#                                                               y = "value", color = "chain"))
#   if (inc_warmup)
#     base <- base + ggplot2::annotate("rect", xmin = -Inf,
#                                      xmax = plot_data$warmup, ymin = -Inf, ymax = Inf,
#                                      fill = rstan:::rstanvis_aes_ops("grays")[2L])
#   graph <- base + ggplot2::geom_path(...) + ggplot2::scale_color_manual(values = clrs) +
#     thm+
#      ggplot2::theme_dark()+
#     # ggplot2::labs(x = "", y = "")   +
#     ggplot2::theme(text = ggplot2::element_text(family = "YuGo-Medium"),
#                    plot.background = ggplot2::element_rect(fill = "black"))
#
#   if (plot_data$nparams == 1)
#     graph <- graph + ggplot2::ylab(unique(plot_data$samp$parameter))
#   else graph <- graph + ggplot2::facet_wrap(~parameter, nrow = nrow,
#                                             ncol = ncol, scales = "free")
#   if (!is.null(window)) {
#     if (!is.numeric(window) || length(window) != 2)
#       stop("'window' should be a numeric vector of length 2.")
#     graph <- graph + ggplot2::coord_cartesian(xlim = window)
#   }
#   graph
# }
