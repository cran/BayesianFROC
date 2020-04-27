#' @title SBC
#' @description Priors should guarante suitable conditions such that the ...
#' @param stanmodel see \code{?sbc}
#' @param data To specify priors.
#' @param M The number of samples for rank statistics
#' @param iter MCMC iterations
#' @param refresh ????
#' @return ????
#' @export
#' @author  Some Stan developer, I am not sure,..., who?
#' @examples
#' \dontrun{
#'
#' stanModel <- stan_model_of_sbc()
#'
#' Simulation_Based_Calibration_single_reader_single_modality_via_rstan_sbc(
#'   ite = 233,
#'   M = 11,
#'   epsilon = 0.04,
#'   stanModel = stanModel
#' )
#'  }# dontrun
#'
#'
sbcc<-function (stanmodel, data, M,iter,refresh)
{
  stopifnot(methods::is(stanmodel, "stanmodel"))
  stan_code <- rstan::get_stancode(stanmodel)
  stan_code <- scan(what = character(), sep = "\n", quiet = TRUE,
                    text = stan_code)
  pars_lines <- grep("[[:space:]]*(pars_)|(pars_\\[.*\\])[[:space:]]*=",
                     stan_code, value = TRUE)
  pars_lines <- pars_lines[!grepl("^[[:space:]]*vector",
                                  pars_lines) & !grepl("^[[:space:]]*real", pars_lines)]
  pars_names <- trimws(sapply(strsplit(pars_lines, split = "=",
                                       fixed = TRUE), utils::tail, n = 1))
  pars_names <- unique(sub("^([a-z,A-Z,0-9,_]*)_.*;",
                           "\\1", pars_names))
  noUnderscore <- grepl(";", pars_names, fixed = TRUE)
  if (any(noUnderscore)) {
    warning(paste("The following parameters were added to pars_ but did not",
                  "have the expected underscore postfix:", paste(pars_names[noUnderscore],
                                                                 collapse = " ")))
    return()
  }
  has_log_lik <- any(grepl("log_lik[[:space:]]*;[[:space:]]*",
                           stan_code))


  post_and_modelParam <- parallel::mclapply(1:M, FUN = function(m) {
    S <- seq(from = 0, to = .Machine$integer.max, length.out = M)[m]
    out <- rstan::sampling(stanmodel, data, pars = c("ranks_",
                                              if (has_log_lik) "log_lik"),
                           include = T,
                    chains = 1L, cores = 1L, seed = S, save_warmup = FALSE,
                    thin = 1L)
    out@stanmodel <- new("stanmodel")
    return(out)
  })

 post<-  post_and_modelParam
 # class(post[[1]])
  browser()

  bad <- sapply(post, FUN = function(x) x@mode != 0)
  if (any(bad)) {
    warning(sum(bad), " out of ", M, " runs failed. Try decreasing 'init_r'")
    if (all(bad))
      stop("cannot continue")
    post <- post[!bad]
  }
  noTwin <- is.na(match(pars_names, names(post[[1]]@par_dims)))
  if (any(noTwin)) {
    warning(paste("The following underscored priors did not have a matching",
                  "parameter without the underscore:", paste(pars_names[noTwin],
                                                             collapse = " ")))
    pars_names <- NULL
  }
  pars_names <- try(flatnames(pars_names, post[[1]]@par_dims[pars_names]),
                    silent = TRUE)
  if (!is.character(pars_names)) {
    warning("parameter names could not be calculated due to non-compliance with conventions; see help(sbc)")
    pars_names <- NULL
  }
  sampler_params <- simplify2array(sapply(post, FUN = get_sampler_params,
                                          inc_warmup = FALSE))
  Y <- sapply(post, FUN = function(p) {
    means <- get_posterior_mean(p)[, 1]
    means[grepl("y_$|y_\\[.*\\]", names(means))]
  })
  pars <- sapply(post, FUN = function(p) {
    means <- get_posterior_mean(p)[, 1]
    mark <- grepl("pars_\\[[[:digit:]]+\\]", names(means))
    return(means[mark])
  })
  if (length(pars) > 0L) {
    if (is.null(dim(pars)))
      pars <- matrix(pars, nrow = 1)
    if (dim(pars)[1] != length(pars_names)) {


      warning("parameter names miscalculated due to non-compliance with conventions; see help(sbc)")


      pars_names <- NULL
    }
    if (is.null(dim(pars))) {
      pars <- t(as.matrix(pars))
    }
    rownames(pars) <- pars_names
  }
  ranks <- lapply(post, FUN = function(p) {
    r <- extract(p, pars = "ranks_", permuted = FALSE)[,
                                                       1, ]
    if (is.null(dim(r))) {
      r <- as.matrix(r)
    }
    colnames(r) <- pars_names
    r[] <- r > 0
    return(r)
  })
  if (has_log_lik)
    pareto_k <- sapply(post, FUN = function(x) suppressWarnings(loo(x))$diagnostics$pareto_k)
  out <- list(ranks = ranks,
                  Y = Y,
               pars = pars,
     sampler_params = sampler_params,
           pareto_k = if (has_log_lik) pareto_k
     )
  class(out) <- "sbc"
  return(out)
}









#' @title from rstan package
#'
#' @param names A vector of characters
#' @param dims A positive integer
#' @param col_major A logical
#' @author  Some Stan developer, I am not sure,..., who?
#' @return A vector of characters
#' @export
#'
#'@examples
#'
#'  flatnames(c("a","b"),3)
#'
#'# [1] "a[1]" "a[2]" "a[3]" "b[1]" "b[2]" "b[3]"
#'
#'
flatnames  <- function (names, dims, col_major = FALSE)
{
  if (length(names) == 1)
    return(flat_one_par(names, dims[[1]], col_major = col_major))
  nameslst <- mapply(flat_one_par, names, dims, MoreArgs = list(col_major = col_major),
                     SIMPLIFY = FALSE, USE.NAMES = FALSE)
  if (is.vector(nameslst, "character"))
    return(nameslst)
  do.call(c, nameslst)
}











#' @title  Makes array names
#'
#' @param n   A character, \code{n} is an abbreviation of \code{name}
#' @param d   A vector of integers, to be passed to \code{\link{seq_array_ind}()}
#' @param col_major   A logical, to be passed to \code{\link{seq_array_ind}()}
#' @author  Some Stan developer, I am not sure,..., who?
#' @return  a vector of characters
#' @export
#'
#' @examples
#'
#'  a<-flat_one_par("a",1:3)
#'
#'#  > a
#'#  [1] "a[1,1,1]" "a[1,1,2]" "a[1,1,3]" "a[1,2,1]" "a[1,2,2]" "a[1,2,3]"


flat_one_par<-function (n, d, col_major = FALSE)
{
  if (0 == length(d))
    return(n)
  nameidx <- seq_array_ind(d, col_major)
  names <- apply(nameidx, 1, function(x) paste(n, "[",
                                               paste(x, collapse = ","), "]", sep = ""))
  as.vector(names)
}



#' @title  Makes a Matrix from a vector of itegers
#' @description To make sbc funtion
#'
#' @param d A vector of integers
#' @param col_major A logical, whether,.... ?
#'
#' @return A matrix, dimension is \code{prod(d)} times \code{ length(d)}.
#' @export
#' @author  Some Stan developer, I am not sure,..., who?
#'
#'
#'@examples
#'
#'   a <- seq_array_ind(1:3,col_major = TRUE)
#'#> a
#'#
#'#       [,1] [,2] [,3]
#'# [1,]    1    1    1
#'# [2,]    1    2    1
#'# [3,]    1    1    2
#'# [4,]    1    2    2
#'# [5,]    1    1    3
#'# [6,]    1    2    3
#'
#'
#'
#'
#'
#'
#' b<-seq_array_ind(1:3,col_major = FALSE)
#'
#'
seq_array_ind <- function (d, col_major = FALSE)
{
  if (length(d) == 0L)
    return(numeric(0L))
  total <- prod(d)
  if (total == 0L)
    return(array(0L, dim = 0L))
  len <- length(d)
  if (len == 1L)
    return(array(1:total, dim = c(total, 1)))
  res <- array(1L, dim = c(total, len))
  if (total == 1)
    return(res)
  jidx <- if (col_major)
    1L:len
  else len:1L
  for (i in 2L:total) {
    res[i, ] <- res[i - 1, ]
    for (j in jidx) {
      if (res[i - 1, j] < d[j]) {
        res[i, j] <- res[i - 1, j] + 1
        break
      }
      res[i, j] <- 1
    }
  }
  res
}
