
#' @title Scatter Plot of FPFs and TPFs via Splitting Factor
#' @description Make a factor vector by which we plot FPF and TPF.
#' @param cex A positive real number, specifying the size of dots in the resulting plot.
#' @param dataList.MRMC A list, indicating FROC data of MRMC.
#' See also \code{dataList} which is a variable of the function \code{\link{fit_Bayesian_FROC}()}.
#' @param colored_by_modality A logical, if TRUE, then the color in the scatter plot means modality ID.
#'  If not, then the each color in the scatter plot indicates reader ID.
#'
#' @param numbered_by_modality A logical, if TRUE, then the number in the scatter plot means modality ID.
#'  If not, then the each number in the scatter plot indicates reader ID.
#' @inheritParams fit_Bayesian_FROC
#' @return A dataframe, which is added TPF and FPF, etc into \code{dataList.MRMC}.
#'
#'\strong{\emph{Added Vectors as Contents of the Data-frame}}
#'\describe{
#'\item{ \code{CFP}    }{ A vector of \strong{\emph{Cumulative False Positive}}  }
#'\item{ \code{CTP}    }{ A vector of \strong{\emph{Cumulative True Positive }}  }
#'\item{ \code{TPF}    }{ A vector of \strong{\emph{True Positive Fraction   }}  }
#'\item{ \code{FPF}    }{ A vector of \strong{\emph{False Positive Fraction  }} per image or per lesion according to the logical variable \code{ModifiedPoisson}  }
#'\item{ \code{factor}    }{What this means is trivial.}

#'}
#'\strong{\emph{Vectors as Contents of the Data-frame \code{dataList.MRMC}}}
#'
#' \describe{
#' \item{ \code{c }  }{A vector of positive integers,  representing  the \emph{\strong{confidence level}}. This vector must be made by \code{rep(rep(C:1), M*Q)} }
#' \item{ \code{m }  }{A vector of positive integers,  representing  the \emph{\strong{modality}} ID vector. }
#' \item{ \code{q }  }{A vector of positive integers,  representing  the \emph{\strong{reader}} ID vector.}
#' \item{ \code{h }  }{A vector of non-negative integers,  representing  the number of \emph{\strong{hits}}.   }
#' \item{ \code{f }  }{A vector of non-negative integers,  representing  the number of \emph{\strong{false alarm}}.  }
#'  }
#'
#'
#' @export
# examples -----
#' @examples
#'
#'#========================================================================================
#'#                               The 1st example
#'#========================================================================================
#'
#'
#' v  <- v_truth_creator_for_many_readers_MRMC_data(M=1,Q=37)
#' m  <- mu_truth_creator_for_many_readers_MRMC_data(M=1,Q=37)
#' d  <- create_dataList_MRMC(mu.truth = m,v.truth = v)
#'
#' plot_FPF_TPF_via_dataframe_with_split_factor(d,
#'   colored_by_modality    = TRUE,
#'   numbered_by_modality   = TRUE)
#'
#' plot_FPF_TPF_via_dataframe_with_split_factor(d,
#'   colored_by_modality   = FALSE,
#'   numbered_by_modality   = TRUE)
#'
#' plot_FPF_TPF_via_dataframe_with_split_factor(d,
#'   colored_by_modality    = TRUE,
#'   numbered_by_modality  = FALSE)
#'
#'
#' plot_FPF_TPF_via_dataframe_with_split_factor(d,
#'   colored_by_modality   = FALSE,
#'   numbered_by_modality  = FALSE)
#'
#'#========================================================================================
#'#                               The 2-nd example
#'#========================================================================================
#'#
#'
#'
#' v  <- v_truth_creator_for_many_readers_MRMC_data(M=2,Q=37)
#' m  <- mu_truth_creator_for_many_readers_MRMC_data(M=2,Q=37)
#' d  <- create_dataList_MRMC(mu.truth = m,v.truth = v)
#'
#' plot_FPF_TPF_via_dataframe_with_split_factor(d,
#'   colored_by_modality    = TRUE,
#'   numbered_by_modality   = TRUE)
#'
#' plot_FPF_TPF_via_dataframe_with_split_factor(d,
#'   colored_by_modality   = FALSE,
#'   numbered_by_modality   = TRUE)
#'
#' plot_FPF_TPF_via_dataframe_with_split_factor(d,
#'   colored_by_modality    = TRUE,
#'   numbered_by_modality  = FALSE)
#'
#'
#' plot_FPF_TPF_via_dataframe_with_split_factor(d,
#'   colored_by_modality   = FALSE,
#'   numbered_by_modality  = FALSE)
#'
#'
#'
#'#========================================================================================
#'#                               The 3rd example
#'#========================================================================================
#'
#'
#'
#'
#' v  <- v_truth_creator_for_many_readers_MRMC_data(M=3,Q=7)
#' m  <- mu_truth_creator_for_many_readers_MRMC_data(M=3,Q=7)
#' d  <- create_dataList_MRMC(mu.truth = m,v.truth = v)
#'
#' plot_FPF_TPF_via_dataframe_with_split_factor(d,
#'   colored_by_modality    = TRUE,
#'   numbered_by_modality   = TRUE)
#'
#' plot_FPF_TPF_via_dataframe_with_split_factor(d,
#'   colored_by_modality   = FALSE,
#'   numbered_by_modality   = TRUE)
#'
#' plot_FPF_TPF_via_dataframe_with_split_factor(d,
#'   colored_by_modality   = TRUE,
#'   numbered_by_modality  = FALSE)
#'
#'
#' plot_FPF_TPF_via_dataframe_with_split_factor(d,
#'   colored_by_modality   = FALSE,
#'   numbered_by_modality  = FALSE)

#'#========================================================================================
#'#                               The 4th example
#'#========================================================================================
#'
#'
#'
#'
#' plot_FPF_TPF_via_dataframe_with_split_factor( dataList.MRMC = dd,
#'                                               colored_by_modality  = TRUE,
#'                                               numbered_by_modality = TRUE)
#'
#'
#'
#'#========================================================================================
#'#                               The 5th example
#'#========================================================================================
#'
#'
#' a <- plot_FPF_TPF_via_dataframe_with_split_factor(dd)
#'
#' p <- ggplot2::ggplot(a, ggplot2::aes(FPF, TPF,
#'                             group = factor(factor),
#'                             colour = factor(m)) ) +
#'    ggplot2::geom_line(size = 1.4)
#' print(p)
#'
#'
#'
#'
#'
#'#========================================================================================
#'#                               The 6th example
#'#========================================================================================
#'
#' a <- plot_FPF_TPF_via_dataframe_with_split_factor(dd,cex = 1.8)
#'
#'
#'#========================================================================================
#'#                               The 7th example
#'#========================================================================================
#'
#'
#' # Plot empirical FROC curve whose modality is specified as following manner
#'
#' a <- plot_FPF_TPF_via_dataframe_with_split_factor(dd)
#' aa <- a[a$m == c(2,3), ]
#'
#' p <- ggplot2::ggplot(aa, ggplot2::aes(FPF, TPF,
#'                             group = factor(factor),
#'                             colour = factor(m)) ) +
#'    ggplot2::geom_line(size = 1.4)
#' print(p)
#'
#'
#'
#' # Plot empirical FROC curve whose modality is specified as following manner
#'
#' a <- plot_FPF_TPF_via_dataframe_with_split_factor(dd)
#' aa <- a[a$m %in%  c(4,3), ]
#'
#' p <- ggplot2::ggplot(aa, ggplot2::aes(FPF, TPF,
#'                             group = factor(factor),
#'                             colour = factor(m)) ) +
#'    ggplot2::geom_line(size = 1.4)
#' print(p)
#'
#'
#' # Plot empirical FROC curve whose modality is specified as following manner
#'
#' a <- plot_FPF_TPF_via_dataframe_with_split_factor(dd)
#' aa <- a[a$m %in% c(3,4), ]
#'
#' p <- ggplot2::ggplot(aa, ggplot2::aes(FPF, TPF,
#'                             group = factor(factor),
#'                             colour = factor(m)) ) +
#'    ggplot2::geom_line(size = 1.4)
#' print(p)
#'
#'
#'
#'
plot_FPF_TPF_via_dataframe_with_split_factor <- function(
  dataList.MRMC ,
  ModifiedPoisson =FALSE,
  colored_by_modality =TRUE,
  numbered_by_modality =TRUE,
  cex =1.3

){


M <- dataList.MRMC$M
Q <- dataList.MRMC$Q
C <- dataList.MRMC$C
m <- dataList.MRMC$m
q <- dataList.MRMC$q
c <- dataList.MRMC$c
h <- dataList.MRMC$h
f <- dataList.MRMC$f
NI <- dataList.MRMC$NI
NL <- dataList.MRMC$NL
names(h) <-NULL
names(f) <-NULL
names(m) <-NULL
names(q) <-NULL
names(c) <-NULL
names(M) <-NULL
names(Q) <-NULL
names(C) <-NULL
names(NI) <-NULL
names(NI) <-NULL


if(ModifiedPoisson==FALSE){ NX <- NI;
                           xlabb <- " per image"}
if(ModifiedPoisson==TRUE){ NX <- NL;
                           xlabb <- " per lesion"}

 factor <-rep(1:(M*Q),1,each =C)
 h<-data.frame(h=h)
 h<- split(x=h, f=factor)
 CTP <- lapply(h, cumsum)
 CTP <- unlist(CTP)
 TPF <- unlist(CTP)/NL


 f<-data.frame(f=f)
 f<- split(x=f, f=factor) # Very caution ----
 CFP <- lapply(f, cumsum)
 CFP <- unlist(CFP)
 FPF <- unlist(CFP)/NX
 names(CTP) <-NULL
 names(CFP) <-NULL
 names(TPF) <-NULL
 names(FPF) <-NULL

 df <- data.frame(
   m=dataList.MRMC$m,
   q=dataList.MRMC$q,
   c=dataList.MRMC$c,
   # h = h,
   # f = f,
   # NI = dataList.MRMC$NI,
   # NL = dataList.MRMC$NL,
   CTP=CTP,
   CFP=CFP,
   TPF=TPF,
   FPF=FPF,
   factor=factor
 )
 # dark_theme()

 if(colored_by_modality){ col <- m;
                         col.main <- "modality"}
 if(!colored_by_modality) {col <-q
                         col.main <- "reader" }

 if(numbered_by_modality) {number <- m
                         num.main <- "modality"}
 if(!numbered_by_modality){ number <-q
                         num.main <- "reader"}

 main <- paste("Color =", col.main, ",  Number = ",num.main )
 xlab = paste("FPF",xlabb)
 with(df, plot(FPF, TPF, type="n",main =main,xlab = xlab))
 with(df, text(FPF, TPF, number, col=col, cex=cex))

# To plot empiciral FROC, the author should add zeros in FPF and TPF. 2020 Mar.


 interval.length <- C
 co_interval.length <- length(FPF)/interval.length
 FPF.ext <- as.vector(t(cbind(0, matrix(FPF, co_interval.length, byrow=T))))
 TPF.ext <- as.vector(t(cbind(0, matrix(TPF, co_interval.length, byrow=T))))
 e <- m_q_c_vector_from_M_Q_C(M,Q,C+1)
 m <- e$m
 q <- e$q
 c <- e$c

  factor <-rep(1:(M*Q),1,each =C+1)
 # browser()

 df.extended <- data.frame(
    m=m,
    q=q,
    c=c,
    TPF=TPF.ext,
    FPF=FPF.ext,
    factor=factor
 )

# knitr::kable(df)
# browser()

 return(df.extended)
  }#function


# as.vector(t(cbind(0, matrix(x, interval.length, byrow=T))))


#' @title Plot empirical FROC Curves by traditional ways of \pkg{ggplot2}
#' @description  Plot empirical FROC Curves.
#' @inheritParams plot_FPF_TPF_via_dataframe_with_split_factor
#' @param modalityID A vector of integer, specifying modality ID to be drawn.
#' @param readerID A vector of integer, specifying modality ID to be drawn.

#' @return An object made by ggplot2, I am not sure what it is.
#' @export
#'
#' @examples
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#                               The 1-st example
#'#========================================================================================
#'
#'
#' plot_empirical_FROC_curves(dd,readerID = 1:4,modalityID = 1:5)
#'plot_empirical_FROC_curves(dd,readerID = 1,modalityID = c(4,3))
#'plot_empirical_FROC_curves(dd,readerID = 2,modalityID = c(4,3))
#'plot_empirical_FROC_curves(dd,readerID = 3,modalityID = c(4,3))
#'plot_empirical_FROC_curves(dd,readerID = 4,modalityID = c(4,3))
#'
#'
#'
#'
#'
#'
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#                               The  example
#'#========================================================================================
#'
#'
#'      v  <- v_truth_creator_for_many_readers_MRMC_data(M=2,Q=37)
#'  m  <- mu_truth_creator_for_many_readers_MRMC_data(M=2,Q=37)
#'  d  <- create_dataList_MRMC(mu.truth = m,v.truth = v)
#'
#'
#'     plot_empirical_FROC_curves(d,readerID = 1:14,modalityID = 1:2)
#'
#'
#'     plot_empirical_FROC_curves(d,readerID = 1:24,modalityID = 1:2)
#'
#'
#'     plot_empirical_FROC_curves(d,readerID = 1:34,modalityID = 1:2)
#'
#'
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#                               The  example
#'#========================================================================================
#'
#'
#'
#' v  <- v_truth_creator_for_many_readers_MRMC_data(M=2,Q=7)
#' m  <- mu_truth_creator_for_many_readers_MRMC_data(M=2,Q=7)
#' d  <- create_dataList_MRMC(mu.truth = m,v.truth = v)
#'
#'
#' plot_empirical_FROC_curves(d,readerID = 1,modalityID = 1:2)
#' plot_empirical_FROC_curves(d,readerID = 2,modalityID = 1:2)
#' plot_empirical_FROC_curves(d,readerID = 3,modalityID = 1:2)
#' plot_empirical_FROC_curves(d,readerID = 4,modalityID = 1:2)
#' plot_empirical_FROC_curves(d,readerID = 5,modalityID = 1:2)
#' plot_empirical_FROC_curves(d,readerID = 6,modalityID = 1:2)
#' plot_empirical_FROC_curves(d,readerID = 7,modalityID = 1:2)
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#                               The  example
#'#========================================================================================
#'
#'
#' plot_empirical_FROC_curves(dd)
#' plot_empirical_FROC_curves(dd,modalityID = c(3,5))
#' plot_empirical_FROC_curves(dd,modalityID = c(3,5),readerID = c(1,4))
#' plot_empirical_FROC_curves(dd,modalityID = c(3,5),readerID = c(1,3))
#' plot_empirical_FROC_curves(dd,modalityID = c(3,5),readerID = c(1,2,3))
#' plot_empirical_FROC_curves(dd,modalityID = c(3,5),readerID = c(1,2,3))
#' plot_empirical_FROC_curves(dd,modalityID = c(3,5),readerID = c(3))
#' plot_empirical_FROC_curves(dd,modalityID = c(3,5),readerID = c(1,2,3))
#' plot_empirical_FROC_curves(dd,modalityID = c(3,5),readerID = c(1,2))
#' plot_empirical_FROC_curves(dd,modalityID = c(3,5),readerID = c(2))
#' plot_empirical_FROC_curves(dd,modalityID = c(3),readerID = c(2))
#' plot_empirical_FROC_curves(dd,modalityID = c(5),readerID = c(2))
#'
#'
#'
#'
#'
#'
#'
#'
#'
plot_empirical_FROC_curves <- function(
   dataList.MRMC ,
   ModifiedPoisson =FALSE,
   colored_by_modality =TRUE,
   numbered_by_modality =TRUE,
   cex =1.3,
   modalityID =c(1, dataList.MRMC$M),
   readerID =c(1, dataList.MRMC$Q)


) {

   if(ModifiedPoisson) xlabel <- "False Positive Fraction per lesion"
   if(!ModifiedPoisson) xlabel <- "False Positive Fraction per image"

   a <- plot_FPF_TPF_via_dataframe_with_split_factor(
               dataList.MRMC =dataList.MRMC ,
               ModifiedPoisson =ModifiedPoisson,
               colored_by_modality =colored_by_modality,
               numbered_by_modality =numbered_by_modality,
               cex =cex
            )

   aa <- a[a$m %in% modalityID, ]
   aa <- aa[aa$q %in% readerID, ]


   upper.lim.x <- max(a$FPF)
   lower.lim.x <- min(a$FPF)

   upper.lim.y <- max(a$TPF)
   lower.lim.y <- min(a$TPF)

   p <- ggplot2::ggplot(aa, ggplot2::aes(aa$FPF, aa$TPF,
                                group = factor(aa$factor),
                                colour = factor(aa$m)) ) +
      ggplot2::geom_line(size = 1.4)+
      ggplot2::xlim(lower.lim.x,upper.lim.x)+
      ggplot2::ylim(lower.lim.y,upper.lim.y)+

      ggplot2::labs(
         # subtitle="Colored by modality ID.",
           y="True Positive Fraction",
           x= xlabel,
           title="Empirical (observed) FROC Curve (Colored by modality)"
           # caption = "Source: midwest"
           ) +

      ggplot2::theme(
         axis.title.x = ggplot2::element_text(size = 12),#label name
         axis.title.y = ggplot2::element_text(size = 12),#label name
         axis.text.x = ggplot2::element_text(size = 22),
         axis.text.y = ggplot2::element_text(size = 22),
         # panel.background = ggplot2::element_rect(fill = 'darkgray', colour = 'red'),
         plot.background = ggplot2::element_rect(fill = "gray"),
#          axis.text =ggplot2::element_text(size=12),
#          axis.title=ggplot2::element_text(size=33,face="bold"),
         legend.title = ggplot2::element_text(color = "blue", size = 10,face="bold"),
         legend.text = ggplot2::element_text(color = "black", size = 20,face="bold")
         )
      # theme_grey(
      #    base_size = 11,
      #    base_family = "",
      #    base_line_size = 11/22,
      #    base_rect_size = 11/22
      # )
   # p <- p + guides(colour=guide_legend(title="Modality ID"))
   p$labels$colour <- "Modality"

   print(p)


}
