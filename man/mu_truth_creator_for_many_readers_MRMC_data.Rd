% Generated by roxygen2: do not edit by hand
% Please edit documentation in
%   R/validation_MRMC_Create_dataList_MRMC_Hit_from_rate_etc.R
\name{mu_truth_creator_for_many_readers_MRMC_data}
\alias{mu_truth_creator_for_many_readers_MRMC_data}
\title{mu of MRMC model paramter}
\usage{
mu_truth_creator_for_many_readers_MRMC_data(M, Q)
}
\arguments{
\item{M}{An integer, indicating a number of modalities}

\item{Q}{An integer, indicating a number of readers}
}
\value{
An array, representing a mu of MRMC model paramter
}
\description{
mu of MRMC model paramter
}
\examples{


    m <- mu_truth_creator_for_many_readers_MRMC_data(M=4,Q=50)



\dontrun{

#========================================================================================
#          Large number of readers or modalities causes non-convergence MCMC
#========================================================================================


  v <- v_truth_creator_for_many_readers_MRMC_data(M=4,Q=6)
m <- mu_truth_creator_for_many_readers_MRMC_data(M=4,Q=6)
d <-create_dataList_MRMC(mu.truth = m,v.truth = v)
#fit <- fit_Bayesian_FROC( ite  = 1111,  cha = 1, summary = TRUE, dataList = d )

#plot_FPF_and_TPF_from_a_dataset(fit@dataList)




#========================================================================================
#                             convergence
#========================================================================================




 v  <- v_truth_creator_for_many_readers_MRMC_data(M=2,Q=21)
 m  <- mu_truth_creator_for_many_readers_MRMC_data(M=2,Q=21)
 d  <- create_dataList_MRMC(mu.truth = m,v.truth = v)
#fit <- fit_Bayesian_FROC( ite  = 200,  cha = 1, summary = TRUE, dataList = d)


#========================================================================================
#                            non-convergence
#========================================================================================



v  <- v_truth_creator_for_many_readers_MRMC_data(M=5,Q=6)
 m  <- mu_truth_creator_for_many_readers_MRMC_data(M=5,Q=6)
 d  <- create_dataList_MRMC(mu.truth = m,v.truth = v)
#  fit <- fit_Bayesian_FROC( ite  = 111,  cha = 1, summary = TRUE, dataList = d)



#========================================================================================
#                           convergence
#========================================================================================


v  <- v_truth_creator_for_many_readers_MRMC_data(M=1,Q=36)
m  <- mu_truth_creator_for_many_readers_MRMC_data(M=1,Q=36)
d  <- create_dataList_MRMC(mu.truth = m,v.truth = v)
#fit <- fit_Bayesian_FROC( ite  = 2000,  cha = 1, summary = TRUE, dataList = d)










#========================================================================================
#                            non-convergence
#========================================================================================


v  <- v_truth_creator_for_many_readers_MRMC_data(M=1,Q=37)
m  <- mu_truth_creator_for_many_readers_MRMC_data(M=1,Q=37)
d  <- create_dataList_MRMC(mu.truth = m,v.truth = v)
# fit <- fit_Bayesian_FROC( ite  = 111,  cha = 1, summary = TRUE, dataList = d)







}




}
