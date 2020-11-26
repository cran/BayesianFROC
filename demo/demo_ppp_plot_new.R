

f <- fit_Bayesian_FROC( ite  = 333, summary = TRUE,  cha = 1, dataList = dataList.Chakra.1 )

plot_dataset_of_ppp(f)

f <- fit_Bayesian_FROC( ite  = 111, summary = TRUE,  cha = 1, dataList = dd )

plot_dataset_of_ppp_MRMC(f)
