fit <- fit_Bayesian_FROC( ite  = 61,  cha = 1, summary = T, dataList = dataList.Chakra.1 )

ppp(fit)

f <- fit_Bayesian_FROC( ite  = 61, summary = TRUE,  cha = 1, dataList = data_much_low_p_value )
ppp(f)


fit.MRMC <- fit_Bayesian_FROC( ite  = 71,  cha = 1, summary = T, dataList = dd )

ppp(fit.MRMC)




message("
# The R scripts used in the demo.


# Show an example dataset
", crayon::bgBlack$cyan$bold$italic$underline(" viewdata(dataList.Chakra.1.with.explantation)"),"



# fit our model to an example data of MRMC
", crayon::bgBlack$cyan$bold$italic$underline(" fit <- fit_Bayesian_FROC( ite  = 111, summary = TRUE,  cha=1,  dataList = dataList.Chakra.1)"),"



#   p-value in the Bayesian sence. The Null hypothesis is that the model is well fitted to data.
", crayon::bgBlack$cyan$bold$italic$underline(" ppp(fit)"),"







# fit our model to the above data
", crayon::bgBlack$cyan$bold$italic$underline(" fit.MRMC <- fit_Bayesian_FROC( ite  = 111, summary = TRUE,  cha=1,  dataList = dd)"),"



#   p-value in the Bayesian sence. The Null hypothesis is that the model is well fitted to data.
", crayon::bgBlack$cyan$bold$italic$underline(" ppp(fit.MRMC)"),"









#  Remark
", crayon::bgWhite$red$bold$italic$underline("

The complexity of calculation of ppp
depends on the number of MCMC iterations.
If the number of MCMC iterations is larger,
then the required time of calculation of ppp
is also too larger.
Thus, in this example,
the author use very small MCMC iterations
to obtain ppp in confortable time."),"








#  Remark
", crayon::bgWhite$blue$bold$italic$underline("
MRMC is a traditional abbreviation
of Multiple readers and multiple cases,
where case means imaging modality."),"




        ")




# Demo finished !!
