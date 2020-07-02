f <- fit_Bayesian_FROC( ite  = 1111, #The number of iterations of MRMC
                        summary = TRUE,
                        cha = 3, #The number of chains of MRMC
                        dataList = dataList.Chakra.1 , #A dataset to be fitted a model
                        multinomial = TRUE #By TRUE, the classical 1989 Chakraborty's model is fitted to a dataList
                        )
