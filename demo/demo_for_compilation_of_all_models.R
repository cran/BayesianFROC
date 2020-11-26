
f <- fit_Bayesian_FROC( ite  = 31,  cha = 1, dataList = d, multinomial = FALSE )


f <- fit_Bayesian_FROC( ite  = 31,  cha = 1, dataList = d, multinomial = TRUE )


f <- fit_Bayesian_FROC( ite  = 31,  cha = 1, dataList = dd )


f <- fit_Bayesian_FROC( ite  = 31,  cha = 1, dataList = ddddd )


# This SBC model is not correct in some sense.
stanModel <- stan_model_of_sbc(model_ver = )

Simulation_Based_Calibration_single_reader_single_modality_via_rstan_sbc(
  stanModel = stanModel,
  ite     = 233, NI=1111, NL=1111,
  M       = 1111,
  epsilon = 0.04,BBB = 1.1,AAA =0.0091,sbc_from_rstan = TRUE)
