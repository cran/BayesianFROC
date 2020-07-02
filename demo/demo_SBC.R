
stanModel <- stan_model_of_sbc()

Simulation_Based_Calibration_single_reader_single_modality_via_rstan_sbc(
  stanModel = stanModel,
  ite       = 233,
  M         = 11,
  epsilon   = 0.04,
  BBB       = 1.1,
  AAA       = 0.0091,
  sbc_from_rstan = TRUE)
