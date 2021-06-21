set.seed(20192020)
library(eSIR)

NI_complete <-
  c(
    41,
    41,
    41,
    45,
    62,
    131,
    200,
    270,
    375,
    444,
    549,
    729,
    1052,
    1423,
    2714,
    3554,
    4903,
    5806,
    7153,
    9074,
    11177,
    13522,
    16678,
    19665,
    22112,
    24953,
    27100,
    29631,
    31728,
    33366
  )
RI_complete <- c(
  1,
  1,
  7,
  10,
  14,
  20,
  25,
  31,
  34,
  45,
  55,
  71,
  94,
  121,
  152,
  213,
  252,
  345,
  417,
  561,
  650,
  811,
  1017,
  1261,
  1485,
  1917,
  2260,
  2725,
  3284,
  3754
)
N <- 58.5e6
R <- RI_complete / N
Y <- NI_complete / N - R

change_time <- c("01/23/2020", "02/04/2020", "02/08/2020")
pi0 <- c(1.0, 0.9, 0.5, 0.1)

res.step <- tvt.eSIR(
  Y,
  R,
  begin_str = "01/13/2020",
  T_fin = 200,
  pi0 = pi0,
  change_time = change_time,
  dic = F,
  casename = "Hubei",
  save_files = F,
  save_mcmc = T,
  save_plot_data = F,
  M = 5e2,
  nburnin = 2e2
)

# Now I load the mcmc output and process it

load("Hubei_forecast_mcmc.RData")

thetaI_band <-
  data.frame(t(apply(
    theta_pp[, , 2],
    2,
    quantile,
    probs = c(0.025, 0.975),
    na.rm = T
  )))
combined_I_matrix <- data.frame(theta_pp[, , 2])

diff_I_matrix <-
  cbind(combined_I_matrix[, 1], t(diff(t(combined_I_matrix))))
diff_I_band <-
  data.frame(t(apply(
    diff_I_matrix,
    2,
    quantile,
    probs = c(0.025, 0.975),
    na.rm = T
  )))

thetaR_band <-
  data.frame(t(apply(
    theta_pp[, , 3],
    2,
    quantile,
    probs = c(0.025, 0.975),
    na.rm = T
  )))
combined_R_matrix <- data.frame(theta_pp[, , 3])

diff_R_matrix <-
  cbind(combined_R_matrix[, 1], t(diff(t(combined_R_matrix))))
diff_R_band <-
  data.frame(t(apply(
    diff_R_matrix,
    2,
    quantile,
    probs = c(0.025, 0.975),
    na.rm = T
  )))

colnames(diff_I_band) = colnames(diff_R_band) = c("lower", "upper")

incidence_band <- diff_I_band + diff_R_band
incidence_band[incidence_band < 0] <- 0