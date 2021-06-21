# This bit of the code is just to run a toy model for illustration. Actual processing stars later.

set.seed(06162021)
library(eSIR)

India_Data = read.csv(url(
  "https://api.covid19india.org/csv/latest/case_time_series.csv"
))

# March 14 to April 13, 2020

NI_complete <- India_Data$Total.Confirmed[45:75]
RI_complete <-
  India_Data$Total.Recovered[45:75] + India_Data$Total.Deceased[45:75]

N <- 1.38e9
R <- RI_complete / N
Y <- NI_complete / N - R

res.step <- tvt.eSIR(
  Y,
  R,
  begin_str = "03/14/2020",
  T_fin = 100,
  casename = "India",
  dic = T,
  save_files = T,
  save_plot_data = T,
  save_mcmc = T,
  M = 5e2,
  nburnin = 2e2
)

# Output processing starts here.

load("India_forecast_mcmc.RData")

# I_Compartment_Draws = data.frame(N * Y_pp) # May replace Y_pp with theta_pp[, , 2] to get the true proportions
# R_Compartment_Draws = data.frame(N * R_pp) # May replace R_pp with theta_pp[, , 3] to get the true proportions

I_Compartment_Draws = data.frame(N * theta_pp[, , 2])
R_Compartment_Draws = data.frame(N * theta_pp[, , 3])

# Computing scale-free adjustment factor

adj_len = 2

# load("India_plot_data.RData")

other_plot <- plot_data_ls[[2]]
T_prime    <- other_plot[[1]]

infection_plot_ls <- plot_data_ls[[4]]
data_comp = infection_plot_ls[[3]]

removed_plot_ls <- plot_data_ls[[5]]
data_comp_R = removed_plot_ls[[3]]

NI_complete <- India_Data %>% filter(Date_YMD <= "2021-02-28" & Date_YMD >= (as.Date("2021-02-28") - 99)) %>% pull(Total.Confirmed)

adj_v <-
  mean(as.vector(NI_complete[(T_prime - adj_len):T_prime]) / N / (data_comp[(T_prime -
                                                                               adj_len):T_prime, "mean"] + data_comp_R[(T_prime - adj_len):T_prime, "mean"]))

I_Compartment_Draws = data.frame(I_Compartment_Draws * adj_v)
R_Compartment_Draws = data.frame(R_Compartment_Draws * adj_v)

# Daily new cases

Diff_I_Draws = data.frame(cbind(
  I_Compartment_Draws[, 1] - infection_plot_ls$data_comp$mean[T_prime] * N,
  t(diff(t(
    I_Compartment_Draws
  )))
))

Diff_R_Draws = data.frame(cbind(
  R_Compartment_Draws[, 1] - removed_plot_ls$data_comp_R$mean[T_prime] * N,
  t(diff(t(
    R_Compartment_Draws
  )))
))

Daily_New_Draws = Diff_I_Draws + Diff_R_Draws

Daily_New_Summary = data.frame(
  lower = apply(
    Daily_New_Draws,
    2,
    quantile,
    probs = 0.025,
    na.rm = T
  ),
  median = apply(
    Daily_New_Draws,
    2,
    quantile,
    probs = 0.500,
    na.rm = T
  ),
  upper = apply(
    Daily_New_Draws,
    2,
    quantile,
    probs = 0.975,
    na.rm = T
  ),
  mean = apply(Daily_New_Draws,
               2,
               mean,
               na.rm = T)
)

# Total cases during a period

T_Pred = 30 # Change to period length of interest accordingly

Pred_Total_Period = colSums(Daily_New_Summary[1:T_Pred, , drop = F])

# Daily new deaths

CFR = rep(0.01, T_Pred) # Change to CFR schedule of interest

Daily_Deaths_Draws = data.frame(t(t(Daily_New_Draws[, 1:T_Pred]) * CFR))

Daily_Deaths_Summary = data.frame(
  lower = apply(
    Daily_Deaths_Draws,
    2,
    quantile,
    probs = 0.025,
    na.rm = T
  ),
  median = apply(
    Daily_Deaths_Draws,
    2,
    quantile,
    probs = 0.500,
    na.rm = T
  ),
  upper = apply(
    Daily_Deaths_Draws,
    2,
    quantile,
    probs = 0.975,
    na.rm = T
  ),
  mean = apply(Daily_Deaths_Draws,
               2,
               mean,
               na.rm = T)
)

# Total deaths during a period

Deaths_Total_Period = colSums(Daily_Deaths_Summary[1:T_Pred, , drop = F])

# Save results

save(
  Daily_New_Summary,
  Pred_Total_Period,
  Daily_Deaths_Summary,
  Deaths_Total_Period,
  file = "Results.rda"
)
