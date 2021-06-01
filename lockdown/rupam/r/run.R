library(rjags)
library(gtools)
library(ggplot2)
library(chron)
library(VGAM)
library(here)
library(glue)

devtools::install_github("lilywang1988/eSIR")

library(eSIR)
f <- list.files(here("lockdown", "rupam", "src"))
for (i in seq_along(f)) {source(glue("{here('lockdown', 'rupam', 'src')}/{f[i]}"))}
# source("utils_functions.R")
# source("tvt.eSIR.R")

NI_complete <- c(
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
N = 58.5e6
R <- RI_complete / N
Y <- NI_complete / N - R #Jan13->Feb 11
### Step function of pi(t)
change_time <- c("01/23/2020", "02/04/2020", "02/08/2020")
pi0 <- c(1.0, 0.9, 0.5, 0.1)
### Rayleigh survival for omega(t), can be changed to a step function
rayleigh = TRUE
sigma0 = 310
res.step <-
  tvt.eSIR(
    Y,
    R,
    begin_str = "01/13/2020",
    death_in_R = 0.4,
    T_fin = 200,
    pi0 = pi0,
    change_time = change_time,
    rayleigh = rayleigh,
    sigma0 = sigma0,
    dic = F,
    casename = "Hubei_step",
    save_files = F,
    save_mcmc = F,
    M = 5e2,
    nburnin = 2e2
  )