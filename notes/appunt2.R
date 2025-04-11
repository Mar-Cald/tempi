# Imposta i parametri
set.seed(123)

# Parameters for simulation
n = 1000  # Number of observations
a = 6.5  # Baseline intercept for the log-transformed mean (mu)


RT = exp(a + rnorm(n,0,.5)) + 200

data = data.frame(rt = RT,
                  log_rt = log(RT))

fitN = brm(log_rt ~ 1, data = data, family = gaussian(), refresh = 0)

fitLN = brm(rt ~ 1, data = data, family = shifted_lognormal(), refresh = 0)


saveRDS(fitN, file = "slides/fitN.rds")
saveRDS(fitLN, file = "slides/fitLN.rds")
saveRDS(data, file = "slides/data.rds")


cat(sprintf("Median real RT: %.3f\n", median(RT)))
cat(sprintf("Median predicted RT: %.3f\n", median(pred_normal)))
cat(sprintf("Error: %.3f\n", median(RT)- median(pred_normal)))
