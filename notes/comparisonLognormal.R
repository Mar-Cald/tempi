rm(list=ls())

# Carica i pacchetti necessari
library(brms)
library(tidyverse)
library(bayestestR)
library(future.apply)


# Imposta i parametri
set.seed(123)

# Parameters for simulation
n = 1000  # Number of observations
a = 6.5  # Baseline intercept for the log-transformed mean (mu)
b = 0.1  # Effect of covariate X
x = rep(0:1,each=n/2)

RT = exp(a + b*x + rnorm(n,0,.5))

par(mfrow = c(1, 2))

hist(RT)
hist(log(RT))

data = tibble(rt = RT,
              log_rt = log(RT),
              group = x)

fitN = brm(log_rt ~ group, data = data, family = gaussian(),
            prior = c(prior(normal(0, 1), class = "b")),
            sample_prior = TRUE, refresh = 0)

fitLN = brm(rt ~ group, data = data, family = lognormal(),
             prior = c(prior(normal(0, 1), class = "b")),
             sample_prior = TRUE, refresh = 0)


# Funzione di simulazione
simulate_50 = function(i, n) {
  results = tibble()
  
  for (j in 1:50) {
    
    # Genera dati
    true_effect = b
    
    RT = exp(a + b*x + rnorm(n,0,.5))
    
    data = tibble(rt = RT,
                   log_rt = log(RT),
                   group = x)
    
    # Adatta i modelli Bayesiani
    fit_normal = update(fitN, newdata = data)
    fit_lognormal = update(fitLN, newdata = data)
    
    # Estrai le stime
    estimate_normal = fixef(fit_normal)["group", "Estimate"]
    estimate_lognormal = fixef(fit_lognormal)["group", "Estimate"]
    
    # Calcola il bias
    bias_normal = estimate_normal - true_effect
    bias_lognormal = estimate_lognormal - true_effect
    
    # Calcola Rss
    rss_normal = sum(bias_normal^2)
    rss_lognormal = sum(bias_lognormal^2)
    
    # Calcola i BF
    bf_normal = 1 / hypothesis(fit_normal,"group=0")$hypothesis[[6]]
    bf_lognormal = 1 / hypothesis(fit_lognormal,"group=0")$hypothesis[[6]]
    
    # Calcola le P
    p_normal = p_direction(fit_normal)[2, 2]
    p_lognormal = p_direction(fit_lognormal)[2, 2]
    
    # Calcola le previsioni medie per ciascun gruppo
    pred_normal = exp(posterior_predict(fit_normal, ndraws = 200))
    pred_lognormal = posterior_predict(fit_lognormal, ndraws = 200)
    
    # Calcola l'errore di previsione
    pred_error_normal  = rep(NA,nrow(pred_normal))
    for(i in 1:nrow(pred_normal)){
      pred_error_normal[i] = sum(abs(RT - pred_normal[i,]))
    }
    sum(pred_error_normal)
    
    pred_error_lognormal  = rep(NA,nrow(pred_lognormal))
    for(i in 1:nrow(pred_lognormal)){
      pred_error_lognormal[i] = sum(abs(RT - pred_lognormal[i,]))
    }
    sum(pred_error_lognormal)
    
    
    # Salva i risultati
    results <- bind_rows(results, tibble(
      estimate_normal, estimate_lognormal,
      bias_normal, bias_lognormal,
      rss_normal, rss_lognormal,
      bf_normal, bf_lognormal,
      p_normal, p_lognormal,
      pred_error_normal, pred_error_lognormal
    ))
  }
  return(results)
}

# Imposta il numero di core
n_cores = 10

# Imposta il backend parallelo
plan(multisession, workers = parallel::detectCores() - 2)

# Esegui le simulazioni in parallelo
start_time = Sys.time()
results = future_lapply(1:n_cores, function(i) simulate_50(i, n),
                         future.seed = TRUE)
end_time = Sys.time()
print(end_time - start_time)

# Ferma il backend parallelo
plan(sequential)

# Combina i risultati
results_df = bind_rows(results)


write_csv(results_df, file = "slides/sim_lognorm.csv")

# Riassumi i risultati
summary_results <- results_df %>%
  summarise(
    mean_estimate_normal = mean(estimate_normal),
    mean_estimate_lognormal = mean(estimate_lognormal),
    mean_bias_normal = mean(bias_normal),
    mean_bias_lognormal = mean(bias_lognormal),
    mean_rss_normal = mean(rss_normal),
    mean_rss_lognormal = mean(rss_lognormal),
    power_bf_normal = mean(bf_normal > 1),
    power_bf_lognormal = mean(bf_lognormal > 1),
    power_p_normal = mean(p_normal > .975),
    power_p_lognormal = mean(p_lognormal > .975),
    mean_pred_error_normal = mean(pred_error_normal),
    mean_pred_error_lognormal = mean(pred_error_lognormal)
  )

print(summary_results)
true_effect= b
# Visualizza la potenza
ggplot(results_df) +
  geom_density(aes(x = estimate_normal, fill = "Normal"), alpha = 0.4) +
  geom_density(aes(x = estimate_lognormal, fill = "Lognormal"), alpha = 0.4) +
  labs(title = "Confronto delle stime",
       x = "Stima dell'effetto",
       y = "Densità") +
  scale_fill_manual(values = c("Normal" = "blue", "Lognormal" = "red")) +
  geom_vline(xintercept = true_effect, linetype = "dashed", linewidth = 1)

# Grafici dei risultati
par(mfrow = c(2, 2), mar = c(5, 5, 2, 2))

plot(1:nrow(results_df), results_df$estimate_normal,
     type = 'b', pch = 16, col = 'blue', ylim = c(-0.05, 0.2),
     xlab = "Simulazione", ylab = "Stima", main = "Normale (log RT)")
abline(h = true_effect, col = "red", lwd = 2, lty = 2)

plot(1:nrow(results_df), results_df$estimate_lognormal,
     type = 'b', pch = 16, col = 'green4', ylim = c(-0.05, 0.2),
     xlab = "Simulazione", ylab = "Stima", main = "Lognormale (RT)")
abline(h = true_effect, col = "red", lwd = 2, lty = 2)

plot(1:nrow(results_df), results_df$p_normal,
     type = 'b', pch = 16, col = 'blue', ylim = c(0.5, 1),
     xlab = "Simulazione", ylab = "P-direction", main = "Normale (log RT)")
abline(h = 0.975, col = "red", lwd = 2, lty = 2)

plot(1:nrow(results_df), results_df$p_lognormal,
     type = 'b', pch = 16, col = 'green', ylim = c(0.5, 1),
     xlab = "Simulazione", ylab = "P-direction", main = "Lognormale (RT)")
abline(h = 0.975, col = "red", lwd = 2, lty = 2)


# Visualizza la distribuzione degli errori di previsione
ggplot(results_df) +
  geom_density(aes(x = pred_error_normal, fill = "Normal"), alpha = 0.4) +
  geom_density(aes(x = pred_error_lognormal, fill = "Lognormal"), alpha = 0.4) +
  labs(title = " ",
       x = "Errore di previsione",
       y = "Densità") +
  scale_fill_manual(values = c("Normal" = "blue", "Lognormal" = "red"))



ggplot(results_df) +
  geom_density(aes(x = rss_normal, fill = "Normal"), alpha = 0.4) +
  geom_density(aes(x = rss_lognormal, fill = "Lognormal"), alpha = 0.4) +
  labs(title = " ",
       x = "Errore di previsione",
       y = "Densità") +
  scale_fill_manual(values = c("Normal" = "blue", "Lognormal" = "red"))
