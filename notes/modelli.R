# Brms 

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

# Enrico-----------------------------------------------------------------
dat_read=readr::read_csv("slides/data/time-readingWordLists.csv")

#grepl(pattern = "Parole", dat_e$listType)

dat_e = dat_read[dat_read$time < 100 & 
                   grepl(pattern = "Parole", dat_read$listType), ]

sum(grepl(pattern = "AFC", dat_e$listType))

dat_e$freq = factor(c(rep("alta", 238),rep("bassa",238)))

dat_e$age = scale(dat_e$Age_months)[,1]
dat_e$grade = scale(dat_e$Grade)[,1]


mod_brms_inv = brm(time ~ freq + age  + (1 |ID), data = dat_e,
               family = inverse.gaussian(link = "log"),
               save_pars = save_pars(all = TRUE))
saveRDS(mod_brms_inv, file = "slides/modInv_e.rds")

formula = bf(time ~ freq + age + (1|ID),
             ndt ~ 1 +  (1|ID))

mod_brms_shift= brm(formula, data = dat_e,
                   family = shifted_lognormal(),iter = 6000,
                   save_pars = save_pars(all = TRUE))
saveRDS(mod_brms_shift, file = "slides/modShift_e_ndt_var.rds")


formula = bf(time ~ freq + age + (1|ID),
             ndt ~ 1)
mod_brms_shift= brm(formula, data = dat_e,
                    family = shifted_lognormal(),iter = 6000,
                    save_pars = save_pars(all = TRUE))

saveRDS(mod_brms_shift, file = "slides/modShift_e_ndt_novar.rds")


mod_brms_gam = brm(time ~ freq + age  + (1|ID), data = dat_e,
                   family = Gamma(link = "log"),
                   save_pars = save_pars(all = TRUE))
saveRDS(mod_brms_gam, file = "slides/modGam_e.rds")


# Giulia-----------------------------------------------------------------
dat_g=read_csv("slides/data/data_clean_g.csv")

formula = bf(rt ~ label + (1 + label|Exp_Subject_Id),
             ndt ~ 1 + cue +  (1 + cue|Exp_Subject_Id))

priors=c(set_prior("normal(6,2)", class = "Intercept"),
            set_prior("normal(0,1)", class = "b"),
            set_prior("normal(100,100)", class = "Intercept", dpar = "ndt"),
            set_prior("normal(0,20)", class = "b", dpar = "ndt"),
            set_prior("cauchy(0,1)", class = "sd", lb = 0))

mod_brms_shift_g= brm(formula, data = dat_g,
                       family = shifted_lognormal(link_ndt = "identity"), cores = 4,
                       iter = 2000, prior = priors,
                       save_pars = save_pars(all = TRUE))

saveRDS(mod_brms_shift_g, file = "slides/mod_brms_shift_g.rds")

