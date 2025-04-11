# Brms dat Enrico

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
mod_brms_shift
plot(mod_brms_shift)

mod_brms_gam = brm(time ~ freq + age  + (1|ID), data = dat_e,
                   family = Gamma(link = "log"),
                   save_pars = save_pars(all = TRUE))
saveRDS(mod_brms_gam, file = "slides/modGam_e.rds")


# Giulia
dat_g=read_csv("slides/data/data_clean_g.csv")
dat_g = dat_g[dat_g$accuracy == 1,]|>na.omit()

dat_g$rt = dat_g$reaction_time
dat_g$cue = as.factor(dat_g$cue)
dat_g$label = as.factor(dat_g$label)
contrasts(dat_g$cue) = contr.sum(2)/2
contrasts(dat_g$cue)
contrasts(dat_g$label) = contr.sum(2)/2

#plot of density distribution for the 3 factors: CUE, MATCH and LABEL
ggdensity(dat_g, x = "reaction_time",
          add = "median", rug = TRUE,
          color = c("label"), palette = c("black", "red"))

ggdensity(dat_g, x = "reaction_time",
          add = "median", rug = TRUE,
          color = c("cue"), palette = c("black", "red"))


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

summary(mod_brms_shift_g)

pp_check(mod_brms_shift_g, nsamples = 100, type = "dens_overlay_grouped", 
         group = "cue")

pp_check(mod_brms_shift_g, nsamples = 100, type = "dens_overlay_grouped", 
         group = "label")

m = summary(mG_brms)
df = data.frame(Intercept = round(m$coefficients[1],2),
                Freq_effect = round(summary(mG_brms)$coefficients[2],2),
                Age_effect = round(summary(mG_brms)$coefficients[3],2))

amlss(R~cs(Fl),family=IG, data=rent)

install.packages('gamlss')
library(gamlss)


mInv = gamlss::gamlss(time ~ freq  + age, 
                   data = na.omit(dat_e), family = IG)
