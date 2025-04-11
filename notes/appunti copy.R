
rm(list = ls())

dat_read=read_csv("slides/data/time-readingWordLists.csv")

#grepl(pattern = "Parole", dat_e$listType)

dat_e = dat_read[dat_read$time < 100 & 
                   grepl(pattern = "Parole", dat_read$listType), ]

sum(grepl(pattern = "AFC", dat_e$listType))

dat_e$freq = factor(c(rep("alta", 238),rep("bassa",238)))
       
dat_e$age = scale(dat_e$Age_months)[,1]
dat_e$grade = scale(dat_e$Grade)[,1]


#plot of density distribution for the 3 factors: CUE, MATCH and LABEL
ggdensity(dat_e, x = "time",
          add = "median", rug = TRUE,
          color = c("freq"), palette = c("black", "red"))



mod_brms_inv = brm(time ~ freq + age  + (1 |ID), data = dat_e,
               family = inverse.gaussian(link = "log"))
saveRDS(mod_brms_inv, file = "modInv_e.rds")

summary(mod_brms_inv)
pp_check(mod_brms_inv, ndraws = 50)

miInv = lme4::glmer(time ~ freq + age  + (1|ID), 
                    data = dat_e, family = inverse.gaussian(link = "log"))

summary(miInv)
AIC(miInv)

miGam = lme4::glmer(time ~ freq + age + (1|ID), data = dat_e, family = Gamma(link = "log") )
performance::performance(miGam)
performance::check_model(miGam)
AIC(miGam)

dat_e$age == 0


mod_brms_gam = brm(time ~ freq + age + grade + (1|ID), data = dat_e,
               family = Gamma(link = "log"))

saveRDS(mod_brms_gam, file = "modGam_e.rds")

fitInv <- add_criterion(mod_brms_inv, "loo")
fitGam <- add_criterion(mod_brms_gam, "loo")

loo_compare(fitInv,fitGam)

pp_check(mod_brms_gam, ndraws = 50)


miInv = lme4::glmer(time ~ freq + age + (1|ID), data = dat_e, family = inverse.gaussian(link = "log"))
miGam = lme4::glmer(time ~ freq + age + (1|ID), data = dat_e, family = Gamma(link = "log") )
milognorm = glmmTMB::glmmTMB(time ~ freq + age + grade + (1|ID), data = dat_e, family = lognormal)



performance::check_predictions(miInv,type = "discrete_both")
performance::check_predictions(miGam,type = "discrete_both", iterations = 200)
performance::check_predictions(milognorm,type = "discrete_both", iterations = 200)

lognormal

dat_g=read_csv("data/dat_g_word.csv")

dat_g$reaction_time = dat_g$reaction_time/1000
               
#plot of density distribution for the 3 factors: CUE, MATCH and LABEL
ggdensity(dat_g, x = "reaction_time",
          add = "median", rug = TRUE,
          color = c("cue"), palette = c("black", "red"))


miInv = lme4::glmer(reaction_time ~ 1 + cue + target_match + (1 + cue + target_match|Exp_Subject_Id), 
                    data = dat_g, family = inverse.gaussian(link = "log"))
miGam = lme4::glmer(reaction_time ~ 1 + cue + target_match + (1 + cue + target_match|Exp_Subject_Id), 
                    data = dat_g, family = Gamma(link = "log") )


AIC(miInv)
AIC(miGam)

performance::check_predictions(miInv,type = "discrete_both")
performance::check_predictions(miGam,type = "discrete_both")


summary(miInv)
summary(miGam)
