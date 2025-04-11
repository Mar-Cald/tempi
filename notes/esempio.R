# per esempio

miInv_brms = brm(rt ~ 1, data = datShift, 
                 family = inverse.gaussian(link = "log"))
saveRDS(miInv_brms, file = "slides/miInv_brms1.rds")

miGam_brms = brm(rt ~ 1, data = datShift, 
                 family = Gamma(link = "log"))
saveRDS(miGam_brms, file = "slides/miGam_brms1.rds")


