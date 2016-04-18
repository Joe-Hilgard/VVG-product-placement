# Tables
load(file = "Study_results.Rdata")

library(plyr)
library(dplyr)
library(broom)

# Supplementary Table 1
gamma1export = cbind("Outcome" = "Gun-owner accident", tidy(gamma1))
gamma2export = cbind("Outcome" = "Gun stolen", tidy(gamma2))
gamma3export = cbind("Outcome" = "Use in self-defense", tidy(gamma3))
gamma4export = cbind("Outcome" = "Non-owner accident", tidy(gamma4))
# Consider renaming terms
#tg1$term = c("(Intercept)", "AR-15", "Strong-gun", "Republican", 
#"Moderate", "Libertarian", "Other-pol", "Male", "Strong_AR-15")

round4 = function(x) round(x, digits = 4)
sup.table1 = bind_rows(gamma1export, gamma2export, gamma3export, gamma4export) %>% 
  mutate_each(funs(round4), estimate:p.value) %>% 
  mutate(term = mapvalues(term, 
                          from = c("(Intercept)", "Gun_type_fAR-15", "Power_fStrong",
                                   "pol_orien_fRep", "pol_orien_fMod", "pol_orien_fLib",
                                   "pol_orien_fOther", "Gender_fMale", 
                                   "Gun_type_fAR-15:Power_fStrong"), 
                          to = c("(Intercept)", "Type: AR-15", "Power: Strong",
                                 "Republican", "Moderate", "Libertarian", "Other",
                                 "Male", "Strong × AR-15")))
write.table(sup.table1, file = "./tables/supTable1.csv", sep = ",", row.names = F)



# Consider magazine size as supplementary table 2?
# table.cap1 = tidy(model.cap)
# table.cap1$term = c("(Intercept)", "AR-15", "Strong-gun", "Republican", "Moderate", "Libertarian", "Other-pol", "Male", "Strong_AR-15")
# table.cap1
# table.cap2 = tidy(model.cap_bin)
# table.cap2$term = c("(Intercept)", "AR-15", "Strong-gun", "Republican", "Moderate", "Libertarian", "Other-pol", "Male", "Strong_AR-15")
# table.cap2
