# Tables
dat = read.delim("processed_data.txt", stringsAsFactors = F)

library(dplyr)

# Table 2: Means and SDs for "all variables"
# curried mean(na.rm = T) and sd(na.rm = T) functions
mean.na.rm = function(x) mean(x, na.rm = T)
sd.na.rm = function(x) sd(x, na.rm = T)
# Make table with dplyr::summarize()
dat %>% 
  group_by(Gun_type, Power) %>% 
  rename(Deaths = p_key_value_1,
         Kills = p_key_value_2) %>% 
  summarize_each(funs(mean.na.rm, sd.na.rm), 
                 Deaths:Kills, speech_protect:glad_gun) %>% 
  signif(3) %>% 
  #t() %>% 
  View

# Supplementary Table 1
gamma1export = cbind("Outcome" = "Gun-owner accident", tidy(gamma1))
gamma2export = cbind("Outcome" = "Gun stolen", tidy(gamma2))
gamma3export = cbind("Outcome" = "Use in self-defense", tidy(gamma3))
gamma4export = cbind("Outcome" = "Non-owner accident", tidy(gamma4))

round4 = function(x) round(x, digits = 4)
sup.table1 = bind_rows(gamma1export, gamma2export, gamma3export, gamma4export) %>% 
  mutate_each(funs(round4), estimate:p.value)
write.table(sup.table1, file = "./tables/supTable1.csv", sep = ",", row.names = F)
