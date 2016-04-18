# Make summed scales for analysis
# Don't forget to reverse-score, you chucklehead!

library(readxl)
library(magrittr)
library(dplyr)
library(psych)
library(ggplot2)

dat = read_excel("data_final.xls", 1)

# data cleaning
# mutate columns to numeric, coercing NAs
dat$AR_2 = as.numeric(dat$AR_2)

# 1st amendment ----
dat$amend1 = dat %>% 
  select(speech_protect, game_amend, media_reg, ben_risk, glad_speech) %>% 
  apply(1, sum)

# 2nd amendment ----
dat$amend2 = dat %>% 
  select(gun_protect, gun_amendment, gun_reg, glad_gun) %>% 
  apply(1, sum)

# AR-15 desire ----
dat$AR_desire = dat %>% 
  select(AR_1:AR_5) %>% 
  apply(1, sum)

# AR-15 buying intention ----
dat$AR_intent = dat %>% 
  select(AR_6:AR_8) %>% 
  apply(1, sum)

# AR-15 price ----
dat$AR_price = as.numeric(dat$AR_pay)
dat$AR_price_blank = ifelse(dat$AR_pay == "blank", 1, 0)
table(dat$AR_price, dat$AR_price_blank, useNA = 'always')

# in-game gun desire ----
dat$game_gun_desire = dat %>% 
  select(want:need) %>% 
  apply(1, sum)

# public policy ----
dat$policy = dat$us_gun_laws + dat$us_rifle + dat$guns_det + 
  (7 - dat$gun_carry) + (7 - dat$school_staff)

dat$policy = dat %>% 
  select(us_gun_laws:school_staff) %>% 
  apply(1, sum)

# normative safety ----
table(dat$per_1); hist(dat$per_1, xlim = c(0, 100))
table(dat$per_2); hist(dat$per_2, xlim = c(0, 100))
table(dat$per_3); hist(dat$per_3, xlim = c(0, 100))
table(dat$per_4); hist(dat$per_4, xlim = c(0, 100))
# Looks like an... exponential function? Hazard function, right?
# [The harmonic mean] is appropriate for situations where the average of rates is desired
#dat$hmean_per = 

# magazine size ----
table(dat$magazine_cap)
# Adjust for text
dat$magazine_cap[dat$magazine_cap == "?"] = NA
infResps = names(table(dat$magazine_cap))[28:33] # text responses are at end
dat$magazine_cap[dat$magazine_cap %in% infResps] = 400
dat$magazine_cap = as.numeric(dat$magazine_cap)
# start binning
dat$magazine_cap[dat$magazine_cap > 30] = 30
dat$magazine_cap_bin = ifelse(dat$magazine_cap == 30, "greater", "other")

# data cleaning ----
# Mutate factor forms of Gun_type and Power
dat$Gun_type_f = factor(dat$Gun_type, labels = c("ZQ-5", "AR-15")) %>% 
  C(sum) %>% 
  relevel(ref = "ZQ-5")
dat$Power_f = factor(dat$Power, labels = c("Weak", "Strong")) %>% 
  C(sum) %>% 
  relevel(ref = "Weak")
# factor form of political orientation, Gender
dat$pol_orien_f = factor(dat$pol_orien, labels = c("Dem", "Rep", "Mod", "Lib", "Other"))
dat$Gender_f = factor(dat$Gender, labels = c("Female", "Male"))

# Export
write.table(dat, "processed_data.txt", sep="\t", row.names=F)
