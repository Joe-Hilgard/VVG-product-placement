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
# Need to think how to aggregate and model these.


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

# Export
write.table(dat, "processed_data.txt", sep="\t", row.names=F)

# Inspect distributions
outcomes = c("amend1", "amend2", "AR_desire", "AR_intent", "AR_price", 
             "game_gun_desire", "policy")
dat = as.data.frame(dat)
for (i in outcomes) hist(dat[,i], main = i)
# AR_intent, AR_price, game_gun_desire all have strong right skew

# Factor structures and alphas ----
# Look at structure of AR-desire items
dat %>% 
  select(AR_1:AR_8) %>% 
  as.data.frame() %>% 
  alpha()

dat %>% 
  select(AR_1:AR_5) %>% 
  as.data.frame() %>% 
  alpha()

# Looks like 1 component
dat %>% 
  select(AR_1:AR_8) %>% 
  filter(complete.cases(.)) %>% 
  princomp()

# Sufficient just to sum those, I'd say
dat$AR_sum1 = dat %>% 
  select(AR_1:AR_8) %>% 
  apply(1, sum)

# Look at structure of gun-rights items ----
dat %>% 
  select(speech_protect:glad_gun) %>% 
  as.data.frame() %>% 
  alpha()

dat %>% 
  select(speech_protect:glad_gun) %>% 
  as.data.frame() %>% 
  filter(complete.cases(.)) %>% 
  princomp()

dat %>% 
  select(speech_protect, game_amend, media_reg, ben_risk, glad_speech) %>% 
  as.data.frame() %>% 
  alpha() 

dat %>% 
  select(speech_protect, game_amend, media_reg, ben_risk, glad_speech) %>% 
  as.data.frame() %>% 
  filter(complete.cases(.)) %>% 
  princomp()

dat %>% 
  select(gun_protect, gun_amendment, gun_reg, glad_gun) %>% 
  as.data.frame() %>% 
  alpha() 

dat %>% 
  select(gun_protect, gun_amendment, gun_reg, glad_gun) %>% 
  as.data.frame() %>% 
  filter(complete.cases(.)) %>% 
  princomp()

# Does summing the two have too much correlation?
dat %>% 
  select(speech_protect:glad_gun) %>% 
  cor(use = 'pairwise') %>% 
  round(3)

dat$speech_sum = dat %>% 
  select(speech_protect, game_amend, media_reg, ben_risk, glad_speech) %>% 
  apply(1, sum)

dat$gun_sum = dat %>% 
  select(gun_protect, gun_amendment, gun_reg, glad_gun) %>% 
  apply(1, sum)

# other items
dat %>% 
  select(want:need) %>% 
  cor(use='pairwise') %>% 
  round(3)

# other other items
dat %>% 
  select(us_gun_laws:school_staff) %>% 
  cor(use='pairwise') %>% 
  round(3)

# What if all just one PCA? ----
# You must scale if you're gonna do that! I think.
# 
pc = dat %>% 
  select(-(Subno:Pilot), -Notes, -per_5, -list_gun_names, -AR_pay) %>% 
  filter(complete.cases(.)) %>% 
  scale() %>% 
  princomp()

pc2 = dat %>% 
  select(-(Subno:Pilot), -Notes, -per_5, -list_gun_names, -AR_pay) %>% 
  filter(complete.cases(.)) %>% 
  scale() %>% 
  principal(nfactors = 2)

plot(pc)
#biplot(pc) # not sure this teaches much


loadings(pc)
