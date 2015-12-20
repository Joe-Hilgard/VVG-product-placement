library(readxl)
library(magrittr)
library(dplyr)
library(psych)
library(BayesFactor)
library(ggplot2)

dat = read_excel("data_final.xls", 1)

# data cleaning
# mutate columns to numeric, coercing NAs

# Unfortunately, this doesn't seem to work, which makes me mad
# dat = dat %>% 
#   mutate_each(funs(as.numeric), vars = AR_1:AR_8)

# Do it the old way
dat$AR_2 = as.numeric(dat$AR_2)

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

# I wonder what happens if I just blast everything into PCA ----
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


lm(AR_sum1 ~ Gun_type * Power, data = dat) %>% 
  summary
dat %>% 
  group_by(Gun_type, Power) %>% 
  summarize(AR_mean = mean(AR_sum1, na.rm=T))

# How much cleaning is there to do?


# Analysis ----
dat_bayes = dat %>% 
  select(Gun_type, Power, AR_sum1) %>% 
  mutate(Gun_type = as.factor(Gun_type),
         Power = as.factor(Power),
         AR_sum1 = as.numeric(AR_sum1)) %>% 
  filter(complete.cases(.))
dat_bayes = data.frame(dat_bayes)
bayes_model = anovaBF(AR_sum1 ~ Gun_type * Power, rscaleFixed = .5, data = dat_bayes)
summary(bayes_model)
plot(bayes_model)
lm(AR_sum1 ~ Gun_type * Power, data = dat_bayes) %>% summary

# Analysis, considering covariates?