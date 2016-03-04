library(pscl)
library(psych)
library(readxl)
library(magrittr)
library(dplyr)
library(tidyr)
library(BayesFactor)
library(ggplot2)
library(broom)

dat = read.delim("processed_data.txt", stringsAsFactors = F)

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

# Analysis ----
# Manipulation check
model.deaths = glm(p_key_value_1 ~ Gun_type_f*Power_f, data = dat, family = "poisson")
model.deaths2 = zeroinfl(p_key_value_1 ~ Gun_type_f*Power_f, data = dat, dist = "negbin")
model.kills = glm(p_key_value_2 ~ Gun_type_f*Power_f, data = dat, family = "poisson")
model.kills2 = glm.nb(p_key_value_2 ~ Gun_type_f*Power_f, data = dat)

tapply(dat$p_key_value_1, list(dat$Gun_type_f, dat$Power_f), mean, na.rm = T)
tapply(dat$p_key_value_2, list(dat$Gun_type_f, dat$Power_f), mean, na.rm = T)

ggplot(dat, aes(x = p_key_value_1, fill = Power_f)) +
  geom_histogram(position = "dodge")

ggplot(dat, aes(x=p_key_value_2, fill = Power_f)) +
  geom_histogram(position = "dodge")

# How bad is data missingness across DVs?
DVs = c("amend1", "amend2", 
        "AR_desire", "AR_intent", "AR_price", 
        "game_gun_desire", "policy", "magazine_cap",
        "per_1", "per_2", "per_3", "per_4")
for (i in DVs) {
  print(i); print(sum(is.na(dat[,i])))
}

# Main outcomes
dat1 = dat %>% 
  select(amend2, AR_intent, game_gun_desire, policy,
         Gun_type_f, Power_f, pol_orien_f, Gender_f) %>% 
  filter(complete.cases(.)) %>% 
  as.data.frame()

# mod1 = anovaBF(amend2 ~ Gun_type_f * Power_f, 
#                rscaleFixed = .5, data = dat1)
# mod2 = anovaBF(AR_intent ~ Gun_type_f * Power_f, 
#                rscaleFixed = .5, data = dat1)
# mod3 = anovaBF(game_gun_desire ~ Gun_type_f * Power_f, 
#                rscaleFixed = .5, data = dat1)
# mod4 = anovaBF(policy ~ Gun_type_f * Power_f, 
#                rscaleFixed = .5, data = dat1)


# Adding politics as a covariate
# These take a LOT of cycles to run. Maybe make a note of the best & most relevant models,
  # run those in lmBF, then make ratios.
mod1_cov = anovaBF(amend2 ~ Gun_type_f * Power_f + pol_orien_f + Gender_f, 
                   rscaleFixed = .5, data = dat1)
mod2_cov = anovaBF(AR_intent ~ Gun_type_f * Power_f + pol_orien_f + Gender_f, 
                   rscaleFixed = .5, data = dat1)
mod3_cov = anovaBF(game_gun_desire ~ Gun_type_f * Power_f + pol_orien_f + Gender_f, 
                   rscaleFixed = .5, data = dat1)
mod4_cov = anovaBF(policy ~ Gun_type_f * Power_f + pol_orien_f + Gender_f, 
                   rscaleFixed = .5, data = dat1)
sort(mod1_cov)

# extract a Bayes Factor like this:
mod1_cov["Gender_f"]@bayesFactor$bf %>% exp()

sort(mod2_cov)
sort(mod3_cov)
sort(mod4_cov)

# AR_desire had some missing data
mod5_cov = dat %>% 
  select(AR_desire, Gun_type_f, Power_f, pol_orien_f, Gender_f) %>% 
  filter(complete.cases(.)) %>% 
  as.data.frame() %>% 
  anovaBF(AR_desire ~ Gun_type_f * Power_f + pol_orien_f + Gender_f, 
          rscaleFixed = .5, data = .)
sort(mod5_cov)

# geometrically-distributed outcomes
## owner experiencing accident
dat$per_1[dat$per_1 == 0] = .001
gamma1 = glm(per_1 ~ Gun_type_f * Power_f + pol_orien_f + Gender_f, 
             data = dat, family = "Gamma")
summary(gamma1)
#plot(gamma1)

## owner experiencing gun theft
dat$per_2[dat$per_2 == 0] = .001
gamma2 = glm(per_2 ~ Gun_type_f * Power_f + pol_orien_f + Gender_f, 
             data = dat, family = "Gamma")
summary(gamma2)
#plot(gamma2)

## owner using in self-defense
dat$per_3[dat$per_3 == 0] = .001
gamma3 = glm(per_3 ~ Gun_type_f * Power_f + pol_orien_f + Gender_f, 
             data = dat, family = "Gamma")
summary(gamma3)
#plot(gamma3)

## non-owner experiencing accident
dat$per_4[dat$per_4 == 0] = .001
gamma4 = glm(per_4 ~ Gun_type_f * Power_f + pol_orien_f + Gender_f, 
             data = dat, family = "Gamma")
summary(gamma4)
#plot(gamma4)

# Magazine capacity
dat$magazine_cap_bin2 = ifelse(dat$magazine_cap_bin == "greater", 1, 0)
model.cap_bin = glm(magazine_cap_bin2 ~ Gun_type_f * Power_f + pol_orien_f + Gender_f, 
              data = dat, family = "binomial")
summary(model.cap_bin)

model.cap = lm(magazine_cap ~ Gun_type_f * Power_f + pol_orien_f + Gender_f, data = dat)
summary(model.cap)

# Save workspace so don't have to run the MCMC chains every time I build the Results!----
save.image(file = "Study_results.Rdata")
