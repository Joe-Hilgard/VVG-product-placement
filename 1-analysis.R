library(readxl)
library(magrittr)
library(dplyr)
library(psych)
library(BayesFactor)
library(ggplot2)

dat = read.delim("processed_data.txt", stringsAsFactors = F)

# data cleaning
# Mutate factor forms of Gun_type and Power
dat$Gun_type_f = factor(dat$Gun_type, labels = c("ZQ-5", "AR-15")) %>% 
  C(sum) %>% 
  relevel(ref = "ZQ-5")
dat$Power_f = factor(dat$Power, labels = c("Weak", "Strong")) %>% 
  C(sum) %>% 
  relevel(ref = "Weak")
# factor form of political orientation
dat$pol_orien_f = factor(dat$pol_orien, labels = c("Dem", "Rep", "Mod", "Lib", "Other"))

# Analysis ----
# Manipulation check
glm(p_key_value_1 ~ Gun_type_f*Power_f, data = dat, family = "poisson") %>% summary
glm(p_key_value_2 ~ Gun_type_f*Power_f, data = dat, family = "poisson") %>% summary

tapply(dat$p_key_value_1, list(dat$Gun_type_f, dat$Power_f), mean, na.rm = T)
tapply(dat$p_key_value_2, list(dat$Gun_type_f, dat$Power_f), mean, na.rm = T)

ggplot(dat, aes(x = p_key_value_1, fill = Power_f)) +
  geom_histogram(position = "dodge")
ggplot(dat, aes(x = interaction(Power_f, Gun_type_f), y = p_key_value_1)) +
  geom_violin() +
  geom_point(position = position_jitter(w = .2, h = 0.2))

ggplot(dat, aes(x=p_key_value_2, fill = Power_f)) +
  geom_histogram(position = "dodge")
ggplot(dat, aes(x = interaction(Power_f, Gun_type_f), y = p_key_value_2)) +
  geom_violin() +
  geom_point(position = position_jitter(w = .15, h = 0))

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
         Gun_type_f, Power_f, pol_orien_f) %>% 
  filter(complete.cases(.)) %>% 
  as.data.frame()

mod1 = anovaBF(amend2 ~ Gun_type_f * Power_f, 
               rscaleFixed = .5, data = dat1)
mod2 = anovaBF(AR_intent ~ Gun_type_f * Power_f, 
               rscaleFixed = .5, data = dat1)
mod3 = anovaBF(game_gun_desire ~ Gun_type_f * Power_f, 
               rscaleFixed = .5, data = dat1)
mod4 = anovaBF(policy ~ Gun_type_f * Power_f, 
               rscaleFixed = .5, data = dat1)


# Adding politics as a covariate
mod1_cov = anovaBF(amend2 ~ Gun_type_f * Power_f + pol_orien_f, 
                   rscaleFixed = .5, data = dat1)
mod2_cov = anovaBF(AR_intent ~ Gun_type_f * Power_f + pol_orien_f, 
                   rscaleFixed = .5, data = dat1)
mod3_cov = anovaBF(game_gun_desire ~ Gun_type_f * Power_f + pol_orien_f, 
                   rscaleFixed = .5, data = dat1)
mod4_cov = anovaBF(policy ~ Gun_type_f * Power_f + pol_orien_f, 
                   rscaleFixed = .5, data = dat1)

# AR_desire had some missing data
mod5_cov = dat %>% 
  select(AR_desire, Gun_type_f, Power_f, pol_orien_f) %>% 
  filter(complete.cases(.)) %>% 
  as.data.frame() %>% 
  anovaBF(AR_desire ~ Gun_type_f * Power_f + pol_orien_f, 
          rscaleFixed = .5, data = .)
sort(mod5_cov)


