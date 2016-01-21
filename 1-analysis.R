library(readxl)
library(magrittr)
library(dplyr)
library(tidyr)
library(psych)
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
model.kills = glm(p_key_value_2 ~ Gun_type_f*Power_f, data = dat, family = "poisson")

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
plot(gamma1)

## owner experiencing gun theft
dat$per_2[dat$per_2 == 0] = .001
gamma2 = glm(per_2 ~ Gun_type_f * Power_f + pol_orien_f + Gender_f, 
             data = dat, family = "Gamma")
summary(gamma2)
plot(gamma2)

## owner using in self-defense
dat$per_3[dat$per_3 == 0] = .001
gamma3 = glm(per_3 ~ Gun_type_f * Power_f + pol_orien_f + Gender_f, 
             data = dat, family = "Gamma")
summary(gamma3)
plot(gamma3)

## non-owner experiencing accident
dat$per_4[dat$per_4 == 0] = .001
gamma4 = glm(per_4 ~ Gun_type_f * Power_f + pol_orien_f + Gender_f, 
             data = dat, family = "Gamma")
summary(gamma4)
plot(gamma4)

# Magazine capacity
dat$magazine_cap_bin2 = ifelse(dat$magazine_cap_bin == "greater", 1, 0)
model.cap_bin = glm(magazine_cap_bin2 ~ Gun_type_f * Power_f + pol_orien_f + Gender_f, 
              data = dat, family = "binomial")
summary(model.cap_bin)

model.cap = lm(magazine_cap ~ Gun_type_f * Power_f + pol_orien_f + Gender_f, data = dat)
summary(model.cap)

# Save workspace so don't have to run the MCMC chains every time I build the Results!----
save.image(file = "Study_results.Rdata")

# Plotting ----
# Manipulation check
# Deaths
ggplot(dat, aes(x = interaction(Gun_type_f, Power_f), y = p_key_value_1)) +
  geom_violin() +
  scale_y_continuous("Player's deaths") +
  scale_x_discrete("Condition")
# Kills
ggplot(dat, aes(x = interaction(Gun_type_f, Power_f), y = p_key_value_2)) +
  geom_violin() +
  scale_y_continuous("Monsters killed") +
  scale_x_discrete("Condition")

# 1st amendment
ggplot(dat, aes(x = interaction(Gun_type_f, Power_f), y = amend1)) +
  geom_violin() +
  geom_boxplot(width = .2) +
  #scale_y_continuous("Monsters killed") +
  scale_x_discrete("Condition")

# 2nd amendment
ggplot(dat, aes(x = interaction(Gun_type_f, Power_f), y = amend2)) +
  geom_violin() +
  geom_boxplot(width = .2) +
  #scale_y_continuous("Monsters killed") +
  scale_x_discrete("Condition")

# 2nd > 1st difference
ggplot(dat, aes(x = interaction(Gun_type_f, Power_f), y = amend2-amend1)) +
  geom_violin() +
  geom_boxplot(width = .2) +
    #scale_y_continuous("Monsters killed") +
  scale_x_discrete("Condition")

# AR desire
ggplot(dat, aes(x = interaction(Gun_type_f, Power_f), y = AR_desire)) +
  geom_violin() +
  geom_boxplot(width = .2) +
  #scale_y_continuous("Monsters killed") +
  scale_x_discrete("Condition")
# AR intent
ggplot(dat, aes(x = interaction(Gun_type_f, Power_f), y = AR_intent)) +
  geom_violin() +
  geom_boxplot(width = .2) +
  #scale_y_continuous("Monsters killed") +
  scale_x_discrete("Condition")
# AR price
ggplot(dat, aes(x = interaction(Gun_type_f, Power_f), y = AR_price)) +
#  geom_violin() +
  geom_boxplot() +
  #scale_y_continuous("Monsters killed") +
  scale_x_discrete("Condition")

ggplot(dat, aes(x = AR_desire, y = AR_price)) +
  #  geom_violin() +
  geom_point() +
  geom_smooth() 

# game gun desire
ggplot(dat, aes(x = interaction(Gun_type_f, Power_f), y = game_gun_desire)) +
  geom_violin() +
  geom_boxplot(width = .2) +
  #scale_y_continuous("Monsters killed") +
  scale_x_discrete("Condition")

# policy
ggplot(dat, aes(x = interaction(Gun_type_f, Power_f), y = policy)) +
  geom_violin() +
  geom_boxplot(width = .2) +
  #scale_y_continuous("Monsters killed") +
  scale_x_discrete("Condition")

# magazine cap
ggplot(dat, aes(x = interaction(Gun_type_f, Power_f), y = magazine_cap)) +
  geom_violin() +
  geom_boxplot(width = .2) +
  #scale_y_continuous("Monsters killed") +
  scale_x_discrete("Condition")

#Tidyr for faceted plot?
dat_tidy = dat %>% 
  select(-AR_price_blank, -AR_price) %>% 
  select(Subno:Power, Gun_type_f, Power_f,  
         p_key_value_1:p_key_value_2, amend1:policy, magazine_cap) %>% 
  rename(deaths = p_key_value_1,
         kills = p_key_value_2,
         free_speech = amend1,
         private_arms = amend2) %>% 
  gather(key = "variable", value = "value", kills:magazine_cap)

ggplot(dat_tidy, aes(x = interaction(Gun_type_f, Power_f),
                     y = value)) +
  geom_violin() +
  geom_boxplot(width = .2) +
  facet_wrap(~variable, scales = "free_y") +
  scale_x_discrete("Effects of Condition")

dat_tidy2 = dat %>% 
  select(Subno:Power, Gun_type_f, Power_f,  
         per_1:per_4) %>% 
  rename(accidental_discharge = per_1,
         stolen = per_2,
         use_in_defense = per_3,
         non_owner_accident = per_4) %>% 
  gather(key = "variable", value = "value", accidental_discharge:non_owner_accident)

ggplot(dat_tidy2, aes(x = interaction(Gun_type_f, Power_f),
                     y = value)) +
  geom_violin() +
  geom_boxplot(width = .2, notch = T) +
  facet_wrap(~variable, scales = "free_y") +
  scale_x_discrete("Effects of Condition")
