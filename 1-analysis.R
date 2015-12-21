library(readxl)
library(magrittr)
library(dplyr)
library(psych)
library(BayesFactor)
library(ggplot2)

dat = read_excel("data_final.xls", 1)

# data cleaning
# Mutate factor forms of Gun_type and Power
dat$Gun_type_f = factor(dat$Gun_type, labels = c("ZQ-5", "AR-15")) %>% 
  C(sum) %>% 
  relevel(ref = "ZQ-5")
dat$Power_f = factor(dat$Power, labels = c("Weak", "Strong")) %>% 
  C(sum) %>% 
  relevel(ref = "Weak")

lm(AR_sum1 ~ Gun_type_f * Power_f, data = dat) %>% 
  summary
dat %>% 
  group_by(Gun_type, Power) %>% 
  summarize(AR_mean = mean(AR_sum1, na.rm=T))

# How much cleaning is there to do?


# Analysis ----
# Manipulation check
glm(p_key_value_1 ~ Gun_type_f*Power_f, data = dat, family = "poisson") %>% summary
glm(p_key_value_2 ~ Gun_type_f*Power_f, data = dat, family = "poisson") %>% summary

tapply(dat$p_key_value_1, list(dat$Gun_type_f, dat$Power_f), mean, na.rm = T)
tapply(dat$p_key_value_2, list(dat$Gun_type_f, dat$Power_f), mean, na.rm = T)

ggplot(dat, aes(x = p_key_value_1, fill = Power_f)) +
  geom_histogram(position = "dodge")
ggplot(dat, aes(x = interaction(Power_f, Gun_type_f), y = p_key_value_1)) +
  geom_violin()

ggplot(dat, aes(x=p_key_value_2, fill = Power_f)) +
  geom_histogram(position = "dodge")
ggplot(dat, aes(x = interaction(Power_f, Gun_type_f), y = p_key_value_2)) +
  geom_violin() +
  geom_point(position = position_jitter(w = .15, h = 0))

# Main outcomes
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