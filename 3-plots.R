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
  gather(key = "variable", value = "value", deaths:magazine_cap)

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
  geom_boxplot(width = .15) +
  facet_wrap(~variable, scales = "free_y") +
  scale_x_discrete("Effects of Condition")