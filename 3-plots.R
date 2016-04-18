# Plotting ----
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
dat = read.delim("processed_data.txt", stringsAsFactors = F)
dat$Condition = mapvalues(dat$Condition, 
                          from = c(1, 2, 3, 4),
                          to = c("Weak Sci-Fi", "Weak AR-15", "Strong Sci-Fi", "Strong AR-15"))

# Fix text for big images
text_good = theme(axis.text = element_text(size = 8),
                  axis.title = element_text(size = 12),
                  strip.text = element_text(size = 12))


#Tidyr for faceted plot?
dat_tidy = dat %>% 
  select(-AR_price_blank, -AR_price) %>% 
  select(Subno:Power, Gun_type_f, Power_f,  
         p_key_value_1:p_key_value_2, amend1:policy, magazine_cap) %>% 
  rename(Deaths = p_key_value_1,
         Kills = p_key_value_2,
         `1st Amendment` = amend1,
         `2nd Amendment` = amend2,
         `AR-15 Evaluation` = AR_desire,
         `AR-15 Purchasing Intent` = AR_intent,
         `In-Game Gun Evaluation` = game_gun_desire,
         `Gun Control Support` = policy,
         `Magazine Size Limit` = magazine_cap) %>% 
  gather(key = "variable", value = "value", Deaths:`Magazine Size Limit`)

# Figure 1: Manipulation checks
dat_tidy %>% 
  filter(variable %in% c("Kills", "Deaths")) %>% 
  ggplot(aes(x = Condition, y = value)) +
  geom_violin() +
  geom_boxplot(width = .2, notch = T) +
  facet_wrap(~variable, scales = "free_y") +
  scale_y_continuous("Count") +
  scale_x_discrete("Effects of Condition") +
  text_good
ggsave(filename = "Figure1.png", 
       width = 6.5, height = 3, 
       units = "in", 
       dpi = 300)

# Figure 2: All outcomes
dat_tidy %>% 
  filter(!(variable %in% c("Deaths", "Kills", "1st Amendment"))) %>% 
  ggplot(aes(x = Condition, y = value)) +
  geom_violin() +
  geom_boxplot(width = .2) +
  facet_wrap(~variable, scales = "free_y", nrow = 3) +
  scale_x_discrete("Effects of Condition") +
  scale_y_continuous("Value") +
  text_good
ggsave(filename = "Figure2.png", 
       width = 6.5, height = 8, 
       units = "in", 
       dpi = 600)

# Figure 3: Gamma-distributed outcomes
dat_tidy2 = dat %>% 
  select(Subno:Power, Gun_type_f, Power_f,  
         per_1:per_4) %>% 
  rename(`Accidental Discharge (Owner)` = per_1,
         `Gun Stolen` = per_2,
         `Use in Self-Defense` = per_3,
         `Accidental Discharge (Non-owner)` = per_4) %>% 
  gather(key = "variable", value = "value", 
         `Accidental Discharge (Owner)`:`Accidental Discharge (Non-owner)`)

ggplot(dat_tidy2, aes(x = Condition, y = value)) +
  geom_violin() +
  geom_boxplot(width = .15) +
  facet_wrap(~variable, scales = "free_y") +
  scale_y_continuous("Percentage") +
  scale_x_discrete("Effects of Condition") +
  text_good
ggsave(filename = "Figure3.png", 
       width = 6.5, height = 4.5, units = "in", 
       dpi = 600)