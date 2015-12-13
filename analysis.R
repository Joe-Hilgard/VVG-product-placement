library(readxl)
library(dplyr)

dat = read_excel("data_final.xls", 1)

# data cleaning
# mutate columns to numeric, coercing NAs

# Unfortunately, this doesn't seem to work, which makes me mad
dat = dat %>% 
  mutate_each(funs(as.numeric), vars = AR_1:AR_8)

# Do it the old way
dat$AR_2 = as.numeric(dat$AR_2)

# peek
dat %>% 
  select(AR_1:AR_8) %>% 
  cor(use = "pairwise") %>% 
  round(3)

# Lots to do. How will I aggregate these scales? PCA? EFA?
# How much cleaning is there to do?
