# This was an attempt to reduce data dimensionality through PCA or EFA
# It was ultimately mothballed as uninformative -- we instead analyze individual items
# It is archived here for posterity.

dat = read.delim("processed_data.txt", stringsAsFactors = F)

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