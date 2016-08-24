# install.packages(c('pscl', 'psych', 'readxl', 'magrittr', 'plyr', 'dplyr',
#                    'tidyr', 'BayesFactor', 'ggplot2', 'broom', 'knitr'))

library(pscl)
library(psych)
library(readxl)
library(magrittr)
library(plyr)
library(dplyr)
library(tidyr)
library(BayesFactor)
library(ggplot2)
library(broom)
library(knitr)

source("0-cleaning.R")
source("1-analysis.R") # Computationally expensive!
knit("2-results.Rmd")
source("3-plots.R")
source("4-tables.R")

