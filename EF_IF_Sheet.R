# Your whole pipeline becomes:
# Data = combinations of dimensions
# (Sector × Source × Scenario × Pollutant × Year)
# That’s basically a multi-dimensional model, and long format handles it naturally.


library(tidyverse)
library(readxl)
library(dplyr)
library(janitor)
library(reshape2)

# Raw energy system emission factors
ef_raw <- read_excel("EFs_and_IFs/ukiam_efs.xlsx") # reading in long EF file

# Processed energy system emission factors
ef_processed <- ef_raw %>% # creating final EF for all sources and combinations, to be called in when needed
  mutate(
    
    net_ef = fuel_ef * (1-ccs_capture) # mutate to add another column called net_ef - which is calculated via product of fuel_ef and ccs_capture
    
  )



  







                   