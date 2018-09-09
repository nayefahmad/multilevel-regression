

#**************************************************************
# MULTILEVEL REGRESSION TO PREDICT RADON LEVELS OF HOUSES 
# USING HOUSE-LEVEL AND COUNTY-LEVEL PREDICTORS 

# DATA CLEANING
#**************************************************************
# 2018-09-09
# Nayef Ahmad

# This is an example from the book "Data analysis using regression and 
# multilevel/hierarchical models" 

library("here")
library("tidyverse")
library("lme4")
library("magrittr")

# rm(list = ls())

# read in data: --------
df1.houses <- read.delim(here("data", 
                              "radon", 
                              "srrs2.dat"), 
                         sep = ",")

str(df1.houses)

# test dataset: 919 houses in the state of Minnesota: 
df2.test.data <- df1.houses %>% 
    filter(state == "MN") %>% 
    select(activity, 
           floor, 
           county) %>% 
    mutate(radon.log = ifelse(activity == 0, 0.1, 
                              log(activity))) %>% 
    rename(radon = activity) %>% 
    select(radon.log, 
           radon,
           floor,
           county) %>% 
    
    # create an ID for each county: 
    mutate(countyID = as.integer(county))

str(df2.test.data) 
head(df2.test.data)


# get county index variable: 
df3.county <- read.delim(here("data", 
                              "radon", 
                              "cty.dat"), 
                         sep = ",")

str(df3.county)

# get the county-level predictor - county uranium levels
# following code is from http://www.stat.columbia.edu/~gelman/arm/examples/radon/radon_setup.R
mn <- df1.houses$state=="MN"

srrs2.fips <- df1.houses$stfips*1000 + df1.houses$cntyfips
usa.fips <- 1000*df3.county[,"stfips"] + df3.county[,"ctfips"]
usa.rows <- match (unique(srrs2.fips[mn]), usa.fips)
uranium <- df3.county[usa.rows,"Uppm"]
u <- log (uranium)

df4.county.uranium <- data.frame(county.uranium = u, 
                                 row = seq_along(u))
# df4.county.uranium


# final combined dataset: 
county.ID.list <- unique(df2.test.data$countyID)
df6.crossref <- data.frame(countyID = county.ID.list, 
                           row = row_number(county.ID.list))

df5.combined.data <- df2.test.data %>% 
    inner_join(df6.crossref) %>% 
    left_join(df4.county.uranium) %>% 
    mutate(floor = factor(floor, 
                          levels = c(0, 1)))

str(df5.combined.data)
head(df5.combined.data)


