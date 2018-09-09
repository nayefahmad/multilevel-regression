

#**************************************************************
# MULTILEVEL REGRESSION TO PREDICT RADON LEVELS OF HOUSES 
# USING HOUSE-LEVEL AND COUNTY-LEVEL PREDICTORS 
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

# extract variables of interest: 
df2.test.data <- df1.houses %>% 
    filter(state == "MN") %>% 
    select(activity, 
           floor, 
           county) %>% 
    mutate(radon.log = log(activity)) %>% 
    rename(radon = activity) %>% 
    select(radon.log, everything())

str(df2.test.data) 
head(df2.test.data)


# get county index variable: 
df3.county <- read.delim(here("data", 
                              "radon", 
                              "cty.dat"), 
                         sep = ",")

str(df3.county)

# following code is from http://www.stat.columbia.edu/~gelman/arm/examples/radon/radon_setup.R
mn <- df1.houses$state=="MN"

county.name <- as.vector(df1.houses$county[mn])
uniq <- unique(county.name)
J <- length(uniq)
county <- rep (NA, J)

# replace county names with IDs: 
for (i in 1:J){
    county[county.name==uniq[i]] <- i
}


# get the county-level predictor
srrs2.fips <- df1.houses$stfips*1000 + df1.houses$cntyfips
usa.fips <- 1000*df3.county[,"stfips"] + df3.county[,"ctfips"]
usa.rows <- match (unique(srrs2.fips[mn]), usa.fips)
uranium <- df3.county[usa.rows,"Uppm"]
u <- log (uranium)



# EDA: ------------



