

#**************************************************************
# MULTILEVEL REGRESSION TO PREDICT RADON LEVELS OF HOUSES 
# USING HOUSE-LEVEL AND COUNTY-LEVEL PREDICTORS 

# REGRESSION MODELS  
#**************************************************************
# 2018-09-09
# Nayef Ahmad

# This is an example from the book "Data analysis using regression and 
# multilevel/hierarchical models" 

library("here")
library("GGally")


# rm(list = ls())

# load data: 
source(here("src", 
            "2018-09-09_radon-levels-data-cleaning.R"))


# EXPLORATORY DATA ANALYSIS : ----------
# correlation plot: 
p1.pairs <- df5.combined.data %>%
    select(radon.log, 
           floor, 
           county.uranium) %>% 
    ggpairs(); p1.pairs

# 1) note that log radon has a very nice-looking normalish distribution
# 2) also note that there is the suggestion that floor 0 predicts 
#   higher radon levels than floor 1 
# 3) Suggestion that as uranium level increases, log radon increases
#   (see scatterplot on lower left) 

ggsave(here("results", "output from src", 
            "corplot.pdf"), 
       p1.pairs)


# examine a specific county: 
p2.rad.vs.floor.hennepin <- df5.combined.data %>% 
    filter(countyID == 141) %>%
    
    ggplot(aes(x = floor, 
               y = radon.log)) + 
    geom_jitter(width = 0.1) + 
    labs(title = "HENNEPIN") +
    
    geom_smooth(); p2.rad.vs.floor.hennepin
    

# let's look at all counties and save all plots in a list: 
counties <- unique(df5.combined.data$county)

plot.list <- list()
for (i in 1:nrow(df6.crossref)) {
    # identify the right county: 
    j <- filter(df6.crossref, 
                row == i) %>% 
        select(countyID) %>% as.integer
    
    plot.list[[i]] <- df5.combined.data %>% 
        filter(countyID == j) %>% 
        
        ggplot(aes(x = floor, 
                   y = radon.log)) + 
        geom_jitter(width = 0.1) +
        labs(title = counties[i] %>% as.character()) + 
        theme_classic(base_size = 16)
    
}


# examine plots (will take a while to run) : 
# plot.list
# plot.list[[15]]

pdf(here("results", "output from src", 
         "radon-vs-floor-all-counties.pdf"))
plot.list
dev.off()






#****************************************************
# CLASSICAL REGRESSION MODELS ---------
#****************************************************

# model with only floor as predictor, with full pooling: 
m0.floor <- lm(radon.log ~ floor - 1,  # add "-1" to show all levels of the factor floor 
               data = df5.combined.data)
summary(m0.floor)


# model with no pooling: 
m1.floor.no.pool <- lm(radon.log ~ floor + county, 
                       data = df5.combined.data)
summary(m1.floor.no.pool)
# using this model, we can't really compare most counties, 
#   because there's too little data in many of them, and without 
#   pooling, we can't estimate their effects with any confidence. 

# also, all the county factors are absorbed into the single 
# indicator variable. We have no model for how group-levle 
# predictors prduce the values of the indicator coefficients. 

# If our reseaerch question was: "What's the impact of county 
# uranium levels, after properly accounting for the hierarchical 
# structure of houses in counties", then this model is useless.  




# model with only county level predictor 
m2.uranium <- lm(radon.log ~ county.uranium, 
                 data = df5.combined.data)
summary(m2.uranium)


# county and house-level predictors: 
m3.floor.and.ur <- lm(radon.log ~ floor + county.uranium, 
                      data = df5.combined.data)
summary(m3.floor.and.ur)


# what happens if we include both county indicators and county-level
#   predictor in a classical regression? 
# ANS. doesn't work. You get NA for uranium estimate 
m4.test <- lm(radon.log ~ floor + county + county.uranium, 
              data = df5.combined.data)
summary(m3.test)



# summarize models 
df7.model.summary <- 
    data.frame(model = paste0("m", 0:3), 
               predictors = c("floor", 
                              "floor + county", 
                              "county.uranium", 
                              "floor + county.uranium"), 
               issues = c("no group-level predictors", 
                          "all group-level predictors absorbed into intercept \"county\", without acknowledging that there is a group-level error in specifying county effects", 
                          "no individual-level predictors ", 
                          "fully pooled model: error structure does not allow for different groups to behave differently ")) %>% print


# DISCUSSION------------------------
# Where does all this leave us? We can conclude from model m3 that radon 
# decreases on 1st floor, and increases as county uranium increases.

# However, our confidence in the estimates is low because they do not 
# take into account the grouped structure of the data: all the houses 
# in the same county are more similar to one another than to those 
# in a different county. Failing to do this impacts standard errors and 
# can cause us to make incorrect inferences. In multilevel modelling, 
# we can add the same term alpha_j to all houses that are in the same 
# county, whicn induces the correlation structure we want. 

# Also focusing on m3, note that a county's effect cannot be fully specified by its 
# uranium level: it's not a perfect correlation. So it is a problem 
# that we can't enter both the group-level predictors and group indicators. 
# There's an error associated with identifying a county using its 
# predictors (call this the group level error). 

# Then there's a separate error associated with the 
# individual house level: even after fully accountign for floor and 
# county effect, there we cannot fully predict log radon. 

# Classical regression does not allow us to separate out these errors, 
# which limits the inferences we can make. 


