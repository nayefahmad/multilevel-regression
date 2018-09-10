

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
library("lme4")
library("arm")  # package for the book 
library("ggpubr")

# help(package = "arm")
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

# > m0: ---------
# model with only floor as predictor, with full pooling: 
m0.floor <- lm(radon.log ~ floor - 1,  # add "-1" to show all levels of the factor floor 
               data = df5.combined.data)
summary(m0.floor)

# > m1: ---------
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



# > m2: ---------
# model with only county level predictor 
m2.uranium <- lm(radon.log ~ county.uranium, 
                 data = df5.combined.data)
summary(m2.uranium)

# > m3: ---------
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

# > m4.1: ---------
# try just county indicators (compare with model m5): 
m4.1.county.only <- lm(radon.log ~ county, 
                     data = df5.combined.data)
summary(m4.1.county.only)

# >> histograms: -----
df5.combined.data$radon.log %>% hist(xlim = c(-3, 4))   # actual values
predict(m4.1.county.only) %>% hist(xlim = c(-3, 4)) # predicted values 

# graph of actual versus predicted values: 
p3.actual.vs.pred.m4.1 <- df5.combined.data %>% 
    ggplot(aes(y = radon.log, 
               x = predict(m4.1.county.only))) + 
    geom_point() + 
    scale_x_continuous(limits = c(-2.5, 5)) + 
    scale_y_continuous(limits = c(-2.5, 5)) + 
    geom_abline(slope = 1, 
                intercept = 0, 
                colour = "grey60") + 
    geom_smooth() + 
    
    labs(x = "Fitted values from classical regression (intercepts only)", 
         y = "Actual response values") + 
    theme_classic(); p3.actual.vs.pred.m4.1




# > summarize models ----------
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



#****************************************************
# MULTILEVEL REGRESSION MODELS ---------
#****************************************************

# > m5 : -----------------
# Varying-intercept model with no predictors: this model simply 
# includes a constant term and allows it to vary by county
m5.multilevel.intercepts <- lmer(radon.log ~ 1 + (1 | county), 
                                 data = df5.combined.data)
# summary(m5.multilevel.intercepts)
arm::display(m5.multilevel.intercepts)  # custom display fn
# this display gives us the individual, lower-level regression
#    coefficients (in this case just the intercept)
#   > "coef.est = 1.33"
# It also gives the 2 different error components - at the 
#   individual level and at the group level
#   > individual level: "Residual error term = 0.76" 
#   > group level: "county error term = 0.33" 

# let's examine the fitted values from this model: 
df8.pred.m5 <- data.frame(county = df5.combined.data$county, 
                          m5.pred = predict(m5.multilevel.intercepts))
head(df8.pred.m5, 20)

# Each fitted value is simply picked from a normal distribution
#   (i.e. the higher-level group distribution that we specify
#   for the second level of the regression).
# Mean of this dist is 1.33, s.dev is 0.31

# >> histograms: ------
predict(m5.multilevel.intercepts) %>% hist(xlim = c(-3, 4)) # predicted values 
df5.combined.data$radon.log %>% hist(xlim = c(-3, 4))       # actual values
# umm, no, maybe not?? Perhaps it's because of the added variance
# at the individual level? Why are these distributions so different? 

# graph of actual versus predicted values: 
p4.actual.vs.pred.m5 <- df5.combined.data %>% 
    ggplot(aes(y = radon.log, 
               x = predict(m5.multilevel.intercepts))) + 
    geom_point() + 
    scale_x_continuous(limits = c(-2.5, 5)) + 
    scale_y_continuous(limits = c(-2.5, 5)) + 
    geom_abline(slope = 1, 
                intercept = 0, 
                colour = "grey60") + 
    geom_smooth() + 
    
    labs(x = "Fitted values from multilevel regression (intercepts only)", 
         y = "Actual response values") + 
    theme_classic(); p4.actual.vs.pred.m5






# > m6 : ------------
# Varying-intercept model with an individual-level predictor: 
m6.mult.floor <- lmer(radon.log ~ floor + (1 | county), 
                      data = df5.combined.data)
summary(m6.mult.floor)
arm::display(m6.mult.floor)  # custom display fn











#******************************************************
# WRITE OUTPUTS: -----------
#******************************************************

# classical vs multilevel regression 
ggarrange(p3.actual.vs.pred.m4.1, 
          p4.actual.vs.pred.m5, 
          nrow = 2) %>% 
    annotate_figure(top = text_grob("Compared to classical regression with only county indicators (top plot), \nthe multilevel regression with only county indicators restricts the range of predicted values \n  ", 
                                    size = 16), 
                    bottom = text_grob("\nSee Gelman and Hill, p257: \"The higher-level distribution has the effect of pulling the estimates of \nalpha_j toward the mean level mu_j\" \nThis is especially useful for estimates in counties with very small sample sizes \nIt also allows us to account for the fact that individuals within the same county are correlated with one another \n ", 
                                       size = 12))
ggsave(here("results", "output from src", 
            "compare-classical-and-multilevel-regression-intercept-only.pdf"))    
