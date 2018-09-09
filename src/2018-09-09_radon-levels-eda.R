

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


# correlation plot: 
p1.pairs <- df5.combined.data %>%
    select(radon.log, 
           floor, 
           county.uranium) %>% 
    ggpairs(); p1.pairs

# 1) note that log radon has a very nice-looking normalish distribution
# 2) also note that there is the suggestion that floor 0 predicts 
#   higher radon levels than floor 1 
# 3) Weak suggestion that as uranium level increases, log radon increases
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

pdf(here("results", "output from src", 
         "radon-vs-floor-all-counties.pdf"))
plot.list
dev.off()








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



# model with only county level predictor 
m2.uranium <- lm(radon.log ~ county.uranium, 
                 data = df5.combined.data)
summary(m2.uranium)

# model is not significant. 