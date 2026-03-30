#### libraries #### ----
# for mixed effects model
library(lme4)

# adds p values for mixed modeld
library(lmerTest)
# gets R2 for mixed models
library(performance)
citation('performance')
# AICc comparisons
library(MuMIn)
citation('MuMIn')
# tidying data
library(dplyr)
# plotting graphs
library(ggplot2)

find("AICc")

### Importing all data ### ----
# soils dataset
final_data_soils <- read.csv("final_data_soils.csv")
# leaf dataset
final_data_leaves <- read.csv("final_data_leaves.csv")
# leaf N:P ratio data
ratio_leaves <- read.csv("ratio.csv")

### All data Wrangling ### ----
## SOIL DATA ##
# Checking classes of columns in my data set
str(final_data_soils)

#changing classes of data
final_data_soils$site <- as.factor(final_data_soils$site)
final_data_soils$block_number <- as.factor(final_data_soils$block_number)
final_data_soils$species <- as.factor(final_data_soils$species)
final_data_soils$treatment <- as.factor(final_data_soils$treatment)
final_data_soils$phosphate_mg_per_g <- as.numeric(final_data_soils$phosphate_mg_per_g)

# Re checking classes of columns in my data set
str(final_data_soils)

# removing data from blanks from this data set

clean_data_soils <- final_data_soils[-c(33, 34,35,36), ]
str(clean_data_soils)


# converting N from ug'g to mg/g 

clean_data_soils <- clean_data_soils %>%
  mutate(
    nitrate_ug_per_g = nitrate_ug_per_g / 1000,
    
  )
# changing the column header to reflect the new units
clean_data_soils <- clean_data_soils %>%
  rename(nitrate_mg_per_g = nitrate_ug_per_g)


clean_data_soils <- droplevels(clean_data_soils)
unique(clean_data_soils$treatment) # checking for spacing issues in treatment column of table
# i found spacing issue, this should fix it 

clean_data_soils$treatment <- trimws(as.character(clean_data_soils$treatment))
clean_data_soils$treatment <- factor(clean_data_soils$treatment)
#checking issue is fixed - it is, it now has three levels
unique(clean_data_soils$treatment)

# creating new column for just cultivated vs uncultivated
new_soil <- clean_data_soils %>%
  mutate(cultivation =
           ifelse(treatment == "Uncultivated",
                  "Uncultivated",
                  "Cultivated"))
str(new_soil)
# change cultivation to a factor 
new_soil$cultivation <- factor(new_soil$cultivation)

# make uncultivated the reference - ready for modelling
new_soil$cultivation <- relevel(new_soil$cultivation, ref = "Uncultivated")

## LEAF DATA ##

# Checking classes of columns in my data set
str(final_data_leaves)

#changing classes of data
final_data_leaves$site <- as.factor(final_data_leaves$site)
final_data_leaves$block_number <- as.factor(final_data_leaves$block_number)
final_data_leaves$species <- as.factor(final_data_leaves$species)
final_data_leaves$treatment <- as.factor(final_data_leaves$treatment)
final_data_leaves$phosphate_mg_per_g <- as.numeric(final_data_leaves$phosphate_mg_per_g)

# Re checking classes of columns in my data set
str(final_data_leaves)

# Deleting empty columns that got imported 
final_data_leaves <- final_data_leaves %>% select(-X,-X.1, -X.2, -X.3, -X.4, -X.5)

# new column ready for modelling
new_leaf <- final_data_leaves %>%
  mutate(cultivation =
           ifelse(treatment == "Uncultivated",
                  "Uncultivated",
                  "Cultivated"))

# making new column a factor 
new_leaf$cultivation <- as.factor(new_leaf$cultivation)

# making uncultivated the reference 
new_leaf$cultivation <- relevel(new_leaf$cultivation, ref = "Uncultivated")

## RATIO DATA ##
ratio_new <- ratio_leaves %>%
  mutate(cultivation =
           ifelse(treatment == "Uncultivated",
                  "Uncultivated",
                  "Cultivated"))
# making new column a factor
ratio_new$cultivation <- as.factor(ratio_new$cultivation)

# making uncultivated the reference ready for modelling
ratio_new$cultivation <- relevel(ratio_new$cultivation, ref = "Uncultivated")



### Soil Ammonium Modelling ### ----


amodel1 <- lmer(ammonium_mg_per_g ~ cultivation * species + (1 | site/block_number), 
                data = new_soil, REML = FALSE)
summary(amodel1)
# checking assumptions 
qqnorm(residuals(amodel1))
qqline(residuals(amodel1))
hist(residuals(amodel1))
plot(amodel1)

# decision to log transform
new_soil$log_a <- log(new_soil$ammonium_mg_per_g)

#full model logged
amodel2 <- lmer(log_a ~ cultivation * species + (1 | site/block_number), 
                data = new_soil, REML = FALSE)
summary(amodel2)
# checking assumptions 
qqnorm(residuals(amodel2))
qqline(residuals(amodel2))
hist(residuals(amodel2))
plot(amodel2)

# no interaction
amodel3 <- lmer(log_a ~ cultivation + species + (1 | site/block_number), 
                data = new_soil, REML = FALSE)

amodel4 <- lm(log_a ~ cultivation + species, data = new_soil, 
             REML = FALSE)

# model selection 
amodelnull <- lm(log_a ~ 1, data = new_soil, REML = FALSE)
AICc(amodel2, amodel3, amodel4, amodelnull)

# summary of best fitting model
summary(amodel3)
r2(amodel3)
# checking assumptions of this model 
qqnorm(residuals(amodel3))
qqline(residuals(amodel3))
hist(residuals(amodel3))
shapiro.test(residuals(amodel3))
plot(amodel3)

### Soil Phosphate Modelling ###

pmodel1 <- lmer(phosphate_mg_per_g ~ cultivation * species + (1 | site/block_number), 
                data = new_soil, REML = FALSE)
summary(pmodel1)
# checking assumptions 
qqnorm(residuals(pmodel1))
qqline(residuals(pmodel1))
hist(residuals(pmodel1))
plot(pmodel1)

# decision to log transform
new_soil$log_p <- log(new_soil$phosphate_mg_per_g)

#full model logged
pmodel2 <- lmer(log_p ~ cultivation * species + (1 | site/block_number), 
                data = new_soil, REML = FALSE)
summary(pmodel2)
# checking assumptions 
qqnorm(residuals(pmodel2))
qqline(residuals(pmodel2))
hist(residuals(pmodel2))
plot(pmodel2)

pmodel3 <- lmer(log_p ~ cultivation + species + (1 | site/block_number), 
                data = new_soil, REML = FALSE)

# best model
pmodel4 <- lmer(log_p ~ cultivation + species + (1 | block_number), 
                data = new_soil, REML = FALSE)
pmodel5 <- lm(log_p ~ cultivation + species, data = new_soil,
              REML = FALSE)

nullmodelp <- lm(log_p ~ 1, data = new_soil, REML = FALSE)
AICc(pmodel2, pmodel3, pmodel4, pmodel5, nullmodelp)

# summary of best fitting model
summary(pmodel4)
r2(pmodel4)
anova(pmodel4)
# checking assumptions of this model 
qqnorm(residuals(pmodel4))
qqline(residuals(pmodel4))
hist(residuals(pmodel4))
shapiro.test(residuals(pmodel4))
plot(pmodel4)

### Leaf Data Modelling ### ---

# leaf nitrogen modelling
nleaf1 <- lmer(n_concentration_mg_per_g ~ cultivation * species + (1 | site/block_number), 
                data = new_leaf, REML = FALSE)
summary(nleaf1)

# checking assumptions 
qqnorm(residuals(nleaf1))
qqline(residuals(nleaf1))
hist(residuals(nleaf1))
plot(nleaf1)

# decision to log transform
new_leaf$log_n <- log(new_leaf$n_concentration_mg_per_g)

nleaf2 <- lmer(log_n ~ cultivation * species + (1 | site/block_number), 
               data = new_leaf, REML = FALSE)
summary(nleaf2)
# checking assumptions 
qqnorm(residuals(nleaf2))
qqline(residuals(nleaf2))
hist(residuals(nleaf2))
plot(nleaf2)

# removing interaction
nleaf2.2 <- lmer(log_n ~ cultivation + species + (1 | site/block_number), 
                 data = new_leaf, REML = FALSE)

# removing site as explains no variance and removing interaction as not significant 
nleaf3 <- lmer(log_n ~ cultivation + species + (1 | block_number), 
               data = new_leaf, REML = FALSE)
summary(nleaf3)

# checking inclusion of random effect is necessary
nleaf4 <- lm(log_n ~ cultivation + species, 
               data = new_leaf, REML = FALSE)
summary(nleaf4)

nullnleaf <- lm(log_n ~ 1, data = new_leaf, REML = FALSE)

# comparing
AICc(nleaf2, nleaf2.2, nleaf3, nleaf4, nullnleaf)
# as there is a < 2 difference between 3 and 4, 3 is selected as it accounts for
## spatial non independence of block

# summary of best fitting model
summary(nleaf3)
r2(nleaf3)
anova(nleaf3)
# checking assumptions of this model 
qqnorm(residuals(nleaf3))
qqline(residuals(nleaf3))
hist(residuals(nleaf3))
shapiro.test(residuals(nleaf3))
plot(nleaf3)


# leaf phosphorous modelling

pleaf1 <- lmer(p_concentration_mg_per_g ~ cultivation * species + (1 | site/block_number), 
               data = new_leaf, REML = FALSE)
summary(pleaf1)

# checking assumptions 
qqnorm(residuals(pleaf1))
qqline(residuals(pleaf1))
hist(residuals(pleaf1))
plot(pleaf1)

# decision to log transform
new_leaf$log_p <- log(new_leaf$p_concentration_mg_per_g)

pleaf2 <- lmer(log_p ~ cultivation * species + (1 | site/block_number), 
               data = new_leaf, REML = FALSE)
summary(pleaf2)

# checking assumptions 
qqnorm(residuals(pleaf2))
qqline(residuals(pleaf2))
hist(residuals(pleaf2))
plot(pleaf2)

# no interaction
pleaf3 <- lmer(log_p ~ cultivation + species + (1 | site/block_number), 
               data = new_leaf, REML = FALSE)

# block number removed due to explaining no variance
pleaf3.3 <- lmer(log_p ~ cultivation + species + (1 | site), 
               data = new_leaf, REML = FALSE)

# sit added as fixed effect as less than 2 levels
pleaf4 <- lm(log_p ~ cultivation + species + site, 
               data = new_leaf, REML = FALSE)

summary(pleaf4)

nullpleaf <- lm(log_p ~1, data = new_leaf, REML = FALSE)

AICc(pleaf2, pleaf3, pleaf3.3, pleaf4, nullpleaf)

# summary of best fitting model
summary(pleaf4)
r2(pleaf4)
anova(pleaf4)
# checking assumptions of this model 
qqnorm(residuals(pleaf4))
qqline(residuals(pleaf4))
hist(residuals(pleaf4))
shapiro.test(residuals(pleaf4))
plot(pleaf4)


###### Foliar NP ratio modelling ######

str(ratio_new)
ratio1 <- lmer(N.P ~ cultivation * species + (1 | site/block_number), 
               data = ratio_new, REML = FALSE)
summary(ratio1)

# checking assumptions 
qqnorm(residuals(ratio1))
qqline(residuals(ratio1))
hist(residuals(ratio1))
plot(ratio1)


# decision to log transform
ratio_new$log_np <- log(ratio_new$N.P)

ratio2 <- lmer(log_np ~ cultivation * species + (1 | site/block_number), 
               data = ratio_new, REML = FALSE)
summary(ratio2)

# removing interaction
ratio3 <- lmer(log_np ~ cultivation + species + (1 | site/block_number), 
               data = ratio_new, REML = FALSE)
summary(ratio3)

# removing block random effect
ratio4 <- lmer(log_np ~ cultivation + species + (1 | site), 
               data = ratio_new, REML = FALSE)
summary(ratio4)

# making site fixed
ratio5 <- lm(log_np ~ cultivation + species + site, 
               data = ratio_new, REML = FALSE)
summary(ratio5)

# removing species 
ratio6 <- lm(log_np ~ cultivation + site, 
             data = ratio_new, REML = FALSE)
summary(ratio6)

nullratio <- lm(log_np ~ 1, data = ratio_new, REML = FALSE)

# comparing

AICc(ratio2, ratio3, ratio4, ratio5, ratio6, nullratio)

# summary of best fitting model
summary(ratio6)
r2(ratio6)
anova(ratio6)
# checking assumptions of this model 
qqnorm(residuals(ratio6))
qqline(residuals(ratio6))
hist(residuals(ratio6))
shapiro.test(residuals(ratio6))
plot(ratio6)


