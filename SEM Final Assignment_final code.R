# SEM 1: Final Assignment

# Group members:Paul Pfaff, Thomas Willems, Marisse Koolstra, Helen Jakobs, 
# Obed Appiah, Theresa Badenbach

# load packages
library("dplyr")
library("lavaan")

# load data

getwd()
setwd("C:/DATA/Masterdingen/SEM1")
data <- read.csv("C:/DATA/Masterdingen/SEM1/SEM Final Project_March 30, 2022_08.25.csv")

# inspect data
head(data)
dim(data)

# Clean data
data <- data %>%  slice(-1, -2, -3) %>%  ## remove item descriptions and test case
  subset(Progress == 100) %>%  ## remove participants who didn't finish survey
  select(-StartDate, -EndDate, -Status, -IPAddress, -Progress, 
         -starts_with("Duration"), -Finished, -RecordedDate, -ResponseId,
         -RecipientFirstName, -RecipientLastName, -RecipientEmail, 
         -ExternalReference, -LocationLatitude, - LocationLongitude,
         -DistributionChannel, -UserLanguage, -Q10) %>% ## remove unnecessary variables
  rename(Nationality = Q5, Age = Q4) #rename variables

# Change nationality labels to 1 = Dutch  and  0 = Non-Dutch
data$Nationality[data$Nationality == "Dutch"] <- 1
data$Nationality[data$Nationality == "Non-Dutch"] <- 0

# Turn whole data frame into numeric
data[] <- lapply(data, as.numeric)

# Inspect age demographics
mean(data$Age) # The mean age  of the participants is 25 years.

# Create age categories with mean split (0 = younger than 25; 1 = 25 yrs or older)
data <- data  %>% mutate(Age.cat = case_when(Age < 25 ~ 0,
                                         Age >= 25 ~ 1))

# Fit theoretical CFA model
Model <- '
FC =~ Q1_1 + Q1_2 + Q1_3 + Q1_4 + Q1_5
PC =~ Q1_6 + Q1_7 + Q1_8 + Q1_9 + Q1_10
'

fit <- cfa(Model, data, std.lv=TRUE) 

# look at the fit measures
summary(fit, fit.measure = TRUE)
fitMeasures(fit)

# look at modification indices
modificationindices(fit)


#fit configural model
conf <- cfa(Model, data, group = "Nationality", std.lv=TRUE)
fitMeasures(conf)
fitMeasures(conf,
            c("rmsea","cfi","tli","rni","rfi","ifi","srmr","gfi"))

# weak invariance
weak <- cfa(Model, data, group = "Nationality",
            group.equal = "loadings", std.lv=TRUE)
fitMeasures(weak)
anova(conf, weak)

# strong invariance
strong <- cfa(Model, data, group = "Nationality",
              group.equal = c("loadings","intercepts"), std.lv=TRUE)
fitMeasures(strong)
anova(conf, weak, strong)

# inspect where the misfit is
lavInspect(strong, "mu")[[1]] -
  lavInspect(strong, "sampstat")[[1]]$mean

lavInspect(strong, "mu")[[2]] -
  lavInspect(strong, "sampstat")[[2]]$mean

# partial strong invariance, set item 4 free in the groups
strong2 <- cfa(Model, data, group = "Nationality",
               group.equal = c("loadings","intercepts"),
               group.partial= "Q1_4~1", std.lv=TRUE)
fitMeasures(strong2)

anova(conf, weak, strong2)

# strict invariance
strict <- cfa(Model, data, group = "Nationality",
              group.equal = c("loadings","intercepts","residuals",
                              "residual.covariances"),
              group.partial= "Q1_4~1", std.lv=TRUE)
fitMeasures(strict)

anova(conf, weak, strong2, strict)

# Compare the variance of the latent variable between the groups (homogeneity test)
eqvar<- cfa(Model, data, group = "Nationality",
            group.equal = c("loadings","intercepts","residuals",
                            "residual.covariances","lv.variances"),
            group.partial= "Q1_4~1", std.lv=TRUE)

fitMeasures(eqvar)
anova(strict, eqvar)


# Compare the means of the latent variable between the groups (homogeneity test)
eqmeans <- cfa(Model, data, group = "Nationality",
               group.equal = c("loadings","intercepts","residuals",
                               "residual.covariances","lv.variances", "means"),
               group.partial= "Q1_4~1", std.lv=TRUE)

fitMeasures(eqmeans)
anova(eqvar, eqmeans)

### Conclusion; The groups do not differ on the means and variance of the latent variables. 
### This means that Dutch and Non-Dutch nationals perceive Amsterdam in the same way when it comes 
### to their perception of the city being a party or a free city.
