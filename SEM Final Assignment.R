# SEM 1: Final Assignment

# Group members:Paul Pfaff, Thomas Willems, Marisse Koolstra, Helen Jakobs, 
# Obed Appiah, Theresa Badenbach

# load packages
library("dplyr")
library("lavaan")
library("semPlot")

# load data
data <- read.csv("/Users/theresabadenbach/Desktop/SEM 1/SEM Final Project_March 30, 2022_08.25.csv")

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
  rename(Nationality = Q5, Age = Q4, Q1 = Q1_1, Q2 = Q1_2, Q3 = Q1_3, Q4 = Q1_4, Q5 = Q1_5,
         Q6 = Q1_6, Q7 = Q1_7, Q8 = Q1_8, Q9 = Q1_9, Q10 = Q1_10) #rename variables

# Change nationality labels to 1 = Dutch  and  0 = Non-Dutch
data$Nationality[data$Nationality == "Dutch"] <- 1
data$Nationality[data$Nationality == "Non-Dutch"] <- 0

# Turn whole data frame into numeric
data[] <- lapply(data, as.numeric)

# Inspect Nationality counts of participants
sum(data$Nationality == 1)
sum(data$Nationality == 0) # There are 54 Dutch and 85 Non-Dutch participants in the sample

# Inspect age demographics
mean(data$Age) # The mean age of the participants is 25 years.

# Create age categories with mean split (0 = younger than 25; 1 = 25 yrs or older)
data <- data  %>% mutate(Age.cat = case_when(Age < 25 ~ 0,
                                         Age >= 25 ~ 1))

# Fit theoretical CFA model
Model <- '
P =~ Q1 + Q2 + Q3 + Q4 + Q5
T =~ Q6 + Q7 + Q8 + Q9 + Q10
'

fit <- cfa(Model, data, std.lv = TRUE)

# look at the fit measures
summary(fit, fit.measure = TRUE)
fitMeasures(fit)

# look at modification indices
modificationindices(fit)

#fit configural model
conf <- cfa(Model, data, group = "Nationality", std.lv = TRUE)
fitMeasures(conf,
            c("rmsea","cfi","tli","rni","rfi","ifi","srmr","gfi"))

# weak invariance
weak <- cfa(Model, data, group = "Nationality",
            group.equal = "loadings", std.lv = TRUE)
anova(conf, weak)

# strong invariance
strong <- cfa(Model, data, group = "Nationality",
              group.equal = c("loadings","intercepts"), std.lv = TRUE)
anova(conf, weak, strong)

# inspect where the misfit is
lavInspect(strong, "mu")[[1]] -
  lavInspect(strong, "sampstat")[[1]]$mean

lavInspect(strong, "mu")[[2]] -
  lavInspect(strong, "sampstat")[[2]]$mean

# partial strong invariance, set item 4 free in the groups
strong2 <- cfa(Model, data, group = "Nationality",
               group.equal = c("loadings","intercepts"),
               group.partial= "Q4~1", std.lv = TRUE)

anova(conf, weak, strong2)

# strict invariance
strict <- cfa(Model, data, group = "Nationality",
              group.equal = c("loadings","intercepts","residuals",
                              "residual.covariances"),
              group.partial= "Q4~1", std.lv = TRUE)

anova(conf, weak, strong2, strict)

# Compare the variance of the latent variable between the groups (homogeneity test)
eqvar<- cfa(Model, data, group = "Nationality",
            group.equal = c("loadings","intercepts","residuals",
                            "residual.covariances","lv.variances"),
            group.partial= "Q4~1", std.lv = TRUE)

anova(strict, eqvar)


# Compare the means of the latent variable between the groups (homogeneity test)
eqmeans <- cfa(Model, data, group = "Nationality",
               group.equal = c("loadings","intercepts","residuals",
                               "residual.covariances","lv.variances", "means"),
               group.partial= "Q4~1", std.lv = TRUE)

anova(eqvar, eqmeans)

### Conclusion: The groups do not differ on the means and variance of the latent variables. 
### This means that Dutch and Non-Dutch nationals perceive Amsterdam in the same way when it comes 
### to their perception of the city being a party or a tolerant/liberal city.

# Create path model
semPaths(fit, 
         style = "ram", 
         what = "std", 
         theme = "colorblind", 
         sizeLat = 10, 
         sizeMan = 7, 
         groups = "latents",
         borders = FALSE,
         sizeInt = 60, 
         pastel = TRUE,
         filetype = "jpg", width = 20, height = 18, filename = "Semmodel_poster") 


### Exploratory Analysis: Age category as grouping variable
# Fit theoretical CFA model
Model2 <- '
FC =~ Q1 + Q2 + Q3 + Q4 + Q5
PC =~ Q6 + Q7 + Q8 + Q9 + Q10
'

fit2 <- cfa(Model2, data, std.lv = TRUE) 

# look at the fit measures
summary(fit2, fit.measure = TRUE)

fitMeasures(fit2)

# look at modification indices
 modificationindices(fit2)


#fit configural model
conf_2 <- cfa(Model2, data, group = "Age.cat", std.lv = TRUE)
fitMeasures(conf,
            c("rmsea","cfi","tli","rni","rfi","ifi","srmr","gfi"))

# weak invariance
weak_2 <- cfa(Model2, data, group = "Age.cat",
              group.equal = "loadings", std.lv = TRUE)
anova(conf_2, weak_2)

# strong invariance
strong_2 <- cfa(Model2, data, group = "Age.cat",
                group.equal = c("loadings","intercepts"), std.lv = TRUE)
anova(conf_2, weak_2, strong_2)

#strict invariance
strict_2 <- cfa(Model2, data, group = "Age.cat",
                group.equal = c("loadings","intercepts","residuals",
                                "residual.covariances"), std.lv = TRUE)

anova(conf_2, weak_2, strong_2, strict_2)

# Compare the variance of the latent variable between the groups (homogeneity test)
eqvar_2<- cfa(Model2, data, group = "Age.cat",
              group.equal = c("loadings","intercepts","residuals",
                              "residual.covariances","lv.variances"), std.lv = TRUE)

anova(strict_2, eqvar_2)


# Compare the means of the latent variable between the groups (homogeneity test)
eqmeans_2 <- cfa(Model2, data, group = "Age.cat",
                 group.equal = c("loadings","intercepts","residuals",
                                 "residual.covariances","lv.variances", "means"), 
                 std.lv = TRUE)

anova(eqvar_2, eqmeans_2)

# same conclusion for age as for nationality

# To determine whether both latent variables fall under the same construct, 
# we create higher order model: General Image of Amsterdam

Model3 <- '
GI =~ Q1 + Q2 + Q3 + Q4 + Q5 + Q6 + Q7 + Q8 + Q9 + Q10
'
# fit higher order model
fit_HO <- cfa(Model3, data, std.lv = TRUE)

# compare two factor model with higher order model
anova(fit, fit_HO)

# inspect modification indices
modificationindices(fit_HO)

