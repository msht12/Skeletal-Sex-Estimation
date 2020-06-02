# ANTH 418
# Final Project
# Matthew Tang
# 42693838

# Load required packages
library(pscl)
library(stargazer)

# Set working directory
setwd("C:/Users/Matthew/Documents/ANTH 418/Project")

# Sex: 1 = male, 2 = female. Convert to binary 0,1 indicator for male.
euro$MALE = ifelse(euro$SEX == 1, 1, 0)

# Read in datasets
euro = read.csv("C:/Users/Matthew/Documents/ANTH 418/Project/European Data Set-May 2018 FAE.csv")
myvars = c("MALE", "BILIAC", "SACRAL.ML1", "T50AP", "FHDSI" , "FDML", "TMAXLN") # Specify the variables to be kept
euro_complete = euro[myvars]                                                    # Store required variables in new dataframe
euro_complete = euro_complete[complete.cases(euro_complete), ]                  # Keep complete cases only

# For long bone dimensions: F=femur, H=humerus, R=radius. 1=right, 2=left

logit.1 = glm(data = euro_complete, MALE ~ FHDSI, family = "binomial")
summary(logit.1)
pR2(logit.1) # Compute McFadden and other pseudo R-squared
euro_complete$logit.1.fitted = logit.1$fitted.values                                     # Obtained fitted probabilities
euro_complete$logit.1.fitted.binary = ifelse(euro_complete$logit.1.fitted >= 0.5, 1, 0)  # Convert probabilities to binary
euro_complete_check_1 = euro_complete$MALE - euro_complete$logit.1.fitted.binary         # Difference btw actual vs fitted (binary)
table(euro_complete_check_1)                                                             # Percent correctly predicted

logit.2 = glm(data = euro_complete, MALE ~ FHDSI + BILIAC, family = "binomial")
summary(logit.2)
pR2(logit.2)
euro_complete$logit.2.fitted = logit.2$fitted.values                                     # Obtained fitted probabilities
euro_complete$logit.2.fitted.binary = ifelse(euro_complete$logit.2.fitted >= 0.5, 1, 0)  # Convert probabilities to binary
euro_complete_check_2 = euro_complete$MALE - euro_complete$logit.2.fitted.binary         # Difference btw actual vs fitted (binary)
table(euro_complete_check_2)                                                             # Percent correctly predicted

logit.3 = glm(data = euro_complete, MALE ~ FHDSI + BILIAC + FDML, family = "binomial")
summary(logit.3)
pR2(logit.3)
euro_complete$logit.3.fitted = logit.3$fitted.values                                     # Obtained fitted probabilities
euro_complete$logit.3.fitted.binary = ifelse(euro_complete$logit.3.fitted >= 0.5, 1, 0)  # Convert probabilities to binary
euro_complete_check_3 = euro_complete$MALE - euro_complete$logit.3.fitted.binary         # Difference btw actual vs fitted (binary)
table(euro_complete_check_3)                                                             # Percent correctly predicted

logit.4 = glm(data = euro_complete, MALE ~ FHDSI + BILIAC + FDML + SACRAL.ML1, family = "binomial")
summary(logit.4)
pR2(logit.4)
euro_complete$logit.4.fitted = logit.4$fitted.values                                     # Obtained fitted probabilities
euro_complete$logit.4.fitted.binary = ifelse(euro_complete$logit.4.fitted >= 0.5, 1, 0)  # Convert probabilities to binary
euro_complete_check_4 = euro_complete$MALE - euro_complete$logit.4.fitted.binary         # Difference btw actual vs fitted (binary)
table(euro_complete_check_4)                                                             # Percent correctly predicted

logit.5 = glm(data = euro_complete, MALE ~ FHDSI + BILIAC + FDML + SACRAL.ML1 + T50AP, family = "binomial")
summary(logit.5)
pR2(logit.5)
euro_complete$logit.5.fitted = logit.5$fitted.values                                     # Obtained fitted probabilities
euro_complete$logit.5.fitted.binary = ifelse(euro_complete$logit.5.fitted >= 0.5, 1, 0)  # Convert probabilities to binary
euro_complete_check_5 = euro_complete$MALE - euro_complete$logit.5.fitted.binary         # Difference btw actual vs fitted (binary)
table(euro_complete_check_5)                                                             # Percent correctly predicted

### Complete model ###
logit.6 = glm(data = euro_complete, MALE ~ FHDSI + BILIAC + FDML + SACRAL.ML1 + T50AP + TMAXLN, family = "binomial")
summary(logit.6)
pR2(logit.6)
euro_complete$logit.6.fitted = logit.6$fitted.values                                     # Obtained fitted probabilities
euro_complete$logit.6.fitted.binary = ifelse(euro_complete$logit.6.fitted >= 0.5, 1, 0)  # Convert probabilities to binary
euro_complete_check_6 = euro_complete$MALE - euro_complete$logit.6.fitted.binary         # Difference btw actual vs fitted (binary)
table(euro_complete_check_6)                                                             # Percent correctly predicted

# Percent correctly predicted
percent_correct = rbind(table(euro_complete_check_1), table(euro_complete_check_2),
                        table(euro_complete_check_3), table(euro_complete_check_4),
                        table(euro_complete_check_5), table(euro_complete_check_6))

# Create regression output tables
stargazer(logit.1, logit.2, logit.3)
stargazer(logit.4, logit.5, logit.6)

# Check for multicollinearity
cor(euro_complete[,1:7])

# Likelihood ratio test
anova(logit.1, logit.2, test ="Chisq")


lm(
  as.formula(paste(colnames(mydata)[1], "~",
                   paste(colnames(mydata)[c(2, 3)], collapse = "+"),
                   sep = ""
  )),
  data=euro
)
