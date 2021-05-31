# loading packages
library(readr)

# importing data set
cervical_cancer <- read_csv("kag_risk_factors_cervical_cancer.csv")

# checking data set
head(cervical_cancer)
names(cervical_cancer)
str(cervical_cancer)
