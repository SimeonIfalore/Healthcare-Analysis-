# importing the Data set 
getwd()
setwd("C:/Users/Simeon/desktop")
library(readxl)
HospitalCost <- read_excel("HospitalCost.xlsx")
View(HospitalCost)

# 1.	To find the age category of people who frequently visit the hospital and has the maximum expenditure.

summary(as.factor(HospitalCost$AGE))
hist(HospitalCost$AGE, main = "Histogram of Age Group and their hospital visits", xlab = "Age group", border = "black", col = c("light blue", "dark blue"), xlim = c(0,20), ylim = c(0,350))


# Summarize expenditure based on age group
ExpenseBasedOnAge <- aggregate(TOTCHG ~ AGE, FUN=sum, data = HospitalCost)

which.max(tapply(ExpenseBasedOnAge$TOTCHG, ExpenseBasedOnAge$TOTCHG, FUN=sum))

barplot(tapply(ExpenseBasedOnAge$TOTCHG, ExpenseBasedOnAge$AGE, FUN=sum), main = "Age group and Expenditure", border = "black", col = c("light green", "dark green"))
         
#2.	Diagnosis-related group that has maximum hospitalization and expenditure.

summary(as.factor(HospitalCost$APRDRG))
DiagnosisCost <- aggregate(TOTCHG ~ APRDRG, FUN = sum, data = HospitalCost)
DiagnosisCost[which.max(DiagnosisCost$TOTCHG), ]

#3.	To make sure that there is no malpractice, 
#the agency needs to analyze if the race of the patient is related to the hospitalization costs.

summary(as.factor(HospitalCost$RACE))
HospitalCost <- na.omit(HospitalCost)

# Once the NA variable is removed the model is built using linear regression to see if race has any influence on hospital cost as shown below. 
summary(as.factor(HospitalCost$RACE))
raceInflModel <- lm(TOTCHG ~ RACE, data = HospitalCost)
summary(raceInflModel)

# 4.	To properly utilize the costs, the agency has to analyze the severity of the hospital costs by age and gender for the proper allocation of resources.

Age_Gender <- as.factor((HospitalCost$FEMALE))
summary(Age_Gender)

# We can see that from the summary the gender between male and female is evenly distributed. 
Age_GenderInflModel <- lm(formula = TOTCHG ~ AGE + FEMALE, data = HospitalCost)
summary(Age_GenderInflModel)

# 5.	Since the length of stay is the crucial factor for inpatients, the agency wants to find if the length of stay can be predicted from age, gender, and race.

Age_Gender_Race_InflModel <- lm(formula = LOS ~ AGE + FEMALE + RACE, data = HospitalCost)
summary(Age_Gender_Race_InflModel)

# 6.	To perform a complete analysis, the agency wants to find the variable that mainly affects hospital costs.
HospCostModel <- lm(formula = TOTCHG ~ ., data = HospitalCost) 
summary(HospCostModel)
