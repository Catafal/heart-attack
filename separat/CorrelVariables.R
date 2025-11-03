# Load data


rm(list=ls())

library(AER)
library(car)
library(FactoMineR)



file_path = "C:/Users/tomas/Downloads/rawData/heart.csv"
df = read.csv(file_path, header = T)

head(df)


df$sex[which(df$sex==0)] <- "Female"
df$sex[which(df$sex==1)] <- "Male"
df$sex <- as.factor(df$sex)

df$cp[which(df$cp==0)] <- "Typical Angina"
df$cp[which(df$cp==1)] <- "Atypical Angina"
df$cp[which(df$cp==2)] <- "Non-anginal Pain"
df$cp[which(df$cp==3)] <- "Asymptomatic"
df$cp <- as.factor(df$cp)

df$fbs[which(df$fbs==0)] <- "<= 120 mg/dL"
df$fbs[which(df$fbs==1)] <- "> 120 mg/dL"
df$fbs <- as.factor(df$fbs)

df$restecg[which(df$restecg==0)] <- "Normal"
df$restecg[which(df$restecg==1)] <- "Abnormality"
df$restecg[which(df$restecg==2)] <- "Hypertrophy"
df$restecg <- as.factor(df$restecg)

df$exang[which(df$exang==0)] <- "No"
df$exang[which(df$exang==1)] <- "Yes"
df$exang <- as.factor(df$exang)

df$slope[which(df$slope==0)] <- "Upsloping"
df$slope[which(df$slope==1)] <- "Flat"
df$slope[which(df$slope==2)] <- "Downsloping"
df$slope <- as.factor(df$slope)

df$thal[which(df$thal==0)] <- "Normal"
df$thal[which(df$thal==1)] <- "Fixed Defect"
df$thal[which(df$thal==2)] <- "Reversible Defect"
df$thal <- as.factor(df$thal)



head(df)

hist(df$age)

plot(df$sex)

summary(df)

sapply(df, class)

df$age <- as.numeric(df$age)
df$trestbps <- as.numeric(df$trestbps)
df$chol <- as.numeric(df$chol)
df$thalach <- as.numeric(df$thalach)
df$oldpeak <- as.numeric(df$oldpeak)

sapply(df, class)

df$thal[which(df$thal==3)] <- NA

miss_val = sum(is.na(df))

miss_val

which(is.na(df))

plot(df$thalach)
hist(df$oldpeak)

summary(df)
sapply(df, class)


##Markdown

##kable -> taula

## Univariate detection


outliers <- function(column){
  
  sumlist <- summary(column)
  q1 <- sumlist[2] 
  q3 <- sumlist[5]
  
  
  boxplot(column, main = "Boxplot ", col = "orange", horizontal = T)
  
  
  # IQR calculation
  iqr <- q3 - q1 
  
  # Mild inferior limit:
  mild_inf_lim <- sumlist[2]-1.5*iqr
  # Extreme inferior limit:
  extreme_inf_lim <- sumlist[2]-3*iqr
  abline(v=mild_inf_lim, col = "red", lty = 2)
  abline(v=extreme_inf_lim, col = "red",  lty = 2, lwd = 2)
  mild_sup_lim <- sumlist[5]+1.5*iqr
  extreme_sup_lim <- sumlist[5]+3*iqr
  abline(v=mild_sup_lim, col = "red", lty = 2)
  abline(v=extreme_sup_lim, col = "red",  lty = 2, lwd = 2)
  
}

outliers(df$age)
outliers(df$trestbps)
outliers(df$chol)
outliers(df$thalach)
outliers(df$oldpeak)
outliers(df$ca)
outliers(df$target)

library(FactoMineR)

#Correlation with sex and all other variables
catdes(df, 2)
# sex & age -> Age has a no effect in the value of sex, since p-value > 0,05
# sex & cp -> 34.93% of Female have Non-anginal Pain, 42.63% of Female have Typical Angina,
#             and 4.17% are Asymptomatic.
#             8.98% of Male are Asymptomatic, 51.05% of Male have Typical Angina, and 24.54%
#             of Male have Non-anginal Pain.
# sex & trestbps -> Trestbps has no effect in the value of sex, since p-value > 0,05
# sex & chol -> Chol has a small to medium effect in the value of sex, since p-value < 0,05
# sex & fbs -> Fbs is not significant.
# sex & restecg -> 3.53% of Female have Hypertrophy, and the rest are not significant.
#                  0.56% of Male have Hypertrophy, and the rest are not significant.
# sex & thalac -> Thalac is not significant.
# sex & exang -> 76.28% of Female have the value of NO, and the other 23.72% have a value of YES.
#                38.01% of Male have a value of YES, and the other 61.99% have a value of NO.
# sex & oldpeak -> Oldpeak has no effect in the value of sex, since p-value > 0,05
# sex & slope -> Slope is not significant.
# sex & ca -> Ca has a small effect in the value of sex, since p-value < 0,05
# sex & thal -> 80% of Female have Reversible Defect, 1.28% of Female have Fixed Defect
#               and 16.99% have no value.
#               50.07% of Male have no value, 8.42% of Male have Fixed Defect, 40.95% of
#               Male have Reversible Defect.
# sex & target -> Target has a medium effect in the value of sex, since p-value < 0,05
