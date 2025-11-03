file_path = "C:/Users/tomas/Downloads/rawData/heart.csv"
df = read.csv(file_path, header = T)

head(df)

boxplot(df$cp)
a1<-as.numeric(df$cp)
which(a1 == 1)

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
df$ca <- as.numeric(df$ca)

sapply(df, class)

df$thal[which(df$thal==3)] <- NA # all errors detected marked as NA

miss_val = sum(is.na(df))

miss_val

which(is.na(df))

plot(df$thalach)
hist(df$oldpeak)

summary(df)
sapply(df, class)


FindMildOutliers <- function(data) {
  lowerq = quantile(data)[2]
  upperq = quantile(data)[4]
  iqr = upperq - lowerq #Or use IQR(data)
  # we identify extreme outliers
  extreme.threshold.upper = (iqr * 1.5) + upperq
  extreme.threshold.lower = lowerq - (iqr * 1.5)
  result <- which(data > extreme.threshold.upper | data < extreme.threshold.lower)
}

FindExtremeOutliers <- function(data) {
  lowerq = quantile(data)[2]
  upperq = quantile(data)[4]
  iqr = upperq - lowerq #Or use IQR(data)
  # we identify extreme outliers
  extreme.threshold.upper = (iqr * 3) + upperq
  extreme.threshold.lower = lowerq - (iqr * 3)
  result <- which(data > extreme.threshold.upper | data < extreme.threshold.lower)
}


FindMissingValues <- function(data) {
  result <- which(sum(is.na(data)) > 0)
}

# Finding all Extreme Outliers by Columns
df2 <- Filter(is.numeric, df)
pos <- lapply(df2, FindExtremeOutliers)
lengths(pos)

num_outliers = length(pos)

# Finding all Mild Outliers by Columns
df2 <- Filter(is.numeric, df)
pos <- lapply(df2, FindMildOutliers)
lengths(pos)

num_outliers = length(pos) + num_outliers 

# Finding all Missing Values by Individuals
pos <- apply(df, 1, FindMissingValues)
pos <- which(pos > 0)
pos

# Finding all Extreme Outliers by Individuals
df2 <- Filter(is.numeric, df)
pos <- apply(df2, 1, FindExtremeOutliers)
pos <- which(pos > 0)
pos

# Finding all Mild Outliers by Individuals
df2 <- Filter(is.numeric, df)
pos <- apply(df2, 1, FindMildOutliers)
pos <- which(pos > 0)
pos
length(pos)


boxplot(df$chol)
boxplot(df$fbs)
boxplot(df$restecg)

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

# Create variable adding the total number missing values, outliers and errors.

num_outliers_miss_errors = miss_val + num_outliers
num_outliers_miss_errors


# Compute the correlation with all other variables. Rank these variables according the correlation
library(FactoMineR)

# Correlation with age and all other variables
condes(df, 1, proba=0.05)
# age & sex -> There is a almost nonexistent dependence between these two variables, since p-value < 0.05
# age & cp -> There is a almost nonexistent dependence between these two variables, since p-value < 0.05
# age & trestbps -> There is slight positive correlation between the age and trestbps,since p-value < 0.05
# age & chol -> There is slight positive correlation between the age and chol,since p-value < 0.05
# age & fbs -> There is a slight dependence between these two variables, since p-value < 0.05
# age & restecg -> There is a almost nonexistent dependence between these two variables, since p-value < 0.05
# age & thalach -> There is a near moderate negative correlation between these variables, since p-value < 0.05
# age & exang -> There is a almost nonexistent dependence between these two variables, since p-value < 0.05
# age & oldpeak -> There is slight positive correlation between the age and oldpeak ,since p-value < 0.05
# age & slope -> There is a almost nonexistent dependence between these two variables, since p-value < 0.05
# age & ca -> There is slight positive correlation between the age and ca,since p-value < 0.05
# age & thal ->  Thal is not significant
# age & target -> There is a slight negative correlation between these variables, since p-value < 0.05


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


# Correlation with cp and all other variable
catdes(df, 3)
#cp & age -> Age has a small effect in the value of cp, since p-value < 0.05
#cp & sex -> 83.11% of Asymptomatic are Male and the other 16.88% are Female. 
#            Sex has no effect on the people who has Atypical Angina.
#            61.62% of Non-anginal Pain are Male and the other 38.38% are Female.
#            73.24% of Typical Angina are Male and the other 26.76% are Female.
#cp & trestbps -> Trestbps has a medium effect in the value of cp, since p-value < 0.05
#cp & chol -> Chol has no effect in cp's value, since p-value > 0.05
#cp & fbs -> Fbs has no effect on the people who is Asymtomatic.
#            90.4% of Atypical Angina has fbs<=120 mg/dL and the other 9.58% has fbs>120 mg/dL
#            19.36% of Non-anginal Pain has fbs > 120 mg/dL and the other 80.63% has fbs<=120 mg/dL
#            Fbs has no effect on the people who has Typical Angina.
#cp & restecg -> 
#cp & thalach -> Thalach has a large effect in the value of cp, since p-value < 0.05
#cp & exang -> 
#cp & oldpeak -> Oldpeak has a large effect in the value of cp, since p-value < 0.05
#cp & slope -> 
#cp & ca -> Ca has a medium effect in the value of cp, since p-value < 0.05
#cp & thal -> 
#cp & target -> Target has a large effect in the value of cp, since p-value < 0.05


condes(df, 5)
#chol & age -> There is a slight positive correlation between the chol and age,since p-value < 0.05
#chol & sex -> There is a almost nonexistent dependence between these two variables, since p-value < 0.05
#chol & cp -> Only the category Typical Angina has a positive (or significant) impact in the mean of chol
#chol & trestbps -> There is a non to a slight positive correlation between these variables, since p-value < 0.05
#chol & fbs -> Fbs is not significant
#chol & restecg -> There is a almost nonexistent dependence between these two variables, since p-value < 0.05
#chol & thalach -> Thalach is not significant
#chol & exang -> There is a almost nonexistent dependence between these two variables, since p-value < 0.05
#chol & oldpeak -> There is a almost nonexistent correlation between these two variables, since p-value < 0.05
#chol & slope -> Only the category Flat has a positive (or significant) impact in the mean of chol
#chol & ca -> There is a almost nonexistent correlation between these two variables, since p-value < 0.05
#chol & thal -> There is a almost nonexistent dependence between these two variables, since p-value < 0.05
#chol & target -> There is a non to slight negative correlation between these two variables, since p-value < 0.05

condes(df, 8)
#thalac & age -> There is a slight negative correlation between the thalac and age,since p-value < 0.05
#thalac & sex -> Sex is not significant
#thalac & cp -> There is a almost nonexistent dependence between these two variables, since p-value < 0.05
#thalac & trestbps -> Trestbps is not significant
#thalac & chol -> Thalach is not significant
#thalac & fbs -> Fbs is not significant
#thalac & restecg -> There is a almost nonexistent dependence between these two variables, since p-value < 0.05
#thalac & exang -> There is a almost nonexistent dependence between these two variables, since p-value < 0.05
#thalac & oldpeak -> There is a slight negative correlation between these two variables, since p-value < 0.05
#thalac & slope -> There is a almost nonexistent dependence between these two variables, since p-value < 0.05
#thalac & ca -> There is a slight negative correlation between these two variables, since p-value < 0.05
#thalac & thal -> There is a almost nonexistent dependence between these two variables, since p-value < 0.05
#thalac & target -> There is a non to slight negative correlation between these two variables, since p-value < 0.05

catdes(df, 9)
# exang & age -> Age has non to small effect in the value of exang, since p-value > 0,05
# exang & sex -> Exang=NO:35.00% are Female, and the other 65.00% are Male.
#                Exang=YES: 78.55% are Male, and the other 21.45% are Female.
# exang & cp -> Exang=NO: 36.32% have Non-anginal Pain, 22.79% have Atypical Angina, 9.41% are Asymptomatic, and
#               31.47% have Typical Angina.
#               Exang=YES: 82.03% have Typical Angina, 3.77% are Asymptomatic, 3.48% have Atypical Angina, and 
#               10.72% have Non-anginal Pain
# exang & trestbps -> Trestbps has no effect in the value of exang, since p-value > 0,05
# exang & chol -> Chol has non to small effect in the value of exang, since p-value < 0,05
# exang & fbs -> Fbs is not significant.
# exang & restecg -> Exang=NO:53.38% have Abnormality, 45.59% are Normal and the rest is not significant.
#                    Exang=YES:54.2% are Normal, 43.48% have ABnormality and the rest is not significant.
# exang & thalac -> Thalac has a large effect in the value of exang, since p-value < 0,05
# exang & oldpeak -> Oldpeak has medium to large effect in the value of exang, since p-value > 0,05
# exang & slope -> Exang=NO: 56.18% have Downsloping, 37.94% have Flat, and 5.88% have Upsloping
#                  Exang=YES: 64.93% have Flat, 9.9% have Upsloping, and 25.22% have Downsloping 
# exang & ca -> Ca has a small effect in the value of exang, since p-value < 0,05
# exang & thal -> Exang=NO:92.5% have Reversible Defect, 7.05% have Fixed Defect
#                 and the rest is not significant.
#                 Exang=YES: 15.07% have Fixed Defect, 83.77% have Reversible Defect and the rest is not significant
# exang & target -> Target has a large effect in the value of exang, since p-value < 0,05


#Imputation of factor thal.

summary(df)

library(missMDA)
# Categorical imputation 
vars_dis<-names(df)[c(11,13)]

summary(df[,vars_dis])
res.input<-imputeMCA(df[,vars_dis],method="EM")
summary(res.input$completeObs)

# Validation is COMPULSORY
barplot(table(df$thal),col="red")
barplot(table(res.input$completeObs[,1]),col="blue")

df[,vars_dis] <- res.input$completeObs
summary(df)


# Heat Map correlacions
df2 <- Filter(is.numeric, df)
df2 <- scale(df2)
cormat <- round(cor(df2),2)
library(reshape2)
melted_cormat <- melt(cormat)

library(ggplot2)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()


### Profiling
# Categorical output:
library(FactoMineR)
summary(df$target)
res.condes <- condes(df, 14, proba = 0.50)
res.condes$quanti  # Global association to numeric variables
res.condes$quali # Partial association of numeric variables to levels of outcome factor
res.condes$category  # Partial association to significative levels in factors


