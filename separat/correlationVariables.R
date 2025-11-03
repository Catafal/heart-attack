file_path = "C:/Users/jordi.catafal/Desktop/heart.csv"
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
condes(df, 1)
# age & sex -> There is a almosts nonexistent dependance between these two variables, since p-value < 0.05
# age & cp -> There is a almosts nonexistent dependance between these two variables, since p-value < 0.05
# age & trestbps -> There is slight positive correlation between the age and trestbps,since p-value < 0.05
# age & chol -> There is slight positive correlation between the age and chol,since p-value < 0.05
# age & fbs -> There is a slight dependance between these two variables, since p-value < 0.05
# age & restecg -> There is a almosts nonexistent dependance between these two variables, since p-value < 0.05
# age & thalach -> There is a near moderate negative correlation between these variables, since p-value < 0.05
# age & exang -> Exang has no effect on the va  lues of age, p-value > 0.05
# age & oldpeak -> There is slight positive correlation between the age and oldpeak ,since p-value < 0.05
# age & slope -> There is a almosts nonexistent dependance between these two variables, since p-value < 0.05
# age & ca -> There is slight positive correlation between the age and ca,since p-value < 0.05
# age & thal ->  There is a almosts nonexistent dependance between these two variables, since p-value < 0.05
# age & target -> There is a slight negative correlation between these variables, since p-value < 0.05

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




