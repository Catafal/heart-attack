file_path = "C:/Users/lluis.cerda/Downloads/heart.csv"
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

df$thal[which(df$thal==3)] <- NA

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
  result <- sum(is.na(data))
}

# Finding all Extreme Outliers by Columns
df2 <- Filter(is.numeric, df)
pos <- lapply(df2, FindExtremeOutliers)
lengths(pos)

# Finding all Mild Outliers by Columns
df2 <- Filter(is.numeric, df)
pos <- lapply(df2, FindMildOutliers)
lengths(pos)

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

# Finding all Missing Values by Columns
pos <- lapply(df, FindMissingValues)
pos[pos > 0]


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

library(FactoMineR)
condes(df, 4)

#trestbps & age -----> Significant positive correlation
#trestbps & oldpeak ->
#trestbps & chol ---->
#trestbps & ca ------>
#trestbps & target -->

