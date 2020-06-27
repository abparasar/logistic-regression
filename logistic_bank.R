#--------Import the libraries--------------
# for impute function to replace NA/ missing value treatment
library(Hmisc)

# for scatter plot and correlation
library(GGally)

#for vif function( variable inflation factor)
library(car)

# ------- read the csv data -----------------
getwd()
#setwd("C:/Users/HP/Desktop/edu/Logistic-Regression-PBA-20200411T144951Z-001/Logistic-Regression-PBA/data")
# read data from csv file
bank_data = read.csv("bank-full.csv")
View(bank_data)
str(bank_data)
summary(bank_data)

#-----------MIssing Value treatment-------------
#imptue the NA values in age column with mean
bank_data$age = as.numeric(impute(bank_data$age, mean))
#imptue the NA values in balance column with mean
bank_data$balance = as.numeric(impute(bank_data$balance, mean))

summary(bank_data)

#------------Outliers treatment (Do for all variable)-----------
#stats function to see outliers
boxplot.stats(bank_data$age)$out
boxplot(bank_data$age)

x <- bank_data$age
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
qnt[1]
qnt[2]
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
caps[1]
caps[2]
H <- 1.5 * IQR(x, na.rm = T)
H
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]

bank_data$age =  x

summary(bank_data)

# optional

boxplot(bank_data$age~bank_data$y,xlab="Product Purchase",ylab="age",col=c("pink","lightblue"), main="Exploratory Data Analysis Plot\n of Purchase Versus Age")
boxplot(bank_data$age~bank_data$y)


#---------------- check correlation----------------
#library(GGally)
#ggpairs(data=bank_data[,sapply(bank_data,is.numeric)], title="bank data")
library(dplyr)

#convert y colum. yes to 1 and no 0  because we will be dealing in probability of product purchase(y)
bank_data= bank_data %>% mutate(y = ifelse(y == "yes", 1, 0))

tail(bank_data)

#picking out numeric data out of bank data so that we can calculate collenearitty
bank_data_numeric = bank_data[,sapply(bank_data,is.numeric)]
bank_data_numeric
summary(bank_data_numeric)
head(bank_data_numeric)

correlation_matrix = cor(bank_data_numeric)
correlation_matrix
write.csv( correlation_matrix,'correlation.csv')

#---------------- multicollinearity -----------------
bank_lm_model<-lm(y~., data=bank_data_numeric)
# using the library car for calculating multicollinearity
vif(bank_lm_model)

# no multicollinearity here( all values less than 2)

#---------------dummy variable---------------
library(caret)
summary(bank_data)
dmy <- dummyVars(" ~ job+marital+education+poutcome+contact+month", data = bank_data,fullRank = T)
dmy
bank_data_transformed <- data.frame(predict(dmy, newdata = bank_data))

head(bank_data_transformed)

summary(bank_data_transformed)

final_data<-cbind(bank_data,bank_data_transformed)
str(final_data)

final_data<-select(final_data,-c(job,marital,contact,education,poutcome,month))
summary(final_data)

#-------------- test and learn data ---------------
smp_size = 0.75 * nrow(final_data)
set.seed(123)
train_ind <- sample(seq_len(nrow(final_data)), size = smp_size)
train <- final_data[train_ind, ]
test <- final_data[-train_ind, ]

#--------------model creation----------
# build linear regression model on train data.
logit_model <- glm(y ~ ., data = train, family = "binomial")
# Check the model output
summary(logit_model)


#---------Remove variable with low p value one by one VVIP ------------------
log_model <- glm(y ~ ., data=select(train, -c(age,marital.single,default,balance,housing,contact.telephone,pdays,previous,job.management,job.self.employed, job.unknown,poutcome.unknown,poutcome.unknown,job.unemployed,job.management)),family = "binomial")
summary(log_model)

# ------------predict target variable/ scoring -----------------------
predictions<-predict(log_model, test,type = "response" )
tail(predictions)

# ---------------- Evaluation/ Validation ----------------------------
combined_data<-cbind(test,predictions)
combined_data

#cutoff is .1 so that increase the sensitivity
combined_data$response <- as.factor(ifelse(combined_data$predictions>0.1, 1, 0))
str(combined_data)

tail(combined_data)

#confusion matrix
conf_matrix<-confusionMatrix(combined_data$response,factor(combined_data$y),positive = "1")
conf_matrix














