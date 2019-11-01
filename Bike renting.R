###Bike renting ###

#Remove all the objects stored 
rm(list = ls())

#Set working directory
setwd("D:/Data Scientist/Project/Bike Renting")

#To check current working directory
getwd()

#Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees', 'readxl')

#install.packages(x)
lapply(x,require,character.only = TRUE)
rm(x)

# Load the data 
Bike = read.csv("day.csv", header = T )

###Exploratory Data Analysis
#Understand the data 
str(Bike)
colnames(Bike)
unique(Bike$season)
unique(Bike$yr)
str(Bike$workingday)
class(Bike$season)
names(Bike)
length(names(Bike))

cnames = names(Bike)

for (i in cnames) {
  print(i)
  print(length(unique(Bike[,i])))
}

##Conversion of data type

num_names = c("instant","temp","atemp","hum","windspeed","casual","registered","cnt")


for (i in num_names) {
  Bike[,i] = as.numeric(Bike[,i])
}

cat_names = c("season","yr","mnth","holiday","weekday","workingday","weathersit")

for (i in cat_names) {
  Bike[,i] = as.factor(as.character(Bike[,i]))
}

Bike$dteday = as.Date(Bike$dteday)

str(Bike)

##Missing value analysis

sum(is.na(Bike))
## Data is free from missing value so no need to do any action.

### Outlier analysis

# ## BoxPlots - Distribution and Outlier Check
numeric_index = sapply(Bike,is.numeric) #selecting only numeric

numeric_data = Bike[,numeric_index]

cnames = colnames(numeric_data)

for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "cnt"), data = subset(Bike))+
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="cnt")+
           ggtitle(paste("Box plot of responded for",cnames[i])))
}
##Ploting graph 
gridExtra::grid.arrange(gn1,gn2,ncol=2)
gridExtra::grid.arrange(gn3,gn4,ncol=2)
gridExtra::grid.arrange(gn5,gn6,ncol=2)
gridExtra::grid.arrange(gn7,gn8,ncol=2)

#___  Getting the outliers data element from each variable  _________

for (i in num_names) {
  print(i)
  val = Bike[,i][Bike[,i] %in% boxplot.stats(Bike[,i])$out]
  print(length(val))
  print(val)
}

#Get the positions of outliers in respective variables

which(Bike$hum %in% boxplot.stats(Bike$hum)$out) #2 outliers
which(Bike$windspeed %in% boxplot.stats(Bike$windspeed)$out)  #13 outliers
which(Bike$casual %in% boxplot.stats(Bike$casual)$out)  #44 outliers

## 59 total outliers present in data
## Replacing the outliers with caping##
for (i in cnames) {
    qnt =quantile(Bike[ ,i], probs=c(.25, .75), na.rm = T)
    H <- 1.5 * IQR(Bike[ ,i], na.rm = T)
    Bike[ ,i][Bike[ ,i] < (qnt[1] - H)] = qnt[1]
    Bike[ ,i][Bike[ ,i] > (qnt[2] + H)] = qnt[2]
summary(Bike[ ,i])
}


#Confirm again for missing values
sum(is.na(Bike))  ## 0

#Confirm again if any outlier exists

for (i in num_names) {
  val = Bike[,i][Bike[,i] %in% boxplot.stats(Bike[,i])$out]
}

length(val)

#Till here we have nor any missing value neither any outliers.

####Feature Selection 
#Anova test for Catogrical variables
av_test = aov(cnt ~   season + yr + mnth + holiday + workingday + weekday  +weathersit , data = Bike)
summary(av_test)

# correlation for continuous variables
corrgram(Bike[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

#VIF test
library(usdm)
vif(numeric_data)
vifcor(numeric_data,th=0.9)

clean_Bike = subset(Bike,select = -c(instant,temp, casual, registered))

#___ THE DATA HERE WE GOT IN DATA FRAME clean_Bike IS ALREADY NORMALIZED.

#Save the clean data after all per-procesing of the data 
write.csv(clean_Bike,"Clean_Bike_R.csv", row.names = F)

rmExcept("clean_Bike")
### DATA Sampling 

#Divide data into train and test data to apply in ML algorithim 
train_index = sample(1:nrow(clean_Bike), 0.80* nrow(clean_Bike))

train = clean_Bike[train_index,]
test = clean_Bike[-train_index,]

#### Model development
### As the target variable is continuous variable, we have to chose regression model

## Creating mape function
mape = function(y, yhat){
  mean(abs((y - yhat)/y))*100
}

##  Decision tree regression model development
#accuracy = 79.14
#MAPE = 20.86
dt_model = rpart(cnt ~. , data = train, method = "anova")

#Predict for new test cases 
predict_dt = predict(dt_model,test[,-12])
#install.packages("Metrics")
library(Metrics)
#Checking error rate
mape(test[,12], predict_dt)# 20.86

####Random forest 
#n 100 
#accuracy = 85.88
#MAPE = 14.12 
#Develop model on train data 
rf_model = randomForest(cnt ~.,train, ntree = 100)

#Predict test data using random forest model
rf_predict = predict(rf_model, test[,-12])

mape(test[,12], rf_predict) #14.12


#n=200
#accuracy = 85.73
#MAPE =  14.27
#Develop model on train data
rf_model = randomForest(cnt ~.,train, ntree = 200)

#Predict test data using random forest model
rf_predict = predict(rf_model, test[,-12])

#Calculating MAPE
mape(test[,12], rf_predict) #14.27


#n=400
#accuracy = 85.71
#MAPE =  14.29
#Develop model on train data
rf_model = randomForest(cnt ~.,train, ntree = 400)

#Predict test data using random forest model
rf_predict = predict(rf_model, test[,-12])

mape(test[,12], rf_predict)# 14.29


#####Linear Regression 
#mape = 15.95
#Accuracy = 84.05
lr_model = lm(cnt ~., data = train)

#summary of the model 

summary(lr_model)
# R-squred : 0.8547
# Adjusted R-squared : 8474
# conclusion: The linear regression explains that 85% of the target variable is explained by the all the independent variables.  

##Predict 

Predict_lr = predict(lr_model, test[,1:12])
##Calculate MAPE
mape(test[,12], Predict_lr) #15.956



