install.packages("caret",
                 repos = "http://cran.r-project.org", 
                 dependencies = c("Depends", "Imports", "Suggests"))
install.packages("Rcpp")
install.packages("ROCR")
install.packages("pROC")

library(caret)
library(ROCR)
library(pROC)


heart.data <- read.csv("heart.csv") 

#Function needed to convert classes of predictor values

convert.AttributeFactor <- function(obj,types){
    for (i in 1:length(obj)){
        FUN <- switch(types[i],character = as.character, 
                                   numeric = as.numeric, 
                                   factor = as.factor)
        obj[,i] <- FUN(obj[,i])
    }
    obj
}

convert.names <- function(row){
  row=gsub("sex1", "male", row)
  row=gsub("thal7", "reversable defect thalassemia", row)
  row=gsub("thal6", "fixed defect thalassemia", row)
  row=gsub("cp4", "asymptomatic chest pain", row)
  row=gsub("cp3", "non-anginal chest pain", row)
  row=gsub("cp2", "atypical angina chest pain", row)
  row=gsub("oldpeak", "ST depression from exercise", row)
  row=gsub("thalach", "maximum heart rate achieved", row)
  row=gsub("trestbps", "resting blood pressure", row)
  row=gsub("ca2", "2 major vessels col/b fluoro., ca2", row)
  row=gsub("ca1", "1 major vessel col/b fluoro., ca1", row)
  row=gsub("slope2", "flat peak exercise ST segment", row)
  row=gsub("slope1", "upsloping peak exercise ST segment", row)
  row=gsub("slope3", "downsloping peak exercise ST segment", row)
  row=gsub("chol", "serum cholestoral", row)
  row=gsub("exang", "exercise induced angina", row)
  row=gsub("restecg2", "restec: showing left ventricular hypertrophy
                      by Estes criteria", row)
  row=gsub("restecg1", "restec: having ST-T wave abnormality", row)
  row=gsub("fbs1", "fasting blood sugar > 120 mg/dl", row)
  }


#adding_the_dataset_attributes_name

names(heart.data) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg",
                   "thalach","exang", "oldpeak","slope", "ca", "thal", "num")

heart.data <- convert.AttributeFactor(heart.data,chclass)

#Check for missing values

s = sum(is.na(heart.data))
heart.data <- na.omit(heart.data)
#str(heart.data)


#Create_Train_Test_DATASET

library(caret)

#Random_number_generator


x <- sample(1:10, 1)
set.seed(x)

#Create_traindata
data1<-heart.data
train <- sample(nrow(data1), 0.05*nrow(data1), replace = FALSE)
TrainSet <- data1[train,]
tempData<- data1[-train,]

#Create_testadata
trainV <- sample(nrow(tempData), 0.001052632*nrow(tempData), replace = FALSE)
ValidSet <- tempData[trainV,]


trainData <- TrainSet
testData <-  ValidSet



#Creating_list_for_Storing_AUC_&_Accuracy
AUC = list()
Accuracy = list()
