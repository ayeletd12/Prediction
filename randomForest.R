#----------------------clean enviroment and consol -------------------------------------------------------------
rm(list=ls(all.names=TRUE))
cat("\014")  # or ctrl + L  # clear Console\

#------------------load this packeges,if miss one, please install first

library(ggplot2)
library(readxl)

library(dplyr)

library(VIM)
library(DT)

library(corrplot)
library(nlme)
library(lme4)

library(data.table)

#--------------upload data --------------
setwd("D:") #manualy update

upload_jobs <- read_excel("Dataset.xlsx", sheet = "Test_Jobs")
JobTitles <- read_excel("Dataset.xlsx", sheet = "Test_JobTitles")
Categories <- read_excel("Dataset.xlsx", sheet = "Test_Categories")



Jobs <- upload_jobs
setDT(Jobs)
#------------------Descriptive Statistic ----------------
# Exploring each feature, missing values, integrity issues, distribution
summary(Jobs)
str(Jobs)
set.seed(101) # Set Seed so that same sample can be reproduced in future also

all_features <- colnames(Jobs)
#all_features <- all_features[-y_index] # Y var

numeric_features_index <- c(6,8,9)#---input by indexes
numeric_features <- all_features[numeric_features_index]
categorial_features <- all_features[-numeric_features_index]

#-----------Job Title ID ---------------------
barplot(prop.table(table(Jobs[["JobTitleId"]])), main = "JobTitles")
JobTitles

Jobs[which(Jobs$JobTitleId=="12"),]
Jobs <- Jobs[-c(which(Jobs$JobTitleId=="12")),]

Jobs$JobTitleId <- factor(Jobs$JobTitleId)

levels(Jobs$JobTitleId) <- JobTitles$JobTitleId
levels(Jobs$JobTitleId)
barplot(prop.table(table(Jobs[["JobTitleId"]])), main = "JobTitles")

#-----------Category ID ---------------------
barplot(prop.table(table(Jobs[["CategoryId"]])), main = "Category Id")
Categories

Jobs[which(Jobs$CategoryId=="5"),]
Jobs <- Jobs[-c(which(Jobs$CategoryId=="5")),]

barplot(prop.table(table(Jobs[["CategoryId"]])), main = "Category Id")
Jobs$CategoryId <- factor(Jobs$CategoryId)

levels(Jobs$CategoryId) <- Categories$CategoryId
barplot(prop.table(table(Jobs[["CategoryId"]])), main = "Category Id")

#-----------City integrity issues -----------------
barplot(prop.table(table(Jobs[["City"]])), main = "City")

levels(factor(Jobs[["City"]]))
Jobs[which(Jobs$City=="New York"), ] 
nrow(Jobs[which(Jobs$City=="New York"), ] )

Jobs[which(Jobs$City=="New-York"), ] 
nrow(Jobs[which(Jobs$City=="New-York"), ] )


Jobs[which(Jobs$City=="New-York"), ("City")] <- "New York"

Jobs$City <- factor(Jobs$City)
levels(Jobs$City) <- c("Binghamton", "Boston","Lodi","Los Angeles", "New York","Newtonville", "Sacramento","Syracuse","West Newton")

barplot(prop.table(table(Jobs[["City"]])), main = "City")
levels(Jobs$City)

#---------Education level integrity issues-------------
#, levels = Categories[["CategoryId"]], lables = Categories[["CategoryId"]])

barplot(prop.table(table(Jobs[["EducationLevel"]])), main = "Education Level")
levels(as.factor(Jobs$EducationLevel))

Jobs[which(Jobs$EducationLevel=="NULL"), ] 
Jobs <- Jobs[-c(which(Jobs$EducationLevel=="NULL")),]

barplot(prop.table(table(Jobs[["EducationLevel"]])), main = "Education Level")

Jobs$EducationLevel <- as.factor(Jobs$EducationLevel)

#---------State integrity issues-------------
barplot(prop.table(table(Jobs[["State"]])), main = "State")
levels(as.factor(Jobs$State))

Jobs[which(Jobs$State=="NJ"), ] 
Jobs[which(Jobs$State=="NJ"), "State"]  <- "NY"

Jobs[which(Jobs$JobId==201), ]
barplot(prop.table(table(Jobs[["State"]])), main = "State")

Jobs$State <- factor(Jobs$State, levels = c("CA", "MA", "NY"))
levels(Jobs$State)
#---------Description Length integrity issues-------------
is.numeric(Jobs[["DescriptionLength"]])
Jobs[is.na(Jobs)] <- 0

hist(Jobs[["DescriptionLength"]], main = "Description Length", freq=FALSE)
lines(density(Jobs[["DescriptionLength"]], na.rm = TRUE),col="red",lwd=2)   

#------------ Clicks integrity issues-------------
is.numeric(Jobs[["Clicks"]])
hist(Jobs[["Clicks"]], main = "Clicks", freq=FALSE)
lines(density(Jobs[["Clicks"]], na.rm = TRUE),col="red",lwd=2)   

Jobs[which(Jobs[["Clicks"]]>1000), ]

Jobs[which(Jobs[["Clicks"]]<1), ]

Jobs <- Jobs[-which(Jobs[["Clicks"]]>1000), ]
Jobs <- Jobs[-which(Jobs[["Clicks"]]<1) ,]

hist(Jobs[["Clicks"]], main = "Clicks", freq=FALSE)
lines(density(Jobs[["Clicks"]], na.rm = TRUE),col="red",lwd=2)   

#----------- Applicants integrity issues-------------
is.numeric(Jobs[["Applicants"]])

which(!grepl('^[0-9]',Jobs[["Applicants"]]))

Jobs[which(Jobs[["Applicants"]]<0),]

Jobs <- Jobs[-which(Jobs[["Applicants"]]<0),]

hist(Jobs[["Applicants"]], main = "Applicants", freq=FALSE)
lines(density(Jobs[["Applicants"]], na.rm = TRUE),col="red",lwd=2)   

Jobs[which(Jobs[["Applicants"]]>80), ]

#---------- Description Length integrity issues-------------

Jobs[which(Jobs[["DescriptionLength"]]<1), ]

#----------more logicals issue ------------------------

Jobs[which(Jobs[ ,  .(noSense=(Clicks<Applicants))]==TRUE), ]



#-------------------------Question 1----------------
# 1.1	Which job has the longest description outside of the state of NY?

ans <- Jobs[Jobs[ ,which(Jobs$State != "NY")], ][which.max(Jobs[["DescriptionLength"]]), ]
ans

# 1.2	Which job title has the highest number of average clicks?

temp <- Jobs[ ,  .(AvgClicks=mean(Clicks)),  by = c("JobTitleId")]
temp
temp[which.max(temp$AvgClicks),]

JobTitles[which.max(temp$AvgClicks),]

# 1.3	Which Category has the highest conversion rate of clicks to applicants?

df <- Jobs[ ,  .(ConversionRate=mean(Clicks / Applicants)),  by = c("CategoryId")]
df
plot(df$ConversionRate)
Categories[which.max(df$ConversionRate), ]



#-----------------------Question 3 ---------------------------------
#predict jobs 1001, 1002 cliack and applicants

#--------splint into train and validation---------

mydataLength<-nrow(Jobs)
SampleTrain<-sample(mydataLength, mydataLength*0.65, replace = FALSE, prob = NULL)
train<-Jobs[SampleTrain,]

mydataTemp<-Jobs[-SampleTrain,]
mydataLength<-nrow(mydataTemp)
SampleValidation<-sample(mydataLength, mydataLength*0.65, replace = FALSE, prob = NULL)
validation<-mydataTemp[SampleValidation,]
test<-mydataTemp[-SampleValidation,]

setDT(train)
#----------------random forest 
library(randomForest)

set.seed(12893)
#------------Applicants
train_rf <- train[, c(2:7, 9)]
validation_rf <- validation[ ,c(2:7, 9)]

str(test)
str(validation_rf)
str(train_rf)

#validation_rf$City <- factor(validation_rf$City )
#levels(validation_rf$City ) <- c("Binghamton", "Boston","Lodi","Los Angeles", "New York","Newtonville", "Sacramento","Syracuse","West Newton")

y_index <- 7 
x_features <- c(1:6)

train_rf$JobTitleId <- as.factor(train_rf$JobTitleId)
train_rf$CategoryId <- as.factor(train_rf$CategoryId)
# find mtry
mtryTrain <-data.frame(matrix(0,ncol = 2,nrow =6))
mtryTrain[,1] <-1:6
colnames(mtryTrain)<-c("mtry","mse")

for(i in 1:6){
  #i<-5
  rf<-randomForest(Applicants ~., data= train_rf , ntree=200, mtry=i)
  print(rf)
  pred <- predict(rf, newdata = validation_rf[ ,..x_features], type='response')
  mtryTrain[i,2] <- with(validation_rf, mean( (Applicants - pred)^2)) #Mean Squared Test Error #sum(pred == validation_rf[ ,..y_index])/nrow(validation_rf)
  
}

maxMtry<-which.min(mtryTrain[,2])
range(mtryTrain[,2])

ggplot(data=as.data.frame(mtryTrain),aes( x=mtry, y=mse)) + xlab("Number of variable on each tree") + ylab("MSE")  + geom_line()


pi<-10
numtree <- seq(10,1000,pi)
mse.RF     <- data.frame(matrix(0,length(numtree),2))
mse.RF[ ,1] <- numtree
colnames(mse.RF) <- c("numtree","mse")

for(trees in numtree){
  rf<-randomForest(Applicants ~., data= train_rf , ntree=trees, mtry = maxMtry)
  print(rf)
  pred <- predict(rf, newdata = validation_rf[ ,..x_features], type='response')
  mse.RF[which(mse.RF[["numtree"]] == trees) ,2] <- with(validation_rf, mean( (Applicants - pred)^2))
}

range(mse.RF$mse)
ntrees <- mse.RF[which.min(mse.RF$mse), 1]
ntrees
ggplot(data=as.data.frame(mse.RF),aes( x=numtree, y=mse)) + xlab("Number of trees in the forest") + ylab("MSE")  + geom_line()

test_rf <- test[,c(2:7)]
testY_rf <- test[,9]

str(test_rf)

rf_applicants<-randomForest(Applicants ~., data= train_rf , ntree=ntrees, mtry = maxMtry)
print(rf)
pred <- predict(rf, newdata = test_rf[ ,..x_features], type='response')
testMSE <- with(testY_rf, mean( (Applicants - pred)^2))
testMSE

#------------Clicks
train_rf <- train[, c(2:7, 8)]
validation_rf <- validation[ ,c(2:7, 8)]

str(test)
str(validation_rf)
str(train_rf)

validation_rf$City <- factor(validation_rf$City )
levels(validation_rf$City ) <- c("Binghamton", "Boston","Lodi","Los Angeles", "New York","Newtonville", "Sacramento","Syracuse","West Newton")

y_index <- 7 
x_features <- c(1:6)

train_rf$JobTitleId <- as.factor(train_rf$JobTitleId)
train_rf$CategoryId <- as.factor(train_rf$CategoryId)
# find mtry
mtryTrain <-data.frame(matrix(0,ncol = 2,nrow =6))
mtryTrain[,1] <-1:6
colnames(mtryTrain)<-c("mtry","mse")

for(i in 1:6){
  #i<-5
  rf<-randomForest(Clicks ~., data= train_rf , ntree=200, mtry=i)
  print(rf)
  pred <- predict(rf, newdata = validation_rf[ ,..x_features], type='response')
  mtryTrain[i,2] <- with(validation_rf, mean( (Clicks - pred)^2)) #Mean Squared Test Error #sum(pred == validation_rf[ ,..y_index])/nrow(validation_rf)
  
}

maxMtry<-which.min(mtryTrain[,2])
range(mtryTrain[,2])

ggplot(data=as.data.frame(mtryTrain),aes( x=mtry, y=mse)) + xlab("Number of variable on each tree") + ylab("MSE")  + geom_line()

maxMtry <- 4
pi<-10
numtree <- seq(10,1000,pi)
mse.RF     <- data.frame(matrix(0,length(numtree),2))
mse.RF[ ,1] <- numtree
colnames(mse.RF) <- c("numtree","mse")

for(trees in numtree){
  rf<-randomForest(Clicks ~., data= train_rf , ntree=trees, mtry = maxMtry)
  print(rf)
  pred <- predict(rf, newdata = validation_rf[ ,..x_features], type='response')
  mse.RF[which(mse.RF[["numtree"]] == trees) ,2] <- with(validation_rf, mean( (Clicks - pred)^2))
}

range(mse.RF$mse)
ntrees <- mse.RF[which.min(mse.RF$mse), 1]
ntrees
ggplot(data=as.data.frame(mse.RF),aes( x=numtree, y=mse)) + xlab("Number of trees in the forest") + ylab("MSE")  + geom_line()

ntrees<- 140

test_rf <- test[,c(2:7)]
testY_rf <- test[,8]

str(test_rf)

rf_clicks<-randomForest(Clicks ~., data= train_rf , ntree=ntrees, mtry = maxMtry)
print(rf_clicks)
pred <- predict(rf_clicks, newdata = test_rf[ ,..x_features], type='response')
testMSE <- with(testY_rf, mean( (Clicks - pred)^2))
testMSE

#-------------------Final fit-----------------
nw<-rbind(data.frame(JobId=1001,	JobTitleId="1",	CategoryId="1",	City="New York",	State="NY",	DescriptionLength=314,	EducationLevel="2"), 
          data.frame(JobId=1002,	JobTitleId="11",	CategoryId="4",	City="New York",	State="NY",	DescriptionLength=199,	EducationLevel="1"))

xtest <- rbind(train[1, 1:7] , nw)
xtest <- xtest[-1,]

nwToModel <- xtest[ ,2:7]

applicantNewPredictRF <- predict(rf_applicants, newdata=nwToModel)
applicantNewPredictRF

clicksNewPredictRF <- predict(rf_clicks, newdata=nwToModel)
clicksNewPredictRF

#---------------Linear Regression --------------
library(MASS)
library(caret)
library(leaps)


full.model <- lm(Applicants ~., data = Jobs[, c(2:7, 9)])
summary(full.model)

fitbackword.aic<- step(full.model, direction = "backward", trace = 1 ,test="F")
summary(fitbackword.aic)

final_model_app <- lm(Applicants ~ DescriptionLength, data = Jobs[, c(2:7, 9)])



full.model <- lm(Clicks ~., data = Jobs[, c(2:7, 8)])
summary(full.model)

fitbackword.aic<- step(full.model, direction = "backward", trace = 1 ,test="F")
summary(fitbackword.aic)

final_model_clicks <- lm(Clicks ~ DescriptionLength, data = Jobs[, c(2:7, 8)])

#-------------------Final fit-----------------
nw<-rbind(data.frame(JobId=1001,	JobTitleId="1",	CategoryId="1",	City="New York",	State="NY",	DescriptionLength=314,	EducationLevel="2"), 
          data.frame(JobId=1002,	JobTitleId="11",	CategoryId="4",	City="New York",	State="NY",	DescriptionLength=199,	EducationLevel="1"))

xtest <- rbind(train[1, 1:7] , nw)
xtest <- xtest[-1,]

nwToModel <- xtest[ ,2:7]

ApplicantsNewPredictLM <- predict(final_model_app, newdata = nwToModel, interval = "prediction")
ApplicantsNewPredictLM

ClicksNewPredictLM <- predict(final_model_clicks, newdata = nwToModel, interval = "prediction")
ClicksNewPredictLM

#--------------------Linear regression other way-------------
library(caret)

data <- Jobs[, c(2:7, 8)]
# Set up a 10-fold cross validation
tc <- trainControl(method = "cv", number = 10)

# Include the setup in your model
lm1_cv <- train(Clicks~., data = data, method = "lm",
                trControl = tc) # here

lm1_cv$finalModel


lm2_cv <- train(Applicants~., data = Jobs[, c(2:7, 9)], method = "lm",
                trControl = tc) # here

lm2_cv$finalModel
#---------Final fit
nw<-rbind(data.frame(JobId=1001,	JobTitleId="1",	CategoryId="1",	City="New York",	State="NY",	DescriptionLength=314,	EducationLevel="2"), 
          data.frame(JobId=1002,	JobTitleId="11",	CategoryId="4",	City="New York",	State="NY",	DescriptionLength=199,	EducationLevel="1"))

xtest <- rbind(train[1, 1:7] , nw)
xtest <- xtest[-1,]

library(dummies)

dummyData <- rbind(train[1, 1:7] , nw)

tempDT <- rbind(Jobs[1, 1:7], nw)
xtestcvlm_dummy <- cbind(tempDT[["JobId"]], dummy(tempDT[["JobTitleId"]]), 
                         dummy(tempDT[["CategoryId"]]), 
                         dummy(tempDT[["City"]]),
                         dummy(tempDT[["State"]]),
                         tempDT[["DescriptionLength"]],
                         dummy(tempDT[["EducationLevel"]]))
colnames(xtestcvlm_dummy)
colnames(xtestcvlm_dummy) <- c("JobId", "JobTitleId2","JobTitleId3",
                               "JobTitleId4","JobTitleId5","JobTitleId6", 
                               "JobTitleId7","JobTitleId8", "JobTitleId9",
                               "JobTitleId10", "JobTitleId11", "CategoryId2", 
                               "CategoryId3", "CategoryId4", "CityBoston",
                               "CityLodi", "CityLos Angeles", "CityNew York",
                               "CityNewtonville", "CitySacramento", "CitySyracuse",
                               "CityWest Newton", "StateMA", "StateNY", "DescriptionLength",         
                               "EducationLevel2", "EducationLevel3")


dummyData <- xtestcvlm_dummy[-1,]

Clicks_cvLM <- predict(lm1_cv$finalModel, newdata = as.data.frame(dummyData), interval = "prediction")
Clicks_cvLM

Applicants_cvLM <-  predict(lm2_cv$finalModel, newdata = as.data.frame(dummyData), interval = "prediction")
Applicants_cvLM


