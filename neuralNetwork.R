#2 classes / Logistic Classification with Neural Network - CNN using neuralnet pa
#
rm(list=ls(all.names=TRUE))
memory.limit(size=56000)
set.seed(123)

#-------------------------load this packeges,if miss one, please install first-------------
library(MASS)

require(RCurl)
require(prettyR)
require(neuralnet)
#------------------------------functions----------------------
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#----------------------set directories and load data----------------
setwd("your directory") #manualy update

mydata<-read.csv("Xy_train.csv",header = TRUE)
x_test<-read.csv("X_test.csv",header = TRUE)
y_test <- read.csv("preds_sample.csv", header = TRUE)

#-------------balance and scale the data
#due to unbalanced data this step performed which double the number of observations
#target variable name is: y 
Y1 <- subset(mydata, y==1)
Y0 <- subset(mydata, y==0)
tmp_data<-mydata
tmp_data <- rbind(tmp_data, Y1)
tmp_data <- rbind(tmp_data, Y0)
Y1 <- subset(tmp_data, y==1)
Y0 <- subset(tmp_data, y==0)
balanced_data<-tmp_data

#delete id 
balanced_data <- subset(balanced_data, select = -c(id))

#scale the data
maxmindf <- as.data.frame(lapply(balanced_data, normalize))


#-------------split train into train and validation set
mydata<-maxmindf 
num_of_Xs<-(length(mydata[1,])-1)
num_of_Xs_and_Y<-length(mydata[1,])

describe(mydata)

mydataLength<-nrow(mydata)
SampleTrain<-sample(mydataLength, mydataLength*0.7, replace = FALSE, prob = NULL)
train<-mydata[SampleTrain,]

validation <- mydata[-SampleTrain,]

#building the model formule assuming the Y variable is in the last column 
formula_string <- "y ~ "
i <- 1
for (var in colnames(train[,1:num_of_Xs])){
  formula_string <- paste(formula_string,  var)
  
  if (i<num_of_Xs){
    formula_string <- paste(formula_string, " + ")
  }
  i <- i+1
}


#------------------------------------------tune the model-------------------------------------
#-----two options of tune - tunning each hiper-parameter seperate or tune at all span (downscroll)

#--------------------first option - tune each hiper-parameter seperate--------------
#-----tune first the number of neurons on each layer
num_neurons <- seq(1, 100, by=1)
acc_matrix <- matrix(0, length(num_neurons), 2)
i <- 1
num_of_hidden_layers <- 1

for (neurons in num_neurons){
  nn2 <- neuralnet(as.formula(formula_string), hidden=c(neurons,num_of_hidden_layers), linear.output=FALSE, threshold=0.01, data = train)
  nn.results <- compute(nn2, validation[, 1:num_of_Xs])
  results <- data.frame(actual = validation[["y"]], prediction = nn.results$net.result)
  
  roundedresults<-sapply(results,round,digits=0)
  roundedresultsdf=data.frame(roundedresults)
  t <- table(roundedresultsdf$actual,roundedresultsdf$prediction)
  acc_matrix[i,2] <- i
  acc_matrix[i,1] <- (t[1]+t[4])/sum(t)
  print(acc_matrix[i,(num_of_hidden_layers)])
  i <- i + 1
}

plot(acc_matrix[ ,2], acc_matrix[ ,1])

ggplot(data.frame(x=acc_matrix[ ,2], y=acc_matrix[ ,1])) + geom_line(aes(x,y), color='blue') + 
  xlab("Number of Neurons") + ylab("Validation Accuracy") + 
  scale_y_continuous(limits=c(0.53, 0.9) , breaks = seq(0, 1, 0.05))

#-----------tune the number of hidden layers
best_acc     <- acc_matrix[,1][which.max(acc_matrix[,1])] 
neurons_best <- acc_matrix[,2][which.max(acc_matrix[,1])] 
layers <- c(1,2,3)
i <- 1

acc_matrix2 <- matrix(0, length(layers))

for (num_of_hidden_layers in layers){
  nn2 <- neuralnet(as.formula(formula_string), hidden=c(neurons_best,num_of_hidden_layers), linear.output=FALSE, threshold=0.01, data = train)
  nn.results <- compute(nn2, validation[, 1:num_of_Xs])
  results <- data.frame(actual = validation[["y"]], prediction = nn.results$net.result)
  
  roundedresults <- sapply(results,round,digits=0)
  roundedresultsdf <- data.frame(roundedresults)
  t <- table(roundedresultsdf$actual,roundedresultsdf$prediction)
  acc_matrix2[i] <- (t[1]+t[4])/sum(t)
  
  num_of_hidden_layers
  print(acc_matrix2[i])
  i <- i + 1
}

acc_matrix2
best_hidden <- 1 #manualy update

#--------------------------second option - tune both parameters together ------------------
#---------------------tune at all span---------------------
num_neurons <- seq(1, 50, by=1)
layers <- c(1,2,3)

acc_matrix3 <- matrix(0, length(num_neurons), length(layers))

for (num_of_hidden_layers in layers){
  for (neurons in num_neurons){
    nn2 <- neuralnet(as.formula(formula_string), hidden=c(neurons,num_of_hidden_layers), linear.output=FALSE, threshold=0.01, data = train)
    nn.results <- compute(nn2, validation[, 1:num_of_Xs])
    results <- data.frame(actual = validation[["y"]], prediction = nn.results$net.result)
    
    roundedresults <- sapply(results,round,digits=0)
    roundedresultsdf <- data.frame(roundedresults)
    t <- table(roundedresultsdf$actual,roundedresultsdf$prediction)
    acc_matrix3[neurons,num_of_hidden_layers] <- (t[1]+t[4])/sum(t)
    print(acc_matrix3[neurons,num_of_hidden_layers])
    
  }
}

acc_matrix3
best_acc <- acc_matrix3[which.max(acc_matrix3)] 
bests <- which(acc_matrix3 == max(acc_matrix3), arr.ind = TRUE)
colnames(bests) <- c("neurons" , "hidden layers")

neurons_best <- 39 # update manualy
best_hidden <- 1 # update manualy

#------------------------------scale test data--------------------------
tmp <- subset(x_test, select = -c(id))

r_x_test <- as.data.frame(lapply(tmp, normalize))

#-------------train model with chosen hiper-parameters and fit on test data-------------
nn_fit <- neuralnet(as.formula(formula_string), hidden=c(neurons_best,best_hidden), linear.output=FALSE, threshold=0.01, data = train)

#network visualization
plot(nn_fit)

nn.results <- compute(nn_fit, r_x_test)
results <- data.frame(actual = y_test, prediction = nn.results$net.result)

roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)

t <- table(roundedresultsdf$y,roundedresultsdf$prediction)
print("Confusion Matrix: ")
t
final_acc <- (t[1]+t[4])/sum(t)
print("Model accuracy on test data: ")
final_acc
