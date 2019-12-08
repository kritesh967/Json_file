require(jsonlite)
library(plyr)
require(tidyr) # or require(tidyverse)
library(dplyr)
#loading and cleaning jason file
data1<- stream_in(file("E:\\dsa\\summer minds_jason\\customersdata.json"),flatten=TRUE)

#taking only required columns
dataf=data1[,1]

#extracting data frames out of nesteded list orders
dfs <- lapply(data1$orders, data.frame, stringsAsFactors = FALSE)
data2=rbind.fill(dfs)
data3=data2[,-c(1,4)]

##extracting data frames out of nesteded list payments
df1<- lapply(data1$paymentMethods, data.frame, stringsAsFactors = FALSE)
data4=rbind.fill(df1)
data5=data4[,-c(1,4,5)]

##extracting data frames out of nesteded list transaction
df3<- lapply(data1$transactions, data.frame, stringsAsFactors = FALSE)
data6=rbind.fill(df3)
data7=data6[,5]

#merging all data frames and keeping only distinct files
data8=merge(dataf,data3,all=TRUE)
data9 <- unique( data8)

#merging all data frames and keeping only distinct files
data10=merge(data9,data5,all = TRUE)
data11 <- unique( data10 )

#merging all data frames and keeping only distinct files
data12=merge(data11,data7,all = TRUE)
data13=unique(data12)

#after data preparation , understanding it
boxplot(data13$orderAmount, horizontal= TRUE)
histinf <-hist(x=data13$orderAmount, breaks=4 , plot =TRUE, col= "lightblue", freq= TRUE,  main = paste("Histogram of Subjects", lables= TRUE))
hist(data13$orderAmount)
table(data13$orderState)
table(data13$x)
table(data13$paymentMethodRegistrationFailure)
table(data13$paymentMethodType)
table(data13$y)
head(data13)
datafin=data13 %>% mutate_if(is.character, as.factor)

#starting with model preparation


library(rpart.plot)
library(RColorBrewer)
library(rpart)
library(RGtk2)
library(rattle)
library(ISLR)
rattle()
library(dplyr)

#constructing training and testing set

#Create Training and testing data set

set.seed(2)
train = sample(1:nrow(datafin),nrow(datafin)*0.5)
test = -train
training_data = datafin[train,]
testing_data = datafin[test,]
testing_High = datafin$x



#Create a Decision tree with defaults

tree1 <- rpart( x ~ orderAmount+orderState+paymentMethodRegistrationFailure +paymentMethodType+y,data=training_data,method="class",minsplit = 1,minbucket=1, cp=0.001)
#Plot decision tree with inbulit functions
plot(tree1)
text(tree1, pretty = 1)

#Plot decision tree with Rattle functions
fancyRpartPlot(tree1)

#Predict test data using decision tree computed
tree_pred1 = predict(tree1, testing_data, type="class")
er1 <-mean(tree_pred1 != testing_High) # misclassification error on comparing actual and Predicted values

#Print Accuracy
Accu1<- 1-er1; Accu1


#Print Complexity parameter
#The complexity measure is a combination of the size of a tree and the ability of the 
#tree to separate the classes of the target variable
printcp(tree1)

#Visualize CP graph
plotcp(tree1)



#rpart by default uses ginin score to create leaf nodes. Create a decision tree using information gain
tree3 <- rpart( x ~ orderAmount+orderState+paymentMethodRegistrationFailure+paymentMethodType+y,method="class",minsplit = 1,minbucket=1, cp=0.001,data=training_data, parms = list(split = 'information'))
fancyRpartPlot(tree3)

tree_pred3 = predict(tree3, testing_data, type="class")
er3 <-mean(tree_pred3 != testing_High) # misclassification error
Accu3<- 1-er3; Accu3


#RANDOM FOREST
library(randomForest)
library(caret)

training_data$x <- as.character(training_data$x)
training_data$x <- as.factor(training_data$x)
treerf <- randomForest(x ~ orderAmount+orderState+paymentMethodRegistrationFailure+paymentMethodType+y,data=training_data)

treerf
plot(treerf)
legend("topright", colnames(treerf$err.rate),col=1:4,cex=0.8,fill=1:4)

tree_predrf = predict(treerf, testing_data, type="class")
errf <-mean(tree_predrf != testing_High) # misclassification error
Accurf<- 1-errf; Accurf


#Random forest with crossvalidation

control <- trainControl(method = "cv", number = 5)

#Use mtry to give  number of columns that needs to be randoly selected, default is Sqrt of total number of columns
treerfcv <- randomForest(x ~ orderAmount+orderState+paymentMethodRegistrationFailure+paymentMethodType+y,data=training_data,trControl = control, method = "RF")


print(treerfcv)

#The mean decrease in Gini coefficient is a measure of how each variable contributes to the homogeneity of the nodes 
#and leaves in the resulting random forest. Each time a particular variable is used to split a node, 
#the Gini coefficient for the child nodes are calculated and compared to that of the original node
varImpPlot(treerfcv,sort=TRUE)


tree_predrfcv = predict(treerfcv, testing_data, type="class")
errfcv <-mean(tree_predrfcv != testing_High) # misclassification error
Accurfcv<- 1-errfcv; Accurfcv

##########################################Bayes Model####################################################################


#install.packages("e1071")
library(e1071)

modelby <- naiveBayes(x ~ orderAmount+orderState+paymentMethodRegistrationFailure+paymentMethodType+y, data = training_data)


predby  = predict(modelby,testing_data)
table(predby)

erby <-mean(predby != testing_High) # misclassification error
Accuby<- 1-erby; Accuby


#################################################### Compare the models################################################################


#Compare all the accuracies and determine which is the best fit
choice <- as.data.frame(rbind(Accu1,Accu3,Accurf,Accurfcv,Accuby))
maxm <- max(choice[,1]);maxm
choice

#running on entire data
final_predict = predict(tree3,datafin, type="class")
erf <-mean(final_predict != testing_High) # misclassification error
Accuf<- 1-erf; Accuf
