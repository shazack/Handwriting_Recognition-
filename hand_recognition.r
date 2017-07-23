

#Reading the input and getting the train and test set into R data frames
digit_train = read.csv('train.csv', header=TRUE)

#splitting the train set into train set and validation set
split_data = sample.split(digit_train$label, SplitRatio=.7)
train = subset(digit_train, split_data==TRUE)
test = subset(digit_train, split_data==FALSE)

#using Random Forest to train and perform validation on a sample of the train set
library(randomForest)
train$label = factor(train$label)
test$label = factor(test$label)
D = randomForest(label ~ ., data=train, ntree=100, do.trace=TRUE,mtry=60,nodesize=10)

#Plotting
PLOT_DIGIT<-varImpPlot(D,sort=TRUE)

#Prediction on the validation set
rFPredict = predict(D, newdata=test)
rFTable = table(test$label, rFPredict)
#Accuracy calculation
accuracy = sum(diag(rFTable))/nrow(test)


#Performing Test on the test data
Testingdata = read.csv('test.csv')
rFPredict1 = predict(D, newdata=Testingdata)
out = levels(rFPredict1)[rFPredict1]
write(out, 'predict.csv')