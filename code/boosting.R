rm(list=ls())
data_known <- read.csv('training_data.csv',header<-T)
data_known[, 'label'] <- as.factor(data_known[, 'label'])
set.seed(5) # set random seed for reproducibility
training_indices <- sample(nrow(data_known), size = 300, replace = FALSE)
data_known.training <- data_known[training_indices,]
data_known.test <- data_known[-training_indices,]
library(rpart)
library(mlbench)
library(caret)
library(lattice)
library(ggplot2)
library(adabag)
library(MASS)
# cp parameter optimization
predictionerror<-c()
cpvalue<-c()
for(i in 1:12){
	=0.001+(i-1)*0.001
	cpvalue[i]<-bb
	boosting.model = boosting(formula = label ~ danceability + energy + key + loudness + mode + speechiness + acousticness + instrumentalness + liveness + valence + tempo + duration + time_signature, data=data_known.training, control=rpart.control(cp=bb))
	predictions = predict(boosting.model,newdata=data_known.test)
	predictionerror[i] <-predictions$error
}
plot(cpvalue,predictionerror,pch=4,col="red",xlab="cp parameter value", ylab="prediction error")
# maxdepth parameter optimization
predictionerror<-c()
maxdepthvalue<-c()
for(i in 1:10){
	maxdepthvalue[i] <- i
	boosting.model = boosting(formula = label ~ danceability + energy + key + loudness + mode + speechiness + acousticness + instrumentalness + liveness + valence + tempo + duration + time_signature, data=data_known.training, control=rpart.control(cp=0.01, maxdepth=i))
	predictions = predict(boosting.model,newdata=data_known.test)
	predictionerror[i]<-predictions$error
}
plot(maxdepthvalue,predictionerror,pch=4,col="red", xlab="maxdepth parameter value",ylab="prediction error")
predictionerror<-c()
mfinalvalue<-c(10,100,500,1000)
for(i in 1:4){
	bb<-mfinalvalue[i]
	boosting.model = boosting(formula = label ~ danceability + energy + key + loudness + mode + speechiness + acousticness + instrumentalness + liveness + valence + tempo + duration + time_signature, data=data_known.training, mfinal=bb, control=rpart.control(cp=0.01, maxdepth=7))
	predictions = predict(boosting.model,newdata=data_known.test)
	predictionerror[i]<-predictions$error
}
plot(mfinalvalue,predictionerror,pch=4,col="red",xlab="number of nodes",ylab="prediction error")

# Ensemble error vs number of trees
boosting.model = boosting(formula = label ~ danceability + energy + key + loudness + mode + speechiness + acousticness + instrumentalness + liveness + valence + tempo + duration + time_signature, data=data_known.training, mfinal=500, control=rpart.control(cp=0.01, maxdepth=7))
predictions = predict(boosting.model,newdata=data_known.test)
errorevol(boosting.model,newdata=data_known.training)->evol.train
errorevol(boosting.model,newdata=data_known.test)->evol.test
plot.errorevol(evol.test,evol.train)

#4-fold cross-validation
boosting.modelcv = boosting.cv(formula = label ~ danceability + energy + key + loudness + mode + speechiness + acousticness + instrumentalness + liveness + valence + tempo + duration + time_signature, data=data_known, coeflearn = 'Freund', v = 4, boos = TRUE, mfinal = 50, control=rpart.control(cp=0.01, maxdepth=7))
boosting.modelcv[-1]