library(randomForest)
library(ggplot2)

#load data
data <- read.csv('training_data.csv',header<-T)
test_data <- read.csv('songs_to_classify.csv',header<-T)
data$label = as.factor(data$label)

#cross-validation for variable selection
trainx=data[,c(1:13)]
trainy=data[,c(14)]

N=10;
testerror=matrix(0,N,13)
for (n in 1:N) {
  result=rfcv(trainx, trainy,step=0.95)
  testerror[n,]=result$error.cv
}

testmean=colMeans(testerror);
testvar=colVars(testerror);
xs=seq(13,1,-1)

p=qplot(x=xs,y=testmean,geom="line")+
  geom_ribbon(aes(ymin=testmean-sqrt(testvar),ymax=testmean+sqrt(testvar)),
  fill="blue",alpha=0.3)+
  ylab('Mean of testerror +/- std.dev')+
  xlab('number of predictors considered')
print(p)

#validation of the number of trees 
N=40;
testerror=matrix(0,N,31)
Bs=seq(0, 300, by = 10)
Bs[1]=1
for (n in 1:N)
{
  for (b in 1:length(Bs))
  {
    train <- sample(x=1:nrow(data), size=nrow(data)*0.75, replace=FALSE)
    songs.train <- data[train,]
    songs.test <- data[-train,]
    
    rf.fit <- randomForest(label ~ ., data = songs.train, ntree=Bs[b])
    rf.probs <- predict(rf.fit, newdata=songs.test)
    
    # test error rate
    testerror[n,b]=mean(songs.test$label != rf.probs);
  }
}
source('colVars.R')

testmean=colMeans(testerror);
testvar=colVars(testerror);

p=qplot(x=Bs,y=testmean,geom="line")+
  geom_ribbon(aes(ymin=testmean-sqrt(testvar),ymax=testmean+sqrt(testvar)),
  fill="blue",alpha=0.3)+
  ylab('Mean of testerror +/- std.dev')+
  xlab('size of the tree')
print(p)

# predition of test data - taking all variables and B=500
rf.fit <- randomForest(label ~ ., data = data)
rf.probs <- predict(rf.fit, newdata=test_data)