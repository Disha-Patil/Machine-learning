#data for training
library(kernlab)
random<-spam[sample(2:nrow(spam)),]
random[,ncol(spam)]<-as.numeric(random[,ncol(spam)])
#non spam == 1 and spam == 2

folds<-10
fold_col<-(ncol(random) + 1)
random[,fold_col] <- rep(1:folds,each=(nrow(random)/folds))

folds<-function(fold,data)
{ 
train_set <- data[-(which(data[,fold_col] == fold)),1:ncol(spam)]
test_set <- data[which(data[,fold_col] == fold),1:ncol(spam)]
return(list(train_set=train_set,test_set=test_set))
}


#find the mu and sigma of training data
training<-function(data)
{
return(list(example=data,mean=colMeans(data),sd=sapply(data,sd)))
}

#apply naive-bayes to classify new example
classification<-function(example)
{
 #find the probability of class given example
  example_feature<-c()
  posterior_prob<-function(example,class_prob,class)
  {
    
    for(i in 1:(ncol(spam)-1))
    {
    example_feature[i]<-dnorm(example[,i],class$mean[i],class$sd[i])
        
    }
   return(class_prob * prod(example_feature))

  }
#class probability for spam is 1/2
 class_prob<-1/2

        #calculate the probablity for each class and select max
          p<-c()
            for (j in 1:2)
            {
              p[j]<-posterior_prob(example,class_prob,training(train_data[which(train_data[,ncol(spam)]==j),1:57]))
            }

          predict_class<-which(p==max(p))

          #example[i,6]<-predict_class

	example<-cbind(example,predict_class)

#print example with predicted class
#return(print(example))
return(predict_class)
}



#e is any example 


acc<-c()
for(j in 1:10)
{
folds_data<-folds(j,random[,])
test<-folds_data$test_set
train<-folds_data$train_set
predicted_class<-c()
for(i in 1:nrow(test))
{
predicted_class[i]<-classification(test[i,],train)
}

acc[j]<-(length(which(test[,ncol(spam)]==predicted_class)))/nrow(test)
}
print(mean(acc))



