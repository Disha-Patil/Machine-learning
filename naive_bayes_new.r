#data for training
random<-iris[sample(1:nrow(iris)),]
random[,ncol(iris)]<-as.numeric(random[,ncol(iris)])

folds<-10
fold_col<-(ncol(random) + 1)
random[,fold_col] <- rep(1:folds,each=(nrow(iris)/folds))

folds<-function(fold,data)
{ 
train_set <- data[-(which(data[,fold_col] == fold)),1:ncol(iris)]
test_set <- data[which(data[,fold_col] == fold),1:ncol(iris)]
return(list(train_set=train_set,test_set=test_set))
}


#find the mu and sigma of training data
training<-function(data)
{
return(list(example=data,mean=colMeans(data),sd=sapply(data,sd)))
}

#apply naive-bayes to classify new example
classification<-function(example,train_data)
{
  example_feature<-c()
  #find the probability of class given example
  posterior_prob<-function(example,class_prob,class)
  {
      for(i in 1:(ncol(iris)-1))
    {
    example_feature[i]<-dnorm(example[,i],class$mean[i],class$sd[i])
    }


    return(class_prob * prod(example_feature))
  }
  
#class probability for iris is 1/3
 class_prob<-1/3

	#calculate the probablity for each class and select max
	    p<-c()
            
	    #folds_data<-folds(1,random[,])
	    for (j in 1:3)
	    {
	     p[j]<-posterior_prob(example,class_prob,training(train_data[which(train_data[,ncol(iris)]==j),1:4]))
	    }

	  predict_class<-which(p==max(p))

          #example[i,6]<-predict_class
	  example<-cbind(example,predict_class)
   	
#print example with predicted class
#return(print(example))
return(predict_class)
}



#e is any example 

#e<-matrix(c(5.1,3.1,1.5,0.2),1)
#e<-matrix(c(6,3,4,1.5),1)


#implement for 10 cv folds

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

acc[j]<-(length(which(test[,ncol(iris)]==predicted_class)))/nrow(test)
}
print(mean(acc))
















