#data for training
library(kernlab)
random<-spam[sample(1:nrow(spam)),]
random[,ncol(spam)]<-as.numeric(random[,ncol(spam)])
#non spam == 1 and spam == 2


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
              p[j]<-posterior_prob(example,class_prob,training(random[which(random[,ncol(spam)]==j),-ncol(spam)]))
            }

          predict_class<-which(p==max(p))

          #example[i,6]<-predict_class

	example<-cbind(example,predict_class)

#print example with predicted class
return(print(example))
}



#e is any example 

e<-spam[4000,]
classification(e)


