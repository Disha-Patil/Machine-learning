

#folds
#data for training
library(tree)

breast_data=read.csv("breast-cancer.csv",header=F)


random<-breast_data[sample(1:nrow(breast_data)),]

folds<-10
fold_col<-(ncol(random) + 1)
cuts = folds * round(nrow(random)/folds)
	if (cuts != nrow(random))
		{ 
		random[1:cuts,fold_col] <-  rep(1:folds,each=(cuts/folds))
		random[(cuts+1):nrow(random),fold_col] <- sample(1:10, nrow(random) - cuts)
		}
	if(cuts == nrow(random)) 
		{ 
		random[,fold_col] <- rep(1:folds,each=(nrow(random)/folds)) 
		}


folds<-function(fold,data)
{ 
train_set <- data[-(which(data[,fold_col] == fold)),1:ncol(random)]
test_set <- data[which(data[,fold_col] == fold),1:ncol(random)]
return(list(train_set=train_set,test_set=test_set))
}


acc<-c()
mcc<-c()
for(j in 1:10)
{
folds_data<-folds(j,random[,])
test<-folds_data$test_set
train<-folds_data$train_set
tr <- tree(train[,11]~.,data = train)   #decision tree model
predicted <- predict(tr, test)
prior <- test[,11]
acc[j]<-(length(which(prior==predicted)))/nrow(test)

tble <- table(prior,predicted)

TN <- tble[1,1];TP<-tble[2,2];FN<-tble[2,1];FP<-tble[1,2]

mcc[j] <- ( (TP*TN) - (FP*FN) / sqrt( (TP+FP)*(TP+FN)*(TN+FP)*(TN+FN)  )  )

}


mean_acc<-mean(acc);mean_mcc<-mean(mcc)
df<-data.frame(cv_accuracy=mean_acc,c_mcc=mean_mcc)
print(df)


