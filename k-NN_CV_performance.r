
# test for k values where you take test values upto 10% of the total data
k<-c(1,3,5,7,9,11)

#randomize the iris data avilable 
random<-iris[sample(nrow(iris[1:100,])),]
avg.eff<-c()
avg.MCC<-c()

#calculate efficiency and MCC for every k
for (iter in 1:length(k))
{
	test_labels_new<-c()
	efficiency<-c()
	MCC<-c()

		#calculate efficiency using 10 fold cv method
		for(fold in 1:10)
		{
		    #break data into trainig and test set
		    train_set <- random[sample(1:100,size=0.8*100),]
		    test_set <- random[sample(1:100,size=0.2*100),]

	            #to convert the class labels into numeric values
		    train_labels<-as.numeric(train_set[,5]=="setosa")
		    test_labels<-as.numeric(test_set[,5]=="setosa")
		    ed<-c()
		    nbr<-c()
		    
		    #print(test_labels)

			#predict label for each of the test example
			for(i in 1:nrow(test_set))
			{
		 		#calculate distance for each test example with each training example
				for(j in 1:nrow(train_set))
		 		{
		  			ed[j]<-dist(rbind(test_set[i,-5],train_set[j,-5]))
		 		}
		
				 #sort the distance vector
				 sorted_ed <- sort(ed)
		
					#determine labels of top k neighbours
					for(l in 1 : k[iter])
					{
					nbr[l]<-train_labels[which(unique(ed) == sorted_ed[l])]
					} 
					#print(nbr)
						# assign labels to test example (logic will change for multiclass)
						if (sum(nbr)>(k[iter]/2))
						{
						 test_labels_new[i]<-1
						}
						else if (sum(nbr)<(k[iter]/2))
						{
						 test_labels_new[i]<-0
						}

		 	 	
			}

				#print(test_labels_new)
				#print(as.matrix(test_labels))

				#compare_labels<-rbind(test_labels,test_labels_new)
				#print(compare_labels)
				#print(test_labels)
				#print(test_labels_new)
		                
				#calculate efficiency for each of the 10 folds
				efficiency[fold]<-length(which(test_labels_new==test_labels)) / length(test_labels)
				#print(efficiency)
			
				#calculate true and false predictions to calculate MCC
				chk<-table(test_labels,test_labels_new)	
				#print(chk)
				TN<-chk[1,1]; FP<-chk[1,2]; FN<-chk[2,1]; TP<-chk[2,2];
				MCC[fold] <- ((TN*TP) - (FP*FN))/ sqrt( (TP+FP)*(TP+FN)*(TN+FP)*(TN+FN)  )
		}
	#for every value of k the MCC and efficiency is mean value of efficiencies for each fold
	avg.MCC[iter]<-mean(MCC)
	avg.eff[iter]<-mean(efficiency)
	#print(avg.eff)
}
#print(avg.eff)
optimal_k<-k[which(avg.eff==max(avg.eff))]
#print(max(avg.eff))
#print(optimal_k)
#print(avg.MCC)
#print(list(k=k,average.efficiency=avg.eff,average.MCC=avg.MCC))

#print output for comparison of performance of each k
df<-data.frame(k=k,average.efficiency=avg.eff,average.MCC=avg.MCC)
print(df)
