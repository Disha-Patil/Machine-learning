
k<-c(1,3,5,7,9,11)
random<-iris[sample(nrow(iris[1:150,])),]

for(iter in 1: length(k))
{
	for(fold in 1:10)
	{
	train_set <- random[sample(1:150,size=0.8*150),]
	test_set <- random[sample(1:150,size=0.2*150),]

	train_labels<-as.numeric(train_set[,5])
	test_labels<-as.numeric(test_set[,5])
		ed<-c()
		nbr<-c()
	test_labels_new<-c()
		for(i in 1:nrow(test_set))
		{
	 		
			for(j in 1:nrow(train_set))
	 		{
	  			ed[j]<-dist(rbind(test_set[i,-5],train_set[j,-5]))
	 		}
		
		 	sorted_ed <- sort(ed)
		
				for(l in 1 : k[iter])
				{
				nbr[l]<-train_labels[which(unique(ed) == sorted_ed[l])]

				}
				cluster1<-length(which(nbr==1)) 
				cluster0<-length(which(nbr==0))
				cluster2<-length(which(nbr==2))
	
				#print(nbr)
					if (max(cluster1,cluster0,cluster2)==cluster1)
					{
					test_labels_new[i]<-1
					}
					else if (max(cluster1,cluster0,cluster2)==cluster0)
				 	{
					test_labels_new[i]<-0
				 	}
					else if(max(cluster1,cluster0,cluster2)==cluster2)
					{
					test_labels_new[i]<-2
					}
	 	 	
		}

		addrow<-rbind(test_labels,test_labels_new)
		test_labels<-addrow

	#print(test_labels_new)
	#print(as.matrix(test_labels))
	op<-test_labels
		print(op)
		efficiency<-c()
		efficiency[1]<-0
		for(i in 2:(length(k) + 1))
		{
		efficiency[i]<-length(which(op[i,]==op[1,]))
		}	
	fold_efficiency[fold]<-efficiency
	}

		
}


req_iter<-min(which(efficiency==max(efficiency)))
optimal_k<-k[req_iter]
print(fold_efficiency)
print(optimal_k)

