
x<-read.csv("ex4x.dat",sep="",header=F)
 y<-read.csv("ex4y.dat",sep="",header=F)

x<-cbind(rep(1,nrow(x)),x)
x<-as.matrix(x)
y<-as.matrix(y)
 logit<-function(z){
+ return(1/(1+exp(-z)))}

# hypothesis 
hypo<-function (x,th) {
  return( logit(x %*% th) )
} # h(x,th)


#cost J
J<-function(x,y,th,m){
return((1/m) * sum(-y * log(hypo(x,th)) - (1-y) * log(1- hypo(x,th))) )
} 

grad<-function(x,y,th,m){
return( 1/m * t(x) %*% (hypo(x,th) - y) )
}

hess<-function(x,yx,thx,m){
return(1/m * t(x) %*% x * diag(hypo(x,th)) * diag(1-hypo(x,th)) )
}



Jj<-array(0,c(10,1))
m<-nrow(x)
th<-matrix(0,nrow=3)

for (i in 1:10){
Jj[i]<-J(x,y,th,m)
th<-th - solve(hess(x,y,th,m)) %*% grad(x,y,th,m)
}


 x1 = c(min(x[,2]), max(x[,2]))
 x2 = (-1/th[3,]) * ((th[2,] * x1) + th[1,])
 plot(x1,x2, type='l',  xlab="test1", ylab="test2")
 points(x[,2],x[,3],col=as.factor(y))




