##plot line with the updated weight vector
plot_boundary<-function(w,color='red'){
  w<-as.numeric(w)
  b=w[1]
  w1=w[2]
  w2=w[3]

  slope=-(w1/w2)
  intercept=-(b/w2)
  abline(a=intercept,b=slope,col=color)
}
#perceptron functionality
#lr-learning rate, niter-no.of iterations
perceptron<-function(x,y,lr,niter){
 #initialize weight vector
   w<-rep(0,dim(x)[2]+1)
  #w<-c(-2,3,1)
  err<-rep(0,niter)

  #loop over no.of iterations
  for(j in 1:niter){
     #loop over training  dataset
    for(i in 1:length(y)){
      #predict using activation function
      z<-sum(w[2:length(w)]*as.numeric(x[i,]))+w[1]
            if(z>0){
        ypred<- 1
       }
       else{
        ypred<- 0
       }
      #if predicted value is correct
      wdiff<-lr*(y[i]-ypred)*c(1,as.numeric(x[i,]))
      w<-w+wdiff
      #update error function
      if((y[i]-ypred)!=0.0){
        err[j]<-err[j]+1
      }
    }

  }
  finalw<-w
  print(finalw)
  plot(x,y,pch=ifelse(y==1,2,8),col=ifelse(y==1,'red','blue'))
  plot_boundary(finalw,color='blue')
  return(err)

}


