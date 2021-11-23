SKNN<-function(data, Class, k, test, Dens) {
         
         #library(mvtnorm)
         #library(mnormt)
         #library(MASS)
         #library(sn)
         #library(compositions)

	   K<-length(levels(as.factor(Class)))
	   D<-vector()
	   for(i in 1:nrow(data)) D[i]<-Dist(test,data[i,])$d   
	   ix<-sort(D,decreasing=F,index=T)$ix[1:k]

	   Res<-PCAy(data)
	   test<-as.vector(Res@PC%*%matrix(test-colMeans(data),ncol=1))
	   data<-Res@Scores

	   Dat<-matrix(data[ix,],nrow=k)          
	   Cl<-Class[ix]             
	   K.new<-length(levels(as.factor(Cl)))
	   cl<-vector()
	   dens<-vector()
	   for(i in 1:K.new) {
	   	    it<-levels(as.factor(Cl))[i]
	   	    cl[i]<-it
	   	    ip<-which(Cl %in% it)
		    dat<-matrix(Dat[ip,],nrow=length(ip))
		    if(Dens==1) dens[i]<-KDEy(dat,test) 
		    if(Dens==2) dens[i]<-UKDE(dat,test)		    
		    if(Dens==3) dens[i]<-AKDE(dat,test)
	   }
	   
	   cl[which(dens==max(dens))]	   
}