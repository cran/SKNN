SKNN<-function(data, Class, k, test, ker) {    
 
         EK<-function(d) {
             ek<-ifelse(abs(d)>1,0,3*(1-d^2)/4)
             ek
         }

         BK<-function(d) {
             ek<-ifelse(abs(d)>1,0,15/16*(1-d^2)^2)
             ek
         }

         TK<-function(d) {
             ek<-ifelse(abs(d)>1,0,1-abs(d))
             ek
         }
         
         Kers<-function(data,test){
              kernel<-vector()
              for(i in 1:nrow(data)) {
                  diff<-Dist(data[i,],test)$diff
                  f<-vector()
                  for(j in 1:ncol(data)) { 
                        if(ker=="GK") f[j]<-dnorm(diff[j],0,1)
                        if(ker=="EK") f[j]<-EK(diff[j]/max(diff))
                        if(ker=="BK") f[j]<-BK(diff[j]/max(diff))
                        if(ker=="TK") f[j]<-TK(diff[j]/max(diff))
                  }
                  f.v<-f[which(f!=0)]
                  kernel[i]<-prod(f.v)
              }
              Ker<-sum(kernel)
         }

	   K<-length(levels(as.factor(Class)))
	   D<-vector()
	   for(i in 1:nrow(data)) D[i]<-Dist(test,data[i,])$d   
	   ix<-sort(D,decreasing=F,index=T)$ix[1:k]

         if(ncol(data)>1) {
	         Res<-PCAy(data)
	         test<-as.vector(Res@PC%*%matrix(test-colMeans(data),ncol=1))
	         data<-Res@Scores
         }

	   Dat<-matrix(data[ix,],nrow=length(ix))          
	   Cl<-Class[ix]             
	   C<-length(levels(as.factor(Cl)))
	   cl<-vector()
	   dens<-vector()

	   for(i in 1:C) {
	   	    it<-levels(as.factor(Cl))[i]
	   	    cl[i]<-it
	   	    ip<-which(Cl %in% it)
		    dat<-matrix(Dat[ip,],nrow=length(ip))
                 dens[i]<-Kers(dat,test)
	   }
	   
	   cl[which(dens==max(dens))[1]]	   
}