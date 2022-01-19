SKNN<-function(data, Class, k, test) {
         
         Ker.normal<-function(data,test){
              ker<-vector()
              for(i in 1:nrow(data)) {
                  diff<-Dist(data[i,],test)$diff
                  f.normal<-vector()
                  for(j in 1:ncol(data)) 
                        f.normal[j]<-dnorm(diff[j],0,1)
                  ker[i]<-prod(f.normal)
              }
              Ker<-sum(ker)
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
                dens[i]<-Ker.normal(dat,test)
	   }
	   
	   cl[which(dens==max(dens))[1]]	   
}