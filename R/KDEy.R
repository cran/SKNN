KDEy<-function(tdat,edat) {
    
      K.normal<-function(S,data,d){
           if(d==1) K<- exp(-data^2/2)/(2*pi)^(1/2)
           else K<- exp(-data%*%solve(S)%*%data/2)/(2*pi)^(d/2)/det(S)^(1/2)
           K
      } 

      if(ncol(tdat)!=length(edat)) cat("Estimating data is not compatible with training data...","\n")
      else {
            d<-ncol(tdat)
            n<-nrow(tdat)
            h<-(4/(d+2))^(1/(d+4))*n^(-1/(d+4))     
            Ker<-vector()
            if(d==1) {
                 for(j in 1:nrow(tdat))  
                      Ker[j]<-K.normal(0,Dist(edat,tdat[j,])$diff/h,d)/h^d
                 dens<-mean(Ker)
            }
            else {
            	 S<-diag(1,ncol(tdat)) 
                 for(j in 1:nrow(tdat))  
                      Ker[j]<-K.normal(S,Dist(edat,tdat[j,])$diff/h,d)/h^d
                 dens<-mean(Ker)
            }
      }
      dens
}

