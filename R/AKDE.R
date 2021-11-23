AKDE<-function(tdat,edat) {

      K.normal<-function(data,d){
           if(d==1) K<- exp(-data^2/2)/(2*pi)^(1/2)
           else K<- exp(-data%*%data/2)/(2*pi)^(d/2)
           K
      } 

      if(ncol(tdat)!=length(edat)) cat("Estimating data is not compatible with training data...","\n")
      else {
            d<-ncol(tdat)
            n<-nrow(tdat)
            h<-(4/(d+2))^(1/(d+4))*n^(-1/(d+4)) 
            if(n>1) {
            	for(i in 1:d) h[i]<-1.06*min(sd(tdat[,i]),IQR(tdat[,i])/1.349)*nrow(tdat)^(-1/(4+d))
            }
            dens<-vector()
            for(i in 1:n) {
                   Ker<-vector()
                   for(j in 1:n)  
                          Ker[j]<-K.normal(Dist(tdat[i,],tdat[j,])$diff/h,d)/prod(h)
                   dens[i]<-mean(Ker)
            }
            
            g<-prod(dens^(1/n))
            lambda<-vector()
            for(i in 1:n) lambda[i]<-ifelse(g==0,1,(dens[i]/g)^(-1/d))
            
            ad.dens<-vector()
            Ker<-vector()
            for(j in 1:n)
                   Ker[j]<-K.normal(Dist(edat,tdat[j,])$diff/(h*lambda[j]),d)/prod(h*lambda[j])
            ad.dens<-mean(Ker)
      }

      ad.dens
} 
