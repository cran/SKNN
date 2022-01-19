KNN<-function(data,Class,k,test) {

    D<-vector()
    for(i in 1:nrow(data)) D[i]<-Dist(test,data[i,])$d   
    ix<-sort(D,decreasing=F,index=T)$ix[1:k]
      
    Cl<-Class[ix]             
    C<-length(levels(as.factor(Cl)))
    q<-vector()
 
    for(i in 1:C) {
        it<-levels(as.factor(Cl))[i]
        q[i]<-length(which(Cl==it))
    }

    levels(as.factor(Cl))[which(q==max(q))[1]]
}
