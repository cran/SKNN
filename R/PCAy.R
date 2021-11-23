PCAy<-function(data) {
	
      p<-1
	Cov<-cov(data)
	Res<-princomp(data)
	i<-1
	Comp<-Res$loadings[,i]	
	while(i<ncol(data))	{
		i<-i+1
	    Comp<-cbind(Comp,Res$loadings[,i])
	}
	colnames(Comp)<-paste("Comp.",1:ncol(data),sep="")
	Proj<-Res$scores
	Var1<-eigen(Cov)$values
	Var2<-Res$sdev^2
	
		P<-c(cumsum(Var1/sum(Var1))[-1],1)
		g<-which(!(P<p))[1]
		IScores<-vector()
		for(j in 1:nrow(data)) {
			W<-vector()
			for(k in 1:g) 
			     W[k]<-Var1[k]*abs(Proj[j,k])/sum(abs(Proj[,k]))
			IScores[j]<-sum(W)
		}		
		return(new("PCAyd",Var=Var1,PC=Comp,Scores=Proj,IScores=IScores))			
}








