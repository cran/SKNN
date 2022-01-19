Dist<-function(x,y) {       
	  d<-sqrt(sum((x-y)^2))
	  diff<-abs(x-y)
	  return(list(d=d,diff=diff))
}


