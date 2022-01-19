\name{KNN}
\alias{KNN}
\title{ K-Nearest Neighbor Classification }
\description{
    It's implementation of the K-Nearest Neighbor classification method for data of any number of dimentions.
}  
\usage{
KNN(data, Class, k, test)
}
\arguments{
  \item{data}{ Numeric. The training data set, matrix.}
  \item{Class}{ Character.  Class of the training observations, vector.}
  \item{k}{ Integer. The number of K to be used.     }
  \item{test}{ Numeric. The sample to be classified. }
}
 
\value{A character.}
\author{Yi Ya}
\references{
A.T. Covert and P. Hart. Nearest Neighbor Pattern Classification. IEEE Transactions on
Information Theory, 13(1): 21-27, 1967.
}

\examples{

Sepal.Length<-c(4.8, 5.1, 4.6, 5.3, 5.0, 5.7, 5.7, 6.2, 5.1, 5.7, 6.7, 6.3, 6.5, 6.2, 5.9)
Sepal.Width<-c(3.0, 3.8, 3.2, 3.7, 3.3, 3.0, 2.9, 2.9, 2.5, 2.8, 3.0, 2.5, 3.0, 3.4, 3.0)
Petal.Length<-c(1.4, 1.6, 1.4, 1.5, 1.4, 4.2, 4.2, 4.3, 3.0, 4.1, 5.2, 5.0, 5.2, 5.4, 5.1)
Petal.Width<-c(0.3, 0.2, 0.2, 0.2, 0.2, 1.2, 1.3, 1.3, 1.1, 1.3, 2.3, 1.9, 2.0, 2.3, 1.8)
Species<-as.factor(c(rep("red",5),rep("blue",5),rep("green",5)))
iris<-cbind(Sepal.Length,Sepal.Width,Petal.Length,Petal.Width)
Res<-length(nrow(iris))
k<-10
for(i in 1:nrow(iris)) 
     Res[i]<-KNN(data=iris,Class=as.vector(Species),k=k,test=iris[i,])
accuracy<-length(which(Res==Species))/length(Species)
plot(x=1:15,y=rep(1,15),col=as.vector(Species),lwd=4,ylim=c(0,3),xlab="",ylab="",
yaxt = "n",xaxt="n")
par(new=TRUE)
plot(x=1:15,y=rep(2,15),col=Res,lwd=4,ylim=c(0,3),xlab="",ylab="",yaxt = "n",xaxt="n")
ind<-which(Res!=Species)
if(length(ind)>0) {
  for(j in 1:length(ind))
      lines(x=c(ind[j],ind[j]),y=c(1+0.05,2-0.05))
}
text(5,0.3,paste("KNN Misclassified:",length(ind)))
axis(2,at=2,labels="KNN",las=1)
text(10,2.5,paste("k: ",k))

}

