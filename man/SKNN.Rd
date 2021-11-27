\name{SKNN}
\alias{SKNN}
\title{Super K-Nearest Neighbor (SKNN) Classification}
\description{
    It's a Super K-Nearest Neighbor classification method with using kernel density to describe weight of the distance between a training observation and the testing sample.
}  
\usage{
SKNN(data, Class, k, test, Dens)
}
\arguments{
  \item{data}{ Numeric. The training data set, matrix.}
  \item{Class}{ Character.  Class of the training observations, vector.}
  \item{k}{ Integer. The number of K to be used.     }
  \item{test}{ Numeric. The sample to be classified. }
  \item{Dens}{ Integer. Type of the kernel density method, 1 for KDEy, 2 for UKDE, 3 for AKDE. }
}
 
\value{A character.}
\author{Yi Ya and Yarong Yang }
\references{Yi Ya, Yarong Yang, Nader Ebrahimi, Yoram Rubin, and Jacob Zhang. SKNN: A Super K-Nearest Neighbor Classification Algorithm. ongoing

Yarong Yang and Nader Ebrahimi (2010). Two Mutual Information Estimators based on Kernel Density Estimation. part of Yarong Yang's Ph.D. dissertation

Yarong Yang, Matt Over, and Yoram Rubin.(2012) Strategic Placement of Localization Devices (such as Pilot Points and Anchors) 
in Inverse Modeling Schemes. Water Resources Research, 48, W08519, doi:10.1029/2012WR011864. 
}



\examples{

Sepal.Length<-c(4.8, 5.1, 4.6, 5.3, 5.0, 5.7, 5.7, 6.2, 5.1, 5.7, 6.7, 6.3, 6.5, 6.2, 5.9)
Sepal.Width<-c(3.0, 3.8, 3.2, 3.7, 3.3, 3.0, 2.9, 2.9, 2.5, 2.8, 3.0, 2.5, 3.0, 3.4, 3.0)
Petal.Length<-c(1.4, 1.6, 1.4, 1.5, 1.4, 4.2, 4.2, 4.3, 3.0, 4.1, 5.2, 5.0, 5.2, 5.4, 5.1)
Petal.Width<-c(0.3, 0.2, 0.2, 0.2, 0.2, 1.2, 1.3, 1.3, 1.1, 1.3, 2.3, 1.9, 2.0, 2.3, 1.8)
Species<-as.factor(c(rep("setosa",5),rep("versicolor",5),rep("virginica",5)))
iris<-cbind(Sepal.Length,Sepal.Width,Petal.Length,Petal.Width)
Res<-length(nrow(iris))
for(i in 1:nrow(iris)) 
     Res[i]<-SKNN(data=iris,Class=as.vector(Species),k=10,test=iris[i,],Dens=3)
percentage<-length(which(Res==Species))/length(Res)
COL<-rep(0,length(Species))
COL[which(Res==levels(Species)[1])]<-1
COL[which(Res==levels(Species)[2])]<-2
COL[which(Res==levels(Species)[3])]<-3
ind<-1:15
#plot training observations
plot(1:length(ind),xlab="observations",ylab="",rep(1,length(ind)),ylim=c(0,3),type="p",
lwd=2,col=Species[ind])
#add the classification results for comparison
points(1:length(ind),rep(2,length(ind)),pch=15,col=COL[ind])

}

