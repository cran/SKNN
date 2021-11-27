\name{PCAy}
\alias{PCAy}
\title{ Revised PCA analysis }
\description{
    It's a revised PCA analysis.
}
\usage{
PCAy(data)
}
\arguments{
  \item{data}{ Numeric. Data matrix for revised PCA analysis.}
}

\value{An object of class "PCAyd".}
\author{Yi Ya, Yarong Yang, and Yoram Rubin }
\references{Yarong Yang, Matt Over, and Yoram Rubin.(2012) Strategic Placement of Localization Devices (such as Pilot Points and Anchors) 
in Inverse Modeling Schemes. Water Resources Research, 48, W08519, doi:10.1029/2012WR011864. 

Yi Ya, Yarong Yang, Nader Ebrahimi, Yoram Rubin, and Jacob Zhang. SKNN: A Super K-Nearest Neighbor Classification Algorithm. ongoing
}



\examples{
Sepal.Length<-c(4.8, 5.1, 4.6, 5.3, 5.0, 5.7, 5.7, 6.2, 5.1, 5.7, 6.7, 6.3,
 6.5, 6.2, 5.9)
Sepal.Width<-c(3.0, 3.8, 3.2, 3.7, 3.3, 3.0, 2.9, 2.9, 2.5, 2.8, 3.0, 2.5,
 3.0, 3.4, 3.0)
Petal.Length<-c(1.4, 1.6, 1.4, 1.5, 1.4, 4.2, 4.2, 4.3, 3.0, 4.1, 5.2, 5.0,
 5.2, 5.4, 5.1)
Petal.Width<-c(0.3, 0.2, 0.2, 0.2, 0.2, 1.2, 1.3, 1.3, 1.1, 1.3, 2.3, 1.9,
 2.0, 2.3, 1.8)
dat<-cbind(Sepal.Length,Sepal.Width,Petal.Length,Petal.Width)
Res<-PCAy(dat)

}


