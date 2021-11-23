\name{PCAyd}
\Rdversion{1.1}
\docType{class}
\alias{PCAyd-class}

\title{ Class to contain the results from revised PCA analysis. }

\description{
The function PCAy return object of class PCAyd that contains the number of clusters, 
the center of each cluster, and the observations in each cluster.
}

\section{Objects from the Class}{
new("PCAyd",Var=new("numeric"),PC=new("matrix"),Scores=new("matrix"),IScores=new("numeric"))
}
\section{Slots}{
  \describe{
    \item{\code{Var}:}{An numeric vector giving the variance of each PC.}
    \item{\code{PC}:}{A numeric matrix about the coefficients of each PC.}
    \item{\code{Scores}:}{A numeric matrix showing the loading coefficiens of each PC.}
    \item{\code{IScores}:}{ A numeric vector with each element being the rowsum of Scores.  }
    }
}

\references{Yi Ya, Yarong Yang, Nader Ebrahimi, and Jacob Zhang. SKNN: A Super K-Nearest Neighbor Classification Algorithm. ongoing
}
\author{
Yi Ya and Yarong Yang
}
\examples{
showClass("PCAyd")
}
\keyword{classes}
