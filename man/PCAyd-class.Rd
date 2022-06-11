\name{PCAyd}
\Rdversion{1.1}
\docType{class}
\alias{PCAyd-class}

\title{ Class to contain the results from revised PCA analysis. }

\description{
The function PCAy returns object of class PCAyd.
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

\references{Yarong Yang, Matt Over, and Yoram Rubin.(2012) Strategic Placement of Localization Devices (such as Pilot Points and Anchors) 
in Inverse Modeling Schemes. Water Resources Research, 48, W08519, doi:10.1029/2012WR011864. 

Yarong Yang(Yi Ya).(2022) SKNN: A Super K-Nearest Neighbor Classification Algorithm. submitted to Journal of Statistical Software
}

\author{
Yi Ya, Yarong Yang, and Yoram Rubin
}
\examples{
showClass("PCAyd")
}
\keyword{classes}
