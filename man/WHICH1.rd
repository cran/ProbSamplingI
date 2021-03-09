\encoding{UTF-8}
\name{WHICH1}
\alias{WHICH1}

\title{Positions of the components of a vector with respect to another vector}
\description{
The WHICH1 function returns the positions in which the vector components (V1) are located in another vector (V2).
}
\usage{
WHICH1(V1,V2)

}

\arguments{
\item{V1}{Vector initial. }
\item{V2}{Vector containing replicates of the components of the initial vector.}

}

\value{
This function is used to extract the positions of all the individuals that are part of the selected clusters, in a cluster sampling.

}
\references{
Särndal, C. E., J. H. Wretman, and C. M. Cassel (1992). Foundations of Inference in Survey Sampling. Wiley New York.

Cochran, W. G. (1977). Sampling Techniques, 3ra ed. New York: Wiley.

Thompson, S. K. (1945). Wiley Series in Probability and Statistics, Sampling, 1ra ed. United States of America.
}
\author{ Jorge Alberto Barón Cárdenas <jorgeabaron@correo.unicordoba.edu.co>

Guillermo Martínez Flórez <guillermomartinez@correo.unicordoba.edu.co>}

\examples{
cong<-rep(1:12,each=10)
Argt<-list(NI=12,nI=3)
selection<-CONGL(Argt=Argt,design="MAS")
WHICH1(selection$Ksel,cong)
}
