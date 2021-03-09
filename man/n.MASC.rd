\encoding{UTF-8}
\name{n.MASC}
\alias{n.MASC}

\title{Sample size using simple random sampling design without conglomerate replacement.}
\description{
The n.MASC function determines sample size using a simple random sampling design without replacement of Conglomerates.
}
\usage{
n.MASC(N,NI,Ni,St,Emax.a,Nc=0.95,n.equal=TRUE)

# For clusters with equal sizes.
# n.MASC(NI,Ni,St,Emax.a,Nc)

# For clusters with different sizes.
# n.MASC(N,NI,St,Emax.a,Nc,n.equal=FALSE)
}

\arguments{
\item{N}{Size of the population, this argument is only necessary if the size of the conglomerates is different. }
\item{NI}{Number of clusters in the population.}
\item{Ni}{Size of the clusters, this argument is only necessary if the conglomerates have equal size (constant size).}
\item{St}{Standard deviation of conglomerate totals.}
\item{Emax.a}{Absolute maximum error.}
\item{Nc}{Confidence level (between 0 and 1) to be set.}
\item{n.equal}{Logical value indicating whether clusters have the same size}

}

\value{
This function returns the sample size under the conditions set in the arguments, that is, the number of clusters to select.

}
\references{
Särndal, C. E., J. H. Wretman, and C. M. Cassel (1992). Foundations of Inference in Survey Sampling. Wiley New York.

Cochran, W. G. (1977). Sampling Techniques, 3ra ed. New York: Wiley.

Thompson, S. K. (1945). Wiley Series in Probability and Statistics, Sampling, 1ra ed. United States of America.
}
\author{ Jorge Alberto Barón Cárdenas <jorgeabaron@correo.unicordoba.edu.co>

Guillermo Martínez Flórez <guillermomartinez@correo.unicordoba.edu.co>}

\examples{


# Sample size for populations with clusters of equal size.

st<-sqrt(1417.8668)
NI<-2000
Ni<-6
e<-2
nc=0.9
n.MASC(St=st,NI=NI,Ni=Ni,Emax.a=e,Nc=nc)

# Sample size for populations with clusters of different sizes.

st=sqrt(2019760.760)
N<-11000
NI<-400
e=10
nc=0.95
n.MASC(St=st,N=N,NI=NI,Emax.a=e,Nc=nc,n.equal=FALSE)
}
