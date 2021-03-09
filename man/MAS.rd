\encoding{UTF-8}
\name{MAS}
\alias{MAS}

\title{Simple Random Sampling Design without Replacement}
\description{
The MAS function selects a random sample or estimates a parameter of interest under a simple random sampling design without replacement.
}
\usage{
MAS(N,n,yk=NULL,zk=NULL,dk=NULL,type="selec",method="fmuller",
              parameter="total",Nc=0.95,Ek=NULL)
# To select: MAS(N,n,method="fmuller")
# To estimate: MAS(yk,N,n,type="estm",parameter="total")
# To estimate in domains: MAS(yk,dk,N,n,type="estm.Ud",parameter="total")
}

\arguments{
\item{N}{ Size of the population}
\item{n}{Sample size.}
\item{yk}{Vector of observations of the characteristic of interest. This vector is only necessary if you want to estimate.}
\item{zk}{Vector of observations of the characteristic of interest of equal length that yk. This vector is necessary if the parameter of interest is the ratio and refers to the variable involved in the denominator of the ratio.}
\item{dk}{Factor that indicates the individuals that belong to each domain of interest,
Only needed if "type"" is equal to "estm.Ud".}
\item{type}{This argument indicates the procedure that will have the function ("select", "estm" or "estm.Ud"). If type is equal to "select" the function will make a selection, if it is equal to "estm" the function will make the estimation of the indicated parameter and if it is equal to "estm.Ud" it will make an estimate in domain.}
\item{method}{Indicates the method or selection mechanism. If Method is equal to "fmuller" the function uses the Fan-Muller method or if it is equal to "cnegative" the function uses the negative coordinate method.}
\item{parameter}{This argument indicates the parameter to be estimated ("total", "mean", "prop", or "ratio"). }
\item{Nc}{ Confidence level (between 0 and 1), for the confidence interval of the estimator in case "type" is equal to "estm" or "estm.Ud".}
\item{Ek}{Vector of random numbers of length equal to the size of the population. This argument is optional and by default the function generates them from a uniform distribution (0,1). }

}

\value{
This function returns two types of results using the simple random sample design without replacement depending on the "type" argument, which indicates whether to select a sample ("select") or to estimate a parameter ("estm" or "estm.Ud").

If type="select", the function returns a list with a vector (Ksel) with the selected individuals' positions.

If type="estm" or type="estm.Ud", the function returns a data frame with the estimation of the parameter of interest, the estimated variance of the estimator, the standard error, the coefficient of variation (in percent) and an interval of trust.

}
\references{
Särndal, C. E., J. H. Wretman, and C. M. Cassel (1992). Foundations of Inference in Survey Sampling. Wiley New York.

Cochran, W. G. (1977). Sampling Techniques, 3ra ed. New York: Wiley.

Thompson, S. K. (1945). Wiley Series in Probability and Statistics, Sampling, 1ra ed. United States of America.
}
\author{Jorge Alberto Barón Cárdenas <jorgeabaron@correo.unicordoba.edu.co>

Guillermo Martinez Florez <guillermomartinez@correo.unicordoba.edu.co>}

\examples{
zk<-rnorm(200,15,2)
yk<-rnorm(200,10,3)
yk.p<-as.factor(ifelse(yk>10,"A","B"))
Sex<-rep(1:2,length=200)
dk<-factor(Sex,labels=c("Man","Woman"))
selection<-MAS(N=200,n=40,type="selec",method="fmuller")
MAS(N=200,n=40,type="selec",method="cnegativo")

MAS(yk=yk[selection$K],N=200,n=40,type="estm",parameter="total")
MAS(yk=yk[selection$K],N=200,n=40,type="estm",parameter="mean")
MAS(yk=yk.p[selection$K],N=200,n=40,type="estm",parameter="prop")
MAS(yk=yk[selection$K],zk=zk[selection$K],N=200,n=40,type="estm",
    parameter="ratio")

# Domain Estimate

MAS(yk=yk[selection$K],dk=dk[selection$K],N=200,n=40,type="estm.Ud",
    parameter="total")
MAS(yk=yk[selection$K],dk=dk[selection$K],N=200,n=40,type="estm.Ud",
    parameter="mean")
MAS(yk=yk.p[selection$K],dk=dk[selection$K],N=200,n=40,type="estm.Ud",
    parameter="prop")
MAS(yk=yk[selection$K],zk=zk[selection$K],dk=dk[selection$K],N=200,n=40,
    type="estm.Ud",parameter="ratio")
}
