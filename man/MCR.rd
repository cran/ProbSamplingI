\encoding{UTF-8}
\name{MCR}
\alias{MCR}

\title{Simple Random Sampling Design with Replacement}
\description{
The MCR function selects a random sample or estimates an interest parameter under a simple random sampling design without replacement.
}
\usage{
MCR(N,m,yk=NULL,zk=NULL,dk=NULL,type="selec",parameter="total",
    Ek=NULL,Nc=0.95)
# To select: MCR(N,m)
# To estimate: MCR(yk,N,m,type="estm",parameter)
# To domain estimate: MCR(yk,dk,N,m,type="est.Ud",parameter)
}

\arguments{
\item{N}{ Size of the population.}
\item{m}{Sample size.}
\item{yk}{Vector of observations of the characteristic of interest. This vector is only necessary if you want to estimate.}
\item{zk}{Vector of observations of the characteristic of interest of equal length that yk. This vector is necessary if the parameter of interest is the ratio and refers to the variable involved in the denominator of the ratio.}
\item{dk}{Factor that indicates the individuals that belong to each domain of interest,
Only needed if type is equal to "estm.Ud".}
\item{type}{This argument indicates the procedure that will have the function ("select", "estm" or "estm.Ud"). If type is equal to "select" the function will make a selection, if it is equal to "estm" the function will make the estimation of the indicated parameter and if it is equal to "estm.Ud" it will make an estimate in domain.}

\item{parameter}{This argument indicates the parameter to be estimated ("total", "mean", "prop", or "ratio"). }
\item{Nc}{ Confidence level (between 0 and 1), for the confidence interval of the estimator in case the type is equal to "estm" or "estm.Ud".}
\item{Ek}{Vector of random numbers of length equal to the size of the population. This argument is optional and by default the function generates them from a uniform distribution (0,1). }

}

\value{
This function returns two types of results using the simple random sample design with replacement, depending on the "type" argument with which it is indicated to select a sample ("select") or to estimate a parameter ("estm" or "estm.Ud").

If type="select", the function returns a list with two elements:
\item{Ksel}{Vector with the positions of the selected individuals.}
\item{pksel}{Vector with the probabilities of selection of individuals.}

If type="estm" or type="estm.Ud", the function returns a data frame with the estimation of the parameter of interest, the estimated variance of the estimator, the standard error, the coefficient of variation (in percentage), a confidence interval and the design effect.
}
\references{
Särndal, C. E., J. H. Wretman, and C. M. Cassel (1992). Foundations of Inference in Survey Sampling. Wiley New York.

Cochran, W. G. (1977). Sampling Techniques, 3ra ed. New York: Wiley.

Thompson, S. K. (1945). Wiley Series in Probability and Statistics, Sampling, 1ra ed. United States of America.
}
\author{Jorge Alberto Barón Cárdenas <jorgeabaron@correo.unicordoba.edu.co>

Guillermo Martínez Flórez <guillermomartinez@correo.unicordoba.edu.co>}

\examples{
yk<-rnorm(200,10,2)
zk<-rnorm(200,15,3)
yk.p<-as.factor(ifelse(yk>10,1,0))
selection<-MCR(N=200,m=40)
MCR(yk=yk[selection$Ksel],N=200,m=40,type="estm",parameter="total")
MCR(yk=yk[selection$Ksel],N=200,m=40,type="estm",parameter="mean")
MCR(yk=yk.p[selection$Ksel],N=200,m=40,type="estm",parameter="prop")
MCR(yk=yk[selection$Ksel],zk=zk[selection$Ksel],N=200,m=40,
     type="estm",parameter="ratio")

# Domain Estimate

Sex<-rep(1:2,length=200)
dk<-factor(Sex,labels=c("Man","Woman"))
MCR(yk=yk[selection$K],dk=dk[selection$K],N=200,m=40,type="estm.Ud")
MCR(yk=yk[selection$K],dk=dk[selection$K],N=200,m=40,type="estm.Ud",
    parameter="mean")
MCR(yk=yk.p[selection$Ksel],dk=dk[selection$K],N=200,m=40,
    type="estm.Ud",parameter="prop")
MCR(yk=yk[selection$Ksel],zk=zk[selection$Ksel],dk=dk[selection$K],
    N=100,m=40,type="estm.Ud",parameter="ratio")

}
