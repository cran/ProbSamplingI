
\encoding{UTF-8}
\name{BER}
\alias{BER}

\title{Bernoulli Sampling Design}
\description{
The BER function selects a random sample or estimates an interest parameter under a Bernoulli design.
}
\usage{
BER(N,Pi,yk=NULL,zk=NULL,dk=NULL,type="selec",parameter="total",
    Nc=0.95,Ek=NULL)
# To selectionar: BER(N,Pi)
# To estimate: BER(yk,Pi,type="estm",parameter="total")
# To estimate in domains: BER(yk,Pi,type="estm.Ud",parameter="total")
}

\arguments{
\item{N}{Size of the population.}
\item{Pi}{Probability of inclusion.}
\item{yk}{Vector of observations of the characteristic of interest. This  vector is only   necessary if you want to estimate}
\item{zk}{Vector of observations of the characteristic of interest of equal length that yk. This vector is necessary if the parameter of interest is the ratio and refers to the variable involved in the denominator of the ratio.}
\item{dk}{Factor that indicates the individuals that belong to each domain of interest,
Only needed if "type" is equal to "estm.Ud".}
\item{type}{This argument indicates the procedure that will have the function ("select", "estm" or "estm.Ud"). If type is equal to "select" the function will make a selection, if it is equal to "estm" the function will perform the estimation of the indicated parameter and if it is equal to "estm.Ud" it will make an estimate in domain.}
\item{parameter}{This argument indicates the parameter to be estimated ("total", "mean", "prop", or "ratio"). }
\item{Nc}{ Confidence level (between 0 and 1), for the confidence interval of the estimator in case the type is equal to "estm" or "estm.Ud".}
\item{Ek}{Vector of random numbers of length equal to the size of the population. This argument is optional and by default the function generates them from a uniform distribution (0,1).}
}

\value{
This function returns two types of results using the Bernoulli sampling design, depending on the "type" argument, which indicates whether you want to select a sample ("select") or estimate a parameter ("estm" or "estm.Ud").

If type="select", the function returns a list with two elements:
\item{Ksel}{Vector with the positions of the selected individuals.}
\item{nk}{Selected sample size.}

If type="estm" or type="estm.Ud", the function returns a data frame with the estimation of the parameter of interest, the estimated variance of the estimator, the standard error, the coefficient of variation (in percentage), a confidence interval and the design effect.
}
\references{
Särndal, C. E., J. H. Wretman, and C. M. Cassel (1992). Foundations of Inference in Survey Sampling. Wiley New York.

Cochran, W. G. (1977). Sampling Techniques, 3ra ed. New York: Wiley.

Thompson, S. K. (1945). Wiley Series in Probability and Statistics, Sampling, 1ra ed. United States of America.
}
\author{ Jorge Alberto Barón Cárdenas <jorgeabaron@correo.unicordoba.edu.co>

 Guillermo Martínez Flórez <guillermomartinez@correo.unicordoba.edu.co>}

\examples{
yk<-rnorm(100,10,2)
zk<-rnorm(100,10,2)
yk.p<-as.factor(ifelse(yk>10,1,0))


selection<-BER(N=100,Pi=0.3,type="selec")
BER(yk=yk[selection$Ksel],Pi=0.3,type="estm",parameter="total")
BER(Pi=0.3,yk=yk[selection$Ksel],type="estm",parameter="mean")
BER(yk=yk.p[selection$Ksel],Pi=0.3,type="estm",parameter="prop")
BER(yk=yk[selection$Ksel],zk=zk[selection$Ksel],Pi=0.3,
 type="estm",parameter="ratio")

# Domain Estimates

#Sex<-sample(2,100,replace=T)
Sex<-rep(1:2,each=50)
dk<-factor(Sex,labels=c("Man","Woman"))

BER(yk=yk[selection$Ksel],dk=dk[selection$Ksel],Pi=0.3,
 type="estm.Ud",parameter="total")
BER(yk=yk[selection$Ksel],dk=dk[selection$Ksel],Pi=0.3,
 type="estm.Ud",parameter="mean")
BER(yk=yk.p[selection$Ksel],dk=dk[selection$Ksel],Pi=0.3,
 type="estm.Ud",parameter="prop")
BER(yk=yk[selection$Ksel],zk=zk[selection$Ksel],
 dk=dk[selection$Ksel],Pi=0.3,type="estm.Ud",parameter="ratio")
}

