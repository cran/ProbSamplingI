\encoding{UTF-8}
\name{PiPT}
\alias{PiPT}

\title{Sampling Design without Replacement with Proportional Inclusion Probabilities for Sizes}
\description{
The PiPT function selects a random sample or estimates an interest parameter under a sampling design with proportional inclusion probabilities proportional to size.
}
\usage{
PiPT(xk,n,yk=NULL,zk=NULL,pik=NULL,mpikl=NULL,dk=NULL,type="selec",
     parameter="total",Nc=0.95,Ek=NULL)

# To select: PiPT(xk,n)

# To estimate: PiPT(yk,pik,mpikl,type="estm",parameter="total")

# To estimate in domains
# PiPT(yk,pik,mpikl,dk,type="estm",parameter="total")
}

\arguments{
\item{xk}{Vector of observations of the auxiliary variable. This vector is only necessary if you wish to select.}
\item{n}{Sample size.}
\item{yk}{Vector of observations of the characteristic of interest. This vector is only necessary if you want to estimate.}
\item{zk}{Vector of observations of the characteristic of interest of equal length that yk. This vector is necessary if the parameter of interest is the ratio and refers to the variable involved in the denominator of the ratio.}
\item{pik}{Vector of the first-order inclusion probabilities.}
\item{mpikl}{Matrix of second-order inclusion probabilities.}
\item{type}{This argument indicates the procedure that will have the function ("select", "estm" or "estm.Ud"). If type is equal to "select" the function will make a selection, if it is equal to "estm" the function will make the estimation of the indicated parameter and if it is equal to "estm.Ud" it will make an estimate in domain.}
\item{dk}{Factor that indicates the individuals that belong to each domain of interest,
Only needed if "type"" is equal to "estm.Ud".}
\item{parameter}{This argument indicates the parameter to be estimated ("total", "mean", "prop", or "ratio"). }
\item{Nc}{ Confidence level (between 0 and 1), for the confidence interval of the estimator in case "type" is equal to "estm" or "estm.Ud".}
\item{Ek}{Vector of random numbers of length equal to the size of the population. This argument is optional and by default the function generates them from a uniform distribution (0,1). }

}

\value{
The PiPT function returns two types of results using a sampling design with inclusion probabilities proportional to size, depending on the argument
"type", which indicates whether to select ("select") or estimate ("estm" or "estm.Ud").

If type="select" the function will return a list with three elements:
\item{Ksel}{Vector with the positions of the selected individuals}
\item{piksel}{First order inclusion probability vector of selected individuals}
\item{mpikl.s}{Matrix of the second-order inclusion probabilities}

If type="estm" or type="estm.Ud", the function returns a data frame with the estimation of the parameter of interest, the estimated variance of the estimator, the standard error, the coefficient of variation (in percentage) and a confidence interval.
}
\references{
Särndal, C. E., J. H. Wretman, and C. M. Cassel (1992). Foundations of Inference in Survey Sampling. Wiley New York.

Cochran, W. G. (1977). Sampling Techniques, 3ra ed. New York: Wiley.

Thompson, S. K. (1945). Wiley Series in Probability and Statistics, Sampling, 1ra ed. United States of America.
}
\author{Jorge Alberto Barón Cárdenas <jorgeabaron@correo.unicordoba.edu.co>

Guillermo Martínez Flórez <guillermomartinez@correo.unicordoba.edu.co>}

\examples{

set.seed(12265)
yk<-rnorm(100,mean=50,sd=5)
zk<-rnorm(100,mean=51,sd=5)
yk.p<-as.factor(ifelse(yk>50,"A","B"))
set.seed(12245)
# Información Auxiliar
xk<-yk*runif(100,min=0.9,max=1.1)
r<-cor(yk,xk)

selection<-PiPT(xk=xk,n=10,type="selec")
PiPT(yk=yk[selection$Ksel],pik=selection$pik,mpikl=selection$mpikl.s,
     type="estm",parameter="total")
PiPT(yk=yk[selection$Ksel],pik=selection$pik,mpikl=selection$mpikl.s,
     type="estm",parameter="mean")
PiPT(yk=yk.p[selection$Ksel],pik=selection$pik,mpikl=selection$mpikl.s,
     type="estm",parameter="prop")
PiPT(yk=yk[selection$Ksel],zk=zk[selection$Ksel],pik=selection$pik,
     mpikl=selection$mpikl.s,type="estm",parameter="ratio")

# Domain Estimate

Sex<-rep(1:2,length=100)
dk<-factor(Sex,labels=c("Man","Woman"))
PiPT(yk=yk[selection$Ksel],pik=selection$pik,mpikl=selection$mpikl.s,
    dk=dk[selection$Ksel],type="estm.Ud",parameter="total")
PiPT(yk=yk[selection$Ksel],pik=selection$pik,mpikl=selection$mpikl.s,
     dk=dk[selection$Ksel],type="estm.Ud",parameter="mean")
PiPT(yk=yk.p[selection$Ksel],pik=selection$pik,mpikl=selection$mpikl.s,
     dk=dk[selection$Ksel],type="estm.Ud",parameter="prop")
PiPT(yk=yk[selection$Ksel],zk=zk[selection$Ksel],pik=selection$pik,
     mpikl=selection$mpikl.s,dk=dk[selection$Ksel],type="estm.Ud",
     parameter="ratio")


}
