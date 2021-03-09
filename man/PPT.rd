\encoding{UTF-8}
\name{PPT}
\alias{PPT}

\title{Sampling Design with Replacement and Size Proportional Selection Probabilities}
\description{
The PPT function selects a random sample or estimates a parameter of interest under a sampling design with proportional proportional selection probabilities (PPT).
}
\usage{
PPT(xk,m,yk=NULL,zk=NULL,pk=NULL,dk=NULL,type="selec",parameter="total",
    method ="acum.total",Nc=0.95,Ek=NULL)

# To select: PPT(xk,m,method="acum.total")
# To estimate: PPT(yk,pk,type="estm",parameter)
# To estimate in domains: PPT(yk,pk,dk,type="estm.Ud",parameter)
}

\arguments{
\item{xk}{ Vector of observations of the auxiliary variable. This vector is only necessary if you wish to select.}
\item{m}{Sample size.}
\item{yk}{Vector of observations of the characteristic of interest. This vector is only necessary if you want to estimate.}
\item{zk}{Vector of observations of the characteristic of interest of equal length that yk. This vector is necessary if the parameter of interest is the ratio and refers to the variable involved in the denominator of the ratio.}
\item{pk}{Vector of the probabilities of selection of individuals.}
\item{dk}{Factor that indicates the individuals that belong to each domain of interest, is only necessary if "type" is equal to "estm.Ud".}
\item{type}{This argument indicates the procedure that will have the function ("select", "estm" or "estm.Ud"). If "type" is equal to "select" the function will make a selection, if it is equal to "estm" the function will perform the estimation of the indicated parameter and if it is equal to "estm.Ud" it will make an estimate in domain.}
\item{method}{Indicates the method or selection mechanism. If method is equal to "total cum." The function uses the total cumulative method or if it is equal to "lahiri" the function uses the method of Lahiri.}
\item{parameter}{This argument indicates the parameter to be estimated ("total", "mean", "prop", or "ratio"). }
\item{Nc}{ Confidence level (between 0 and 1), for the confidence interval of the estimator in case the type is equal to "estm" or "estm.Ud".}
\item{Ek}{Vector of random numbers of length equal to the size of the population. This argument is optional and by default the function generates them from a uniform distribution (0,1). }

}

\value{
This function returns two types of results using the PPT sampling design, depending on the "type" argument with which to select ("select") or estimate ("estm" or "estm.Ud").

If type is equal to "select" the function will return a list with two elements:
\item{Ksel}{Vector with the positions of the selected individuals.}
\item{pksel}{Selection probabilities vector of selected individuals.}

-If type="estm" or type="estm.Ud", the function returns a data frame with the estimation of the parameter of interest, the estimated variance of the estimator, the standard error, the coefficient of variation (in percentage) and a confidence interval.
}
\references{
Särndal, C. E., J. H. Wretman, and C. M. Cassel (1992). Foundations of Inference in Survey Sampling. Wiley New York.

Cochran, W. G. (1977). Sampling Techniques, 3ra ed. New York: Wiley.

Thompson, S. K. (1945). Wiley Series in Probability and Statistics, Sampling, 1ra ed. United States of America.
}
\author{ Jorge Alberto Barón Cárdenas <jorgeabaron@correo.unicordoba.edu.co>

Guillermo Martínez Flórez <guillermomartinez@correo.unicordoba.edu.co>}

\examples{
set.seed(12265)
yk<-rnorm(100,50,5)
zk<-rnorm(100,12,4)
set.seed(12245)
xk<-yk*runif(100,min=0.9,max=1.1)
r<-cor(yk,xk)
yk.p<-as.factor(ifelse(yk>50,"A","B"))

selection<-PPT(xk=xk,m=10,type="selec",method="acum.total")
PPT(yk=yk[selection$Ksel],pk=selection$pksel,type="estm",parameter="total")
PPT(yk=yk[selection$Ksel],pk=selection$pksel,type="estm",parameter="mean")
PPT(yk=yk.p[selection$Ksel],pk=selection$pksel,type="estm",parameter="prop")
PPT(yk=yk[selection$Ksel],zk=zk[selection$Ksel],pk=selection$pksel,
    type="estm",parameter="ratio")

# Domain Estimate

Sex<-rep(1:2,length=100)
dk<-factor(Sex,labels=c("Man","Woman"))
PPT(yk=yk[selection$Ksel],dk=dk[selection$Ksel],pk=selection$pksel,
    type="estm.Ud",parameter="total")
PPT(yk=yk[selection$Ksel],dk=dk[selection$Ksel],pk=selection$pksel,
    type="estm.Ud",parameter="mean")
PPT(yk=yk.p[selection$Ksel],dk=dk[selection$Ksel],pk=selection$pksel,
    type="estm.Ud",parameter="prop")
PPT(yk=yk[selection$Ksel],zk=zk[selection$Ksel],dk=dk[selection$Ksel],
    pk=selection$pksel,type="estm.Ud",parameter="ratio")

}
