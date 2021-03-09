\encoding{UTF-8}
\name{R.SIS}
\alias{R.SIS}

\title{R-Systematic Sampling Design}
\description{
The R.SIS function selects a random sample or estimates a parameter of interest under a r-systematic sampling design.
}
\usage{
R.SIS(N,n,r,yk=NULL,zk=NULL,fact=NULL,dk=NULL,type="selec",
      parameter="total",Nc=0.95,Ek=NULL)

# To select: R.SIS(N,n,r)

#To estimate: R.SIS(N,n,r,fact,yk,type="estm",parameter)

# To estimate in domains
# R.SIS(yk,fact,N,n,r,type="estm.Ud",parameter)
}

\arguments{
\item{N}{ Size of the population.}
\item{n}{Sample size.}
\item{r}{Number of starts.}
\item{yk}{Vector of observations of the characteristic of interest. This vector is only necessary if you want to estimate.}
\item{zk}{Vector of observations of the characteristic of interest of equal length that yk. This vector is necessary if the parameter of interest is the ratio and refers to the variable involved in the denominator of the ratio.}
\item{fact}{Factor indicating that Ur belongs to the observations of the variable of interest and yk. This factor is only necessary if type is equal to "estm" or "estm.Ud".}
\item{dk}{Factor that indicates the individuals that belong to each domain of interest, is only necessary if type is equal to "estm.Ud".}
\item{type}{This argument indicates the procedure that will have the function ("select", "estm" or "estm.Ud"). If "type" is equal to "select" the function will make a selection, if it is equal to "estm" the function will perform the estimation of the indicated parameter and if it is equal to "estm.Ud" it will make an estimate in domain.}
\item{parameter}{This argument indicates the parameter to be estimated ("total", "mean", "prop", or "ratio").}
\item{Nc}{ Confidence level (between 0 and 1), for the confidence interval of the estimator in case "type" is equal to "estm" or "estm.Ud".}
\item{Ek}{Vector of random numbers of length equal to the size of the population. This argument is optional and by default the function generates them from a uniform distribution (0,1). }

}

\value{
This function returns two types of results using the r-systematic sampling design, depending on the "type" argument with which to select ("select") or estimate
"estm.Ud").

If type="select", the function returns a list with four elements:
\item{Sel}{Array with r columns that refers to the clusters selected by each boot}
\item{Ksel}{Vector with selected individuals}
\item{fact}{factor indicating which start each selected individual belongs to}
\item{n.s}{Sample size}

If type="estm" or type="estm.Ud" the function returns a data frame with the estimation of the parameter of interest, the estimated variance of the estimator, the standard error, the coefficient of variation (in percent), an interval of confidence, the intraclass correlation coefficient and the intra-sample rate of variance.

}
\references{
Särndal, C. E., J. H. Wretman, and C. M. Cassel (1992). Foundations of Inference in Survey Sampling. Wiley New York.

Cochran, W. G. (1977). Sampling Techniques, 3ra ed. New York: Wiley.

Thompson, S. K. (1945). Wiley Series in Probability and Statistics, Sampling, 1ra ed. United States of America.
}
\author{ Jorge Alberto Barón Cárdenas <jorgeabaron@correo.unicordoba.edu.co>

Guillermo Martínez Flórez <guillermomartinez@correo.unicordoba.edu.co>}


\examples{
yk<-rnorm(100,40,2)
zk<-rnorm(100,12,2)
yk.p<-as.factor(ifelse(yk>40,"A","B"))
selection<-R.SIS(N=100,n=20,r=3,type="selec")


R.SIS(yk=yk[selection$Ksel],fact=selection$fact,N=100,n=20,r=3,
      type="estm",parameter="total")
R.SIS(yk=yk[selection$Ksel],fact=selection$fact,N=100,n=20,r=3,
       type="estm",parameter="mean")
R.SIS(yk=yk.p[selection$Ksel],fact=selection$fact,N=100,n=20,r=3,
       type="estm",parameter="prop")
R.SIS(yk=yk[selection$Ksel],zk=zk[selection$Ksel],fact=selection$fact,
       N=100,n=20,r=3,type="estm",parameter="ratio")


#Domain Estimate

Sex<-rep(1:2,length=100)
dk<-factor(Sex,labels=c("Man","Woman"))
R.SIS(yk=yk[selection$Ksel],fact=selection$fact,dk=dk[selection$Ksel],
      N=100,n=20,r=3,type="estm.Ud",parameter="total")
R.SIS(yk=yk[selection$Ksel],fact=selection$fact,dk=dk[selection$Ksel],
      N=100,n=20,r=3,type="estm.Ud",parameter="mean")
R.SIS(yk=yk.p[selection$Ksel],fact=selection$fact,dk=dk[selection$Ksel],
      N=100,n=20,r=3,type="estm.Ud",parameter="prop")
R.SIS(yk=yk[selection$Ksel],zk=zk[selection$Ksel],fact=selection$fact,
      dk=dk[selection$Ksel],N=100,n=20,r=3,type="estm.Ud",parameter="ratio")

}
