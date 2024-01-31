\encoding{UTF-8}
\name{ESTRAT}
\alias{ESTRAT}

\title{Stratified Sampling}
\description{
The ESTRAT function selects a random sample or estimates an interest parameter under a stratified sampling.
}
\usage{
ESTRAT(strata,designs,nh,xk=NULL,yk=NULL,zk=NULL,dk=NULL,type="selec",
       Argt,parameter="total",rh=NULL,Ek=NULL,Nc=0.95)
# To select: ESTRAT(strata,nh,designs,xk,rh)
# To estimate: ESTRAT(yk,zk,strata,designs,type="estm",Argt,parameter)
# To estimate in domains: ESTRAT(yk,zk,dk,strata,designs,type="estm",Argt,parameter)
}

\arguments{
\item{strata}{Vector indicating which stratum each individual belongs to.}
\item{nh}{Vector that indicates the number of individuals to select in each stratum. This argument is required if the type argument is equal to "select".}
\item{yk}{Vector of observations of the characteristic of interest. This vector is only necessary if you want to estimate.}
\item{zk}{Vector of observations of the characteristic of interest of equal length that yk. This vector is necessary if the parameter of interest is the ratio and refers to the variable involved in the denominator of the ratio.}
\item{xk}{Vector of observations of the auxiliary variable. This vector is only necessary if it is desired to select in any stratum by means of a probability selection or inclusion probability proportional to size design.}
\item{dk}{Factor that indicates the individuals that belong to each domain of interest, is only necessary if type is equal to "estm.Ud". }
\item{type}{This argument indicates the procedure that will have the function ("select", "estm" or "estm.Ud"). If type is equal to "select" the function will make a selection, if it is equal to "estm" the function will perform the estimation of the indicated parameter and if it is equal to "estm.Ud" will make an estimate in domain.}
\item{designs}{Vector indicating the design to be used in each stratum ("BER", "MAS", "MCR", "PPT", "SIS" or "PiPT").}
\item{parameter}{This argument indicates the parameter to be estimated ("total", "average", "prop" or "reason"). }
\item{Argt}{It is a list with the necessary arguments for the estimates under the respective designs used in the strata.}
\item{rh}{Vector of size equal to the number of strata, necessary if it is desired to select under an r-sistematic design, which will have the number of starts to be used in the corresponding strata and zeros in the rest of the positions where this design is not used.}
\item{Nc}{ Confidence level (between 0 and 1), for the confidence interval of the estimator in case the type is equal to "estm" or "estm.Ud".}
\item{Ek}{Vector of random numbers of length equal to population size. This argument is optional and by default the function generates them from a uniform distribution (0,1). }

}

\value{
This function returns two types of results under the stratified sampling design depending on the "type" argument, which indicates whether to select ("select") or estimate ("estm", "estm.Ud"). If type is equal to "select" the function returns a list with two elements, the first is a data frame (Sample) in which one of its columns indicates the position of the selected individuals in each stratum and the second (Rtdos.h ) is a list with the results obtained in each stratum which are necessary when making a certain estimate. If type is equal to "est" or "estm.Ud", the function returns a list with two data frames with the estimation of the parameter of interest, the estimated variance of the estimator, the standard error, the coefficient of variation (in percentage) and a confidence interval assuming normality; by stratum and in general.

}
\references{
Särndal, C. E., J. H. Wretman, and C. M. Cassel (1992). Foundations of Inference in Survey Sampling. Wiley New York.

Cochran, W. G. (1977). Sampling Techniques, 3ra ed. New York: Wiley.

Thompson, S. K. (1945). Wiley Series in Probability and Statistics, Sampling, 1ra ed. United States of America.
}
\author{ Jorge Alberto Barón Cárdenas <jorgeabaron@correo.unicordoba.edu.co>

Guillermo Martínez Flórez <guillermomartinez@correo.unicordoba.edu.co>}

\examples{
yk<-rnorm(1000,10,2)
xk<-rnorm(1000,10,3)
zk<-rnorm(1000,12,3)
yk.p<-factor(ifelse(yk>10,"A","B"))
strata<-rep(1:5,each=200)
Sex<-rep(1:2,length=1000)
dk<-factor(Sex,labels=c("Man","Woman"))


nh<-c(60,40,40,60,80)
designs<-c("MAS","MAS","MAS","MAS","MAS")
select<-ESTRAT(strata=strata,designs=designs,nh=nh)
Argt<-select$Rtdos.h
Strata<-strata[select$Sample$IND]
yksel<-yk[select$Sample$IND]
yk.psel<-as.factor(yk.p[select$Sample$IND])
zksel<-zk[select$Sample$IND]
ESTRAT(yk=yksel,strata=Strata,designs=designs,Argt=Argt,
       type="estm",parameter="total")
ESTRAT(yk=yksel,strata=Strata,designs=designs,Argt=Argt,
       type="estm",parameter="mean")
ESTRAT(yk=yk.psel,strata=Strata,designs=designs,Argt=Argt,
      type="estm",parameter="prop")
ESTRAT(yk=yksel,zk=zksel,strata=Strata,designs=designs,Argt=Argt,
       type="estm",parameter="ratio")


designs<-c("PiPT","PPT","MAS","MCR","BER")
select<-ESTRAT(xk=xk,strata=strata,designs=designs,nh)
Argt<-select$Rtdos.h
Strata<-strata[select$Sample$IND]
yksel<-yk[select$Sample$IND]
yk.psel<-yk.p[select$Sample$IND]
zksel<-zk[select$Sample$IND]
ESTRAT(yk=yksel,strata=Strata,designs=designs,Argt=Argt,
       type="estm",parameter="total")
ESTRAT(yk=yk.psel,strata=Strata,designs=designs,Argt=Argt,
      type="estm",parameter="prop")
ESTRAT(yk=yksel,strata=Strata,designs=designs,Argt=Argt,
       type="estm",parameter="mean")
ESTRAT(yk=yksel,zk=zksel,strata=Strata,designs=designs,Argt=Argt,
       type="estm",parameter="ratio")

# Estimates in Domains

designs<-c("MAS","MAS","MAS","MAS","MAS")
select<-ESTRAT(strata=strata,designs=designs,nh=nh)
Argt<-select$Rtdos.h
Strata<-strata[select$Sample$IND]
yksel<-yk[select$Sample$IND]
yk.psel<-yk.p[select$Sample$IND]
zksel<-zk[select$Sample$IND]
dksel<-dk[select$Sample$IND]
ESTRAT(yk=yksel,strata=Strata,dk=dksel,designs=designs,Argt=Argt,
        type="estm.Ud",parameter="total")
ESTRAT(yk=yksel,strata=Strata,dk=dksel,designs=designs,Argt=Argt,
      type="estm.Ud",parameter="mean")
ESTRAT(yk=yk.psel,strata=Strata,dk=dksel,designs=designs,Argt=Argt,
       type="estm.Ud",parameter="prop")
ESTRAT(yk=yksel,zk=zksel,strata=Strata,dk=dksel,designs=designs,
       Argt=Argt,type="estm.Ud",parameter="ratio")

}
