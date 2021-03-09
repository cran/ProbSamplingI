\encoding{UTF-8}
\name{M.MET}
\alias{M.MET}

\title{Multi-Stage Sampling}
\description{
The M.MET function selects a random sample or estimates an interest parameter under multi-stage sampling (up to four stages).
}
\usage{
M.MET(F.UM,designs,list.arg,p,type="selec",parameter="total",yk=NULL,
      zk=NULL,xk=NULL,dk=NULL,r=NULL,Nc=0.95)
# To select: M.MET(F.UM=F.UM,p=p,designs)
# To estimate: M.MET(yk,F.UM,p,designs,list.arg,type="estm",parameter)
}

\arguments{
\item{yk}{Vector of observations of the characteristic of interest. This vector is only necessary if you want to estimate.}
\item{zk}{Vector of observations of the characteristic of interest of equal length that yk. This vector is necessary if the parameter of interest is the ratio and refers to the variable involved in the denominator of the ratio.}
\item{xk}{Vector of observations of the auxiliary variable. This vector is only necessary if you want to select using a layout that uses an auxiliary variable.}
\item{dk}{Factor that indicates the individuals that belong to each domain of interest, Only needed if type is equal to "estm.Ud".}
\item{F.UM}{Data.frame that contains columns indicating which sampling unit each individual belongs to within each stage.}
\item{p}{Vector indicating the proportion of individuals to be selected at each sampling stage. This argument is necessary if the type is equal to "select".}
\item{designs}{Vector indicating the design to be used in each stage ("BER", "MAS", "MCR", "R.SIS", "PPT", "PiPT").}
\item{list.arg}{List of arguments required for the estimate}
\item{r}{Number of starts, this argument is only necessary if a r-systematic design is used in the last step.}
\item{type}{This argument indicates the procedure that will have the function ("select", "estm" or "estm.Ud"). If type is equal to "select" the function will make a selection, if it is equal to "estm" the function will perform the estimation of the indicated parameter and if it is equal to "estm.Ud" will make an estimate in domain.}
\item{parameter}{This argument indicates the parameter to be estimated ("total", "mean", "prop" or "ratio").}
\item{Nc}{Confidence level (between 0 and 1), for the confidence interval of the estimator in case the type is equal to "estm" or "estm.Ud".}
}

\value{
This function returns two types of results through the multi-stage sampling strategy that needs to be implemented, depending on the "type" argument, which indicates whether you want to select a sample ("select") or estimate a parameter ("estm" or "estm.Ud").

-If type="select", the function will return a list with two elements:
\item{Sample}{Data frame with the location of the selected individuals}
\item{Results}{List with the results obtained in each stage, which are necessary when making a certain estimate.}

-If type = "estm" or type = "estm.Ud", the function returns a data frame with the estimation of the parameter of interest, the estimated variance of the estimator, the standard error, the coefficient of variation (in percent) and a confidence interval.

}
\references{
Särndal, C. E., J. H. Wretman, and C. M. Cassel (1992). Foundations of Inference in Survey Sampling. Wiley New York.

Cochran, W. G. (1977). Sampling Techniques, 3ra ed. New York: Wiley.

Thompson, S. K. (1945). Wiley Series in Probability and Statistics, Sampling, 1ra ed. United States of America.
}
\author{ Jorge Alberto Barón Cárdenas <jorgeabaron@correo.unicordoba.edu.co>

Guillermo Martínez Flórez <guillermomartinez@correo.unicordoba.edu.co>}

\examples{
#Selection and estimation using a 4-stage sampling

F.UPM<-rep(1:5,each=1000)
F.USM<-rep(1:5,each=200,length=5000)
F.UTM<-rep(1:10,each=20,length=5000)
F.UCM<-rep(1:20,length=5000)
F.UM<-data.frame(F.UPM,F.USM,F.UTM,F.UCM)
p<-c(0.3,0.3,0.3,0.2)
y<-rnorm(5000,10,2)
z<-rnorm(5000,12,2)
y.p<-as.factor(ifelse(y>10,"A","B"))
Sex<-rep(1:2,length=5000)
d<-factor(Sex,labels=c("Man","Woman"))

designs<-c("MAS","MAS","MAS","MAS")
select<-M.MET(F.UM=F.UM,p=p,designs=designs)
F.UM.s<-select$Sample[6:8]
yk<-y[select$Sample$IND]
yk.p<-y.p[select$Sample$IND]
zk<-z[select$Sample$IND]
dk<-d[select$Sample$IND]
list<-select$Results
M.MET(yk=yk,F.UM=F.UM.s,p=p,designs=designs,list.arg=list,
      type="estm",parameter="total")
M.MET(yk=yk,F.UM=F.UM.s,p=p,designs=designs,list.arg=list,
      type="estm",parameter="mean")
M.MET(yk=yk.p,F.UM=F.UM.s,p=p,designs=designs,list.arg=list,
      type="estm",parameter="prop")
M.MET(yk=yk,zk=zk,F.UM=F.UM.s,p=p,designs=designs,list.arg=list,
      type="estm",parameter="ratio")
M.MET(yk=yk,dk=dk,F.UM=F.UM.s,p=p,designs=designs,list.arg=list,
      type="estm.Ud",parameter="total")
M.MET(yk=yk,dk=dk,F.UM=F.UM.s,p=p,designs=designs,list.arg=list,
      type="estm.Ud",parameter="mean")
M.MET(yk=yk.p,dk=dk,F.UM=F.UM.s,p=p,designs=designs,list.arg=list,
      type="estm.Ud",parameter="prop")
M.MET(yk=yk,zk=zk,dk=dk,F.UM=F.UM.s,p=p,designs=designs,list.arg=list,
       type="estm.Ud",parameter="ratio")


xk<-rnorm(5000,10,2)
designs<-c("PiPT","MAS","PiPT","MAS")
select2<-M.MET(xk=xk,F.UM=F.UM,p=p,designs=designs)
F.UM.s<-select2$Sample[6:8]
yk<-y[select2$Sample$IND]
yk.p<-y.p[select2$Sample$IND]
zk<-z[select2$Sample$IND]
dk<-d[select2$Sample$IND]
list<-select2$Results
M.MET(yk=yk,F.UM=F.UM.s,p=p,designs=designs,list.arg=list,
      type="estm",parameter="total")
M.MET(yk=yk,F.UM=F.UM.s,p=p,designs=designs,list.arg=list,
      type="estm",parameter="mean")
M.MET(yk=yk.p,F.UM=F.UM.s,p=p,designs=designs,list.arg=list,
      type="estm",parameter="prop")
M.MET(yk=yk,zk=zk,F.UM=F.UM.s,p=p,designs=designs,list.arg=list,
       type="estm",parameter="ratio")
M.MET(yk=yk,dk=dk,F.UM=F.UM.s,p=p,designs=designs,list.arg=list,
       type="estm.Ud",parameter="total")
M.MET(yk=yk,dk=dk,F.UM=F.UM.s,p=p,designs=designs,list.arg=list,
       type="estm.Ud",parameter="mean")
M.MET(yk=yk.p,dk=dk,F.UM=F.UM.s,p=p,designs=designs,list.arg=list,
      type="estm.Ud",parameter="prop")
M.MET(yk=yk,zk=zk,dk=dk,F.UM=F.UM.s,p=p,designs=designs,list.arg=list,
      type="estm.Ud",parameter="ratio")


}
