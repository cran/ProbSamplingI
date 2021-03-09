\encoding{UTF-8}
\name{CONGL}
\alias{CONGL}

\title{Conglomerate Sampling}
\description{
The CONGL function selects a random sample or estimates a parameter of interest under a cluster sampling design
}
\usage{
CONGL(Argt,cong,design="MAS",type="selec",parameter="total",yk=NULL,
 zk=NULL,dk=NULL,Ek=NULL,Nc=0.95)

# To select: CONGL(Argt=Argt,design)
# To estimate: CONGL(yk,cong,Argt,design,type="estm")
# To estimate in domains: CONGL(yk,dk,cong,Argt,design,type="estm.Ud")

# If the objective is to select a sample, the Argt argument is constructed as follows:

# "MAS": Argt<-list(NI,nI)
# "MCR": Argt<-list(NI,mI)
# "BER": Argt<-list(NI,PiI)
# "PPT": Argt<-list(txkI,mI)
# "PiPT": Argt<-list(txkI,nI)

# If the objective is to estimate a parameter of interest, the Argt argument is
# constructed as follows:

# "MAS": Argt<-list(NI,nI)
# "MCR": Argt<-list(NI,mI)
# "BER": Argt<-list(NI,PiI)
# "PPT": Argt<-list(pkI)
# "PiPT": Argt<-list(pikI,mpiklI)

}

\arguments{
\item{yk}{Vector of observations of the characteristic of interest. This vector is only necessary if you want to estimate.}
\item{zk}{Vector of observations of the characteristic of interest of equal length that yk. This vector is necessary if the parameter of interest is the ratio and refers to the variable involved in the denominator of the ratio.}
\item{dk}{Factor that indicates the individuals that belong to each domain of interest, Only needed if "type"" is equal to "estm.Ud".}
\item{type}{This argument indicates the procedure that will have the function ("select", "estm" or "estm.Ud"). If type is equal to "select" the function will make a selection, if it is equal to "estm" the function will make the estimation of the indicated parameter and if it is equal to "estm.Ud" it will make an estimate in domain.}
\item{Argt}{List with the necessary arguments to select or estimate by the design that you want to use.}
\item{cong}{Vector indicating which cluster each individual belongs to.}
\item{parameter}{This argument indicates the parameter to be estimated ("total", "mean", "prop", or "ratio"). }
\item{design}{Sampling sampling design to be implemented ("BER", "MAS", "MCR", "PPT", "SIS" or "PiPT").}
\item{Nc}{Confidence level (between 0 and 1), for the confidence interval of the estimator in case "type" is equal to "estm" or "estm.Ud".}
\item{Ek}{Vector of random numbers of length equal to the size of the population. This argument is optional and by default the function generates them from a uniform distribution (0,1).}

}

\value{
This function returns two types of results under the cluster sampling design, depending on the "type" argument, which indicates whether to select a sample ("select") or to estimate an interest parameter ("estm", "estm.Ud"). The results obtained in each case depend on the design implemented, in this way, such results are the same ones obtained for the case of element sampling, but nevertheless in the estimation of the total the intra-sample rate of variance is appended (IVI).

}
\references{
Särndal, C. E., J. H. Wretman, and C. M. Cassel (1992). Foundations of Inference in Survey Sampling. Wiley New York.

Cochran, W. G. (1977). Sampling Techniques, 3ra ed. New York: Wiley.

Thompson, S. K. (1945). Wiley Series in Probability and Statistics, Sampling, 1ra ed. United States of America.
}
\author{ Jorge Alberto Barón Cárdenas <jorgeabaron@correo.unicordoba.edu.co>

Guillermo Martínez Flórez <guillermomartinez@correo.unicordoba.edu.co>}

\examples{


yk<-rnorm(120,10,2)
zk<-rnorm(120,12,2)
yk.p<-as.factor(ifelse(yk>10,1,0))
cong<-rep(1:12,each=10);cong
Sex<-rep(1:2,each=60)
dk<-factor(Sex,labels=c("Man","Woman"))
tyi<-tapply(yk,cong,sum)
txkI<-runif(12,0.95,1.1)*tyi
cor(tyi,txkI)
D1<-data.frame(cong,yk,yk.p,zk,dk)


# MAS-CONGLOMERATE

Argt<-list(NI=12,nI=3)
selection<-CONGL(Argt=Argt,design="MAS")
D.sel<-D1[WHICH1(selection$Ksel,cong),]
CONGL(yk=D.sel$yk,cong=D.sel$cong,Argt=Argt,design="MAS",type="estm")
CONGL(yk=D.sel$yk,cong=D.sel$cong,Argt=Argt,design="MAS",type="estm",
      parameter="mean")
CONGL(yk=D.sel$yk,zk=D.sel$zk,cong=D.sel$cong,Argt=Argt,design="MAS",
      type="estm",parameter="ratio")
CONGL(yk=D.sel$yk.p,cong=D.sel$cong,Argt=Argt,design="MAS",type="estm",
      parameter="prop")

#MCR-CONGLOMERATE

Argt<-list(NI=10,mI=3)
selection<-CONGL(Argt=Argt,design="MCR")
D.sel<-D1[WHICH1(selection$Ksel,cong),]
Ni<-table(cong)[selection$Ksel]
cong.s<-rep(1:3,Ni)
CONGL(yk=D.sel$yk,cong=cong.s,Argt=Argt,design="MCR",type="estm")
CONGL(yk=D.sel$yk,cong=cong.s,Argt=Argt,design="MCR",type="estm",parameter="mean")
CONGL(yk=D.sel$yk,zk=D.sel$zk,cong=cong.s,Argt=Argt,design="MCR",type="estm",
       parameter="ratio")
CONGL(yk=D.sel$yk.p,cong=cong.s,Argt=Argt,design="MCR",type="estm",parameter="prop")

#BER-CONGLOMERATE

Argt<-list(NI=10,PiI=0.4)
selection<-CONGL(Argt=Argt,design="BER")
D.sel<-D1[WHICH1(selection$Ksel,cong),]
CONGL(yk=D.sel$yk,cong=D.sel$cong,Argt=Argt,design="BER",type="estm")
CONGL(yk=D.sel$yk,cong=D.sel$cong,Argt=Argt,design="BER",type="estm",
      parameter="mean")
CONGL(yk=D.sel$yk,zk=D.sel$zk,cong=D.sel$cong,Argt=Argt,design="BER",
      type="estm",parameter="ratio")
CONGL(yk=D.sel$yk.p,cong=D.sel$cong,Argt=Argt,design="BER",type="estm",
      parameter="prop")

#PPT-CONGLOMERATE

Argt<-list(txkI=txkI,mI=4)
selection<-CONGL(Argt=Argt,design="PPT") ;selection
Argt<-list(pkI=selection$pksel)
D.sel<-D1[WHICH1(selection$Ksel,cong),]
Ni<-table(cong)[selection$Ksel]
cong.s<-rep(1:4,Ni)
CONGL(yk=D.sel$yk,cong=cong.s,Argt=Argt,design="PPT",type="estm")
CONGL(yk=D.sel$yk,cong=cong.s,Argt=Argt,design="PPT",type="estm",parameter="mean")
CONGL(yk=D.sel$yk,zk=D.sel$zk,cong=cong.s,Argt=Argt,design="PPT",type="estm",
      parameter="ratio")
CONGL(yk=D.sel$yk.p,cong=cong.s,Argt=Argt,design="PPT",type="estm",parameter="prop")


#PiPT-CONGLOMERATE

Argt<-list(txkI=txkI,nI=4)
selection<-CONGL(Argt=Argt,design="PiPT")
Argt<-list(pikI=selection$piksel,mpiklI=selection$mpikl.s)
D.sel<-D1[WHICH1(selection$Ksel,cong),]
CONGL(yk=D.sel$yk,cong=D.sel$cong,Argt=Argt,design="PiPT",type="estm")
CONGL(yk=D.sel$yk,cong=D.sel$cong,Argt=Argt,design="PiPT",type="estm",
       parameter="mean")
CONGL(yk=D.sel$yk,zk=D.sel$zk,cong=D.sel$cong,Argt=Argt,design="PiPT",
       type="estm",parameter="ratio")
CONGL(yk=D.sel$yk.p,cong=D.sel$cong,Argt=Argt,design="PiPT",type="estm",
      parameter="prop")


# Domain Estimate
# MAS-CONGLOMERATE

Argt<-list(NI=12,nI=3)
selection<-CONGL(Argt=Argt,design="MAS")
D.sel<-D1[WHICH1(selection$Ksel,cong),]
CONGL(yk=D.sel$yk,dk=D.sel$dk,cong=D.sel$cong,Argt=Argt,
      design="MAS",type="estm.Ud")
CONGL(yk=D.sel$yk,dk=D.sel$dk,cong=D.sel$cong,Argt=Argt,
      design="MAS",type="estm.Ud",parameter="mean")
CONGL(yk=D.sel$yk,zk=D.sel$zk,dk=D.sel$dk,cong=D.sel$cong,Argt=Argt,
       design="MAS",type="estm.Ud",parameter="ratio")
CONGL(yk=D.sel$yk.p,dk=D.sel$dk,cong=D.sel$cong,Argt=Argt,
      design="MAS",type="estm.Ud",parameter="prop")

# Domain Estimate
# MCR-CONGLOMERATE

Argt<-list(NI=10,mI=3)
selection<-CONGL(Argt=Argt,design="MCR")
D.sel<-D1[WHICH1(selection$Ksel,cong),]
Ni<-table(cong)[selection$Ksel]
cong.s<-rep(1:3,Ni)
CONGL(yk=D.sel$yk,dk=D.sel$dk,cong=cong.s,Argt=Argt,
      design="MCR",type="estm.Ud")
CONGL(yk=D.sel$yk,dk=D.sel$dk,cong=cong.s,Argt=Argt,design="MCR",
      type="estm.Ud",parameter="mean")
CONGL(yk=D.sel$yk,zk=D.sel$zk,dk=D.sel$dk,cong=cong.s,Argt=Argt,
      design="MCR",type="estm.Ud",parameter="ratio")
CONGL(yk=D.sel$yk.p,dk=D.sel$dk,cong=cong.s,Argt=Argt,design="MCR",
       type="estm.Ud",parameter="prop")

# Domain Estimate
# BER-CONGLOMERATE

Argt<-list(NI=10,PiI=0.4)
selection<-CONGL(Argt=Argt,design="BER")
D.sel<-D1[WHICH1(selection$Ksel,cong),]
CONGL(yk=D.sel$yk,dk=D.sel$dk,cong=D.sel$cong,Argt=Argt,
      design="BER",type="estm.Ud")
CONGL(yk=D.sel$yk,dk=D.sel$dk,cong=D.sel$cong,Argt=Argt,
      design="BER",type="estm.Ud",parameter="mean")
CONGL(yk=D.sel$yk,dk=D.sel$dk,zk=D.sel$zk,cong=D.sel$cong,Argt=Argt,
      design="BER",type="estm.Ud",parameter="ratio")
CONGL(yk=D.sel$yk.p,dk=D.sel$dk,cong=D.sel$cong,Argt=Argt,
      design="BER",type="estm.Ud",parameter="prop")


}
