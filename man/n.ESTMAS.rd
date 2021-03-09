\encoding{UTF-8}
\name{n.ESTMAS}
\alias{n.ESTMAS}

\title{Sample Size Through Stratified Sampling}
\description{
The n.ESTMAS function determines the sample size with its corresponding allocation by stratum, using a stratified sampling strategy, where a simple random sampling design with no replacement (ESTMAS) is applied in each stratum; taking into account whether the parameter of interest is the average (or total) or a proportion.
}
\usage{
n.ESTMAS(Nh,Sh,Ch,Ph,Emax.a,Nc=0.95,parameter="mean",Asig="Optima")

# n.ESTMAS(Nh,Sh,Ch,Emax.a,Nc=0.95,parameter="mean",Asig="Optima")
# n.ESTMAS(Nh,Ph,Ch,Emax.a,Nc=0.95,parameter="prop",Asig="Optima")

# n.ESTMAS(Nh,Sh,Emax.a,Nc=0.95,parameter="mean",Asig="Neyman")
# n.ESTMAS(Nh,Ph,Emax.a,Nc=0.95,parameter="prop",Asig="Neyman")

# n.ESTMAS(Nh,Sh,Emax.a,Nc=0.95,parameter="mean",Asig="Proportional")
# n.ESTMAS(Nh,Ph,Emax.a,Nc=0.95,parameter="prop",Asig="Proportional")
}

\arguments{
\item{Nh}{Numerical vector with the respective sizes of strata.}
\item{Sh}{Numerical vector with the respective standard deviations of the variable of interest of each stratum. This argument is necessary only if the parameter of interest is the mean.}
\item{Ch}{Numerical vector with the costs of sampling an element within each stratum. This argument is only necessary if the allocation by stratum is the optimal allocation.}
\item{Ph}{Numerical vector with estimated proportions within each stratum.}
\item{Emax.a}{Absolute maximum error.}
\item{parameter}{Type of parameter to be estimated, either the mean or a proportion ("mean", "prop").}
\item{Nc}{Confidence level (between 0 and 1) that you want to set.}
\item{Asig}{Assignment by stratum ("Optima", "Neyman" or "Proportional")}
}

\value{
This function returns the sample size and the allocation by stratum, through the conditions established in the arguments.
}
\references{
Särndal, C. E., J. H. Wretman, and C. M. Cassel (1992). Foundations of Inference in Survey Sampling. Wiley New York.

Cochran, W. G. (1977). Sampling Techniques, 3ra ed. New York: Wiley.

Thompson, S. K. (1945). Wiley Series in Probability and Statistics, Sampling, 1ra ed. United States of America.
}
\author{ Jorge Alberto Barón Cárdenas <jorgeabaron@correo.unicordoba.edu.co>

Guillermo Martínez Flórez <guillermomartinez@correo.unicordoba.edu.co>}

\examples{

Nc<-0.95
E<-0.3
Nh<-c(400,220,380)
Sh<-sqrt(c(0.7521,1.4366,1.1361))
Ph<-c(0.4,0.2,0.6)
Ch<-c(1000,1200,1500)

# Optimal Assignment
n.ESTMAS(Nh=Nh,Sh=Sh,Ch=Ch,E=E,Nc=0.95,parameter="mean",Asig="Optima")
n.ESTMAS(Nh=Nh,Ph=Ph,Ch=Ch,E=E,Nc=0.95,parameter="prop",Asig="Optima")

# Neyman Assignment
n.ESTMAS(Nh=Nh,Sh=Sh,E=E,Nc=0.95,parameter="mean",Asig="Neyman")
n.ESTMAS(Nh=Nh,Ph=Ph,E=E,Nc=0.95,parameter="prop",Asig="Neyman")

# Proportional Assignment
n.ESTMAS(Nh=Nh,Sh=Sh,E=E,Nc=0.95,parameter="mean",Asig="Proportional")
n.ESTMAS(Nh=Nh,Ph=Ph,E=E,Nc=0.95,parameter="prop",Asig="Proportional")


}
