\encoding{UTF-8}
\name{n.MAS}
\alias{n.MAS}

\title{Sample Size Using Simple Random Sampling Design Without Replacement}
\description{
The n.MAS function determines the sample size by a simple random sample design without replacement, taking into account whether the parameter of interest is the mean (or total) or a proportion.
}
\usage{
n.MAS(N,Argt,Nc=0.95,opc=2)

# n.MAS(N,Argt=c(S,Emax.a),opc=1,Nc=0.95)
# n.MAS(N,Argt=c(Cve,Emax.r),opc=2,Nc=0.95)
# n.MAS(N,Argt=c(p,Emax.a),opc=3,Nc=0.95)
# n.MAS(N,Argt=c(p,Emax.r),opc=4,Nc=0.95)
}

\arguments{
\item{N}{Population size.}
\item{opc}{Numeric value from 1 to 4, which indicates the option to choose.}
\item{Argt}{Vector of length two, in which its components depends on the chosen option ("opc"). If option 1, (opc = 1) is chosen, the components of the Argt vector are in their order, the standard deviation of the variable of interest and the respective absolute maximum error that can be admitted; If option 2 (opc = 2) is chosen, the components of the Argt vector are respectively the estimated coefficient of variation and the relative maximum error to be controlled; If option 3 (opc = 3) is chosen, the components are the estimated proportion and absolute maximum error that can be admitted; And if option 4 (opc = 4) is chosen, the components are the estimated ratio and the relative maximum error respectively.}

\item{Nc}{Confidence level (between 0 and 1) that you want to set.}

}

\value{
This function returns the sample size through the conditions set in the arguments.

}
\references{
Särndal, C. E., J. H. Wretman, and C. M. Cassel (1992). Foundations of Inference in Survey Sampling. Wiley New York.

Cochran, W. G. (1977). Sampling Techniques, 3ra ed. New York: Wiley.

Thompson, S. K. (1945). Wiley Series in Probability and Statistics, Sampling, 1ra ed. United States of America.
}
\author{ Jorge Alberto Barón Cárdenas <jorgeabaron@correo.unicordoba.edu.co>

Guillermo Martínez Flórez <guillermomartinez@correo.unicordoba.edu.co>}

\examples{
# Sample size for the mean (or total) when you want to control the absolute maximum error.

Nc<-0.95
S<-sqrt(6.0590)
Emax.a<-0.2
N<-10000
n.MAS(N=N,Argt=c(S,Emax.a),opc=1)


# Sample size for the mean (or total) when you want to control the relative maximum error.

Cve<-0.4346
Emax.r<-0.05
N<-10000
n.MAS(N=N,Argt=c(Cve,Emax.r))

# Sample size for proportions when you want to control the absolute maximum error.

N<-10000
p<-14/30
Emax.a<-0.04
Nc<-0.9
n.MAS(N=N,Argt=c(p,Emax.a),opc=3,Nc=Nc)

# Sample size for proportions when you want to control the relative maximum error.

N<-10000
p<- 14/30
Emax.r<-0.1
Nc<-0.9
n.MAS(N=N,Argt=c(p,Emax.r),opc=4,Nc=Nc)
}
