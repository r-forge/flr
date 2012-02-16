% lh() function
\name{lh}
\alias{lh}
\alias{lhg}

\title{Generate FLBRP objects using life history parmameters.}

\description{Generates an FLBRP object based on the input life history parameters and generic functions, e.g. those from Gislason et al 2008.}

\usage{
lh(
            par,
            growth       =vonB,
            fnM          =function(par,len,T=290,a=FLPar(c(a=-2.1104327,b=-1.7023068,c=1.5067827,d=0.9664798,e=763.5074169),iter=dims(par)$iter))
                                    exp(a[1]+a[2]*log(len) + a[3]*log(par["linf"]) + a[4]*log(par["k"]) + a[5]/T),
            fnMat        =logistic,
            fnSel        =dnormalFn,
            sr           =list(model="bevholt",s=0.9,v=1e3),
            age=seq(from=1, to = 40, by = 1),
            m.spwn = 0,
            h.spwn = m.spwn,
            f.year.prop = 0.5,
            m.year.prop = 0.5,
            T=290,
            ...)
}

\arguments{
\item{par}{The life history parameters, passed as an \code{FLPar} object.}
\item{growth}{Function to describe the growth. Default is the von Bertalanffy growth equation.}
\item{fnM}{Function to describe the natural mortality at length.}
\item{fnMat}{Function to describe the maturity ogive.}
\item{selFn}{Function to describe the selectivity ogive.}
\item{sr}{The stock recruitment relationship described as a \code{list} of the model name and the parameters.}
\item{age}{The ages of the \code{FLBRP} object.}
\item{m.spwn}{The proportion of the natural mortality before spawning.}
\item{h.spwn}{The proportion of the fishing mortality before spawning.}
\item{f.year.prop = 0.5}{The fraction of the year at which fishing occurs.}
\item{m.year.prop = 0.5}{The fraction of the year at which natural mortality occurs.}
\item{T}{Temperature.}
\item{...}{Other arguments.}
}

\details{
The \emph{a} and \emph{b} parameter values in the \code{par} object should convert g to cm.
}

\value{
An \code{FLBRP} object.
}

\references{
Gislason, H., Pope, J. G., Rice, J. C. and Daan, N. (2008)
\emph{Coexistence in North Sea fish communities: implications for growth and natural mortality.}
ICES Journal of Marine Science, 65: 514-530
}

\author{
The FLR team
}

\seealso{
\code{\link{gislasim}}
}

\examples{
lh1 <- gislasim(FLPar(linf = 130))
brp1 <- lh(lh1)
}

