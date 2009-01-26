% $Id: data.Rd,v 1.21 2008/04/25 13:35:23 imosqueira Exp $
\name{data}
\alias{ple4}
\alias{ple4sex}
\alias{ple4.sr}
\alias{ple4.biol}
\alias{ple4.index}
\alias{ple4.indices}
\alias{nsher}
\alias{bt4}
\title{FLCore datasets}
\description{
    \item{ple4}{A dataset of North Sea (ICES Area IV) plaice catch, yield, landings, discards, natural mortality, weight-at-age and maturity, together with the VPA estimated abundances and fishing mortalities, contained in an FLStock object.}
    \item{ple4sex}{A dataset of North Sea (ICES Area IV) plaice disaggregated by sex catch, yield, landings, discards, natural mortality, weight-at-age and maturity, together with the VPA estimated abundances and fishing mortalities, contained in an FLStock object.}
    \item{ple4.sr}{A dataset of North Sea (ICES Area IV) plaice stock and recruitment.}
    \item{ple4.index}{A dataset of North Sea (ICES Area IV) plaice survey catch per unit effort, index and index variance, contained in an FLIndex object.}
    \item{ple4.indices}{A dataset of two North Sea (ICES Area IV) plaice survey catch per unit effort, index and index variance, contained in an FLIndices object.}
    \item{ple4.index}{A dataset of North Sea (ICES Area IV) plaice survey catch per unit effort, index and index variance, contained in an FLIndex object.}
    \item{ple4.indices}{A dataset of two North Sea (ICES Area IV) plaice survey catch per unit effort, index and index variance, contained in an FLIndices object.}
    \item{ple.biol}{A dataset of North Sea plaice population, numbers, natural mortality, mass and fecundity-at-age, contained in an FLBiol object.}
%    \item{north.beam}{A dataset of class FLFleet corresponding to the northern North Sea beam trawl fleet fishing on plaice.}
%    \item{south.beam}{A dataset of class FLFleet corresponding to the southern North Sea beam trawl fleet fishing on sole and plaice.}
	\item{nsher}{A dataset of class \code{FLStock} for autumn spawning North Sea herring.}
    \item{nsher.sr}{A dataset of North Sea autumn spawning herring stock and recruitment relationship. Fitted using the 'Ricker' model.}
	\item{nsher.biol}{A dataset of class \code{FLBiol} for North Sea herring.}
    \item{bt4}{A beam trawl fleet catching plaice and sole in North Sea area 4.}

	Datasets can be loaded by issuing the \code{data} command, like in \code{data(ple4)}.
}
\seealso{
\code{\link{FLStock}}, \code{\link{FLSR}}, \code{\link{FLIndex}, \code{\link{FLStock}}, \code{\link{FLIndex}}, \code{\link{FLBiol}}, \code{\link{FLFleet}}}
}
\references{
ICES.
}
\keyword{datasets}
