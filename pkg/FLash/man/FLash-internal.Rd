% --- Source file: man/FLash-internal.Rd ---
\name{FLash-internal}
\alias{fwdTranslateSRModel}
\alias{fwdSetSR}
\alias{fwdSetSRs}
\alias{checkTarget}
\alias{checkControl}
\alias{matrixTarget}
\alias{matrixControl}

\title{Internal FLash functions}
\description{
  Internal FLash functions: not exported from the \pkg{FLash} namespace
}
\usage{
fwdTranslateSRModel(sr.model,yr)
fwdSetSR(object, year, sr.model="mean", sr.param=1, sr.residuals="missing", sr.residuals.mult=TRUE)
fwdSetSRs(object, year, sr.model="mean", sr.param=1, sr.residuals="missing", sr.residuals.mult=TRUE)
fwdSetSRs(object, year, sr.model="mean", sr.param=1, sr.residuals="missing", sr.residuals.mult=TRUE)
checkTarget(target)
checkControl(control)
matrixTarget(target)
matrixControl(control)
}
\keyword{internal}
