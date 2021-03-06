% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Durbin2.R
\name{Durbin2}
\alias{Durbin2}
\title{Durbin two stage method}
\usage{
Durbin2(y, x)
}
\arguments{
\item{y}{series name}

\item{x}{series name,}
}
\description{
This function makes Durbin two stage method for autocorrelation.
}
\examples{
IHR = REcoData$IHR
ITH = REcoData$ITH
Durbin2(ITH,IHR)

}
\references{
Selahattin Güriş, Ebru Çağlayan Akay, Burak Güriş, R ile Temel Ekonometri, DER Yayinevi, 2020.
}
\keyword{Autocorrelation}
