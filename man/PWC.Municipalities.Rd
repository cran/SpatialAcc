\name{PWC.Municipalities}
\alias{PWC.Municipalities}
\docType{data}
\title{
Population weighted centroids of the Municipalities in Greece.
}
\description{
Population weighted centroids of Municipalities in Greece (Programme Kallikratis).
}
\usage{data("PWC.Municipalities")}
\format{
  A data frame with 326 observations on the following 6 variables.
  \describe{
    \item{\code{KallCode}}{a character vector of municipality codes to link with data from the Hellenic Statistical Authority (EL.STAT.)}
    \item{\code{pwX}}{a numeric vector of population weighted x coordinates (GGRS87 / Greek Grid)}
    \item{\code{pwY}}{a numeric vector of population weighted y coordinates (GGRS87 / Greek Grid)}
    \item{\code{Pop}}{a numeric vector of population according to the 2011 Census for Population in Greece}
    \item{\code{Lat}}{a numeric vector of municipalitys' latitudes (WGS84)}
    \item{\code{Lon}}{a numeric vector of municipalitys' longitudes (WGS84)}
  }
}
\details{
The population weighted centroids for the 325 Municipalities and the Holy Mountain (Agion Oros) in Greece were computed based on the 13,548 settlements' x and y coordinates and their total population in 2011.}

\references{
Kalogirou, S. (2017). Spatial inequality in the accessibility to hospitals in Greece, FOSS4G Europe, Paris, July 18th to 22nd 2017.

Kalogirou, S., Spatial analysis of accessibility to public hospitals using GIS, in Medical Geographical Information - Applications, Analysis and Mapping, SPRINGER, under publication.
}
\examples{
data(PWC.Municipalities)
plot(PWC.Municipalities$pwX,PWC.Municipalities$pwY)
summary(PWC.Municipalities$Pop)
}
\keyword{datasets}
