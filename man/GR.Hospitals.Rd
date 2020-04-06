\name{GR.Hospitals}
\alias{GR.Hospitals}
\docType{data}
\title{Greek Hospitals}
\description{Locations of General and Specialised Hospitals in Greece.}
\usage{data("GR.Hospitals")}
\format{
  A data frame with 132 observations on the following 15 variables.
  \describe{
    \item{\code{Address}}{character vector of hospitals' addresses}
    \item{\code{Name}}{a character vector of hospitals' names}
    \item{\code{ID}}{a integer vector of hospitals' IDs}
    \item{\code{X}}{a numeric vector of x coordinates (GGRS87 - Greek Grid)}
    \item{\code{Y}}{a numeric vector of y coordinates (GGRS87 - Greek Grid)}
    \item{\code{Postcode}}{a numeric vector of the hospitals' postcodes}
    \item{\code{URL}}{a character vector of hospitals' websites}
    \item{\code{DYPE}}{a integer vector of hospitals' Healthcare Regions}
    \item{\code{KallCode}}{a character vector of municipality codes to link with data from the Hellenic Statistical Authority (EL.STAT.)}
    \item{\code{Dimos}}{a character vector of municipality names (Greek with latin characters)}
    \item{\code{Lat}}{a numeric vector of hospitals' latitudes (WGS84)}
    \item{\code{Lon}}{a numeric vector of hospitals' longitudes (WGS84)}
    \item{\code{Beds15}}{a integer vector of hospitals' beds in 2015}
    \item{\code{Patien15}}{a integer vector of hospitals' admitted patients (hospital discharges) in 2015}
    \item{\code{Nights15}}{a numeric vector of in-patients' nights in 2015}
  }
}
\details{
The X,Y coordinates (as well as the Lat/Lon coordinates) refer to the exact locations of operating hospitals in the summer 2016. Their identification is the results of registry data, formal hospital addresses, OpenStreetMap (https://www.openstreetmap.org) and Google Maps (https://maps.google.com) including Street View. They have been manually digitised by the author of this package.

}
\source{
The source of the hospital beds, hospital discharges and in-patient nights statistics is the Ministry of Health (http://www.moh.gov.gr/articles/bihealth/stoixeia-noshleytikhs-kinhshs/3865-stoixeia-noshleythentwn-sta-nosokomeia-toy-esy-etoys-2015?dl=1).
}
\references{
Kalogirou, S. (2017). Spatial inequality in the accessibility to hospitals in Greece, The International Archives of the Photogrammetry, Remote Sensing and Spatial Information Sciences, XLII-4/W2, 91-94, https://doi.org/10.5194/isprs-archives-XLII-4-W2-91-2017.
}
\examples{
data(GR.Hospitals)
hist(GR.Hospitals$Beds15)
}
\keyword{datasets}
\keyword{Greek Public Hospitals}
