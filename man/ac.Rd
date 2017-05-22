\name{ac}
\alias{ac}
\title{Spatial Accessibility measures}
\description{
This function measures the accesibility certain geographical areas have to certain amenities. The former usually refer to the population residence and the latter to services such as health care, education, and culture. Several functions may define the accessibility measure. See family for further details.}
\usage{
ac(p, n, D, d0, power=2, family="SAM")
}
\arguments{
  \item{p}{is a vector that quantifies the demand for services in each location, usually the population at risk. For example, it may be the number of older people interested in geriatric services at hospitals.}
  \item{n}{is a vector that quantifies the supply of services in each location, usually a characteristic of the service. For example, it may be the number of beds at hospitals.}
   \item{D}{a matrix of a quantity separating the demand from the supply. This is usually a distance matrix, preferably using a road network distance or travel time through the road network.}
  \item{d0}{is the threshold distance or time that defines the catchment area (spatial kernel)}
   \item{power}{the power of the separation variable. This is usually 2 from the theory of the gravity model in geography.}
    \item{family}{a character value to define the accessibility measure function. SAM are the initials of Spatial Accessibility Measure refering to the function \deqn{A_i = \Sigma_j (n_j / (p_i * D_{ij}^2))} discussed in Kalogirou and Foley (2006).}
}
\details{SAM refers to the Spatial Accessibily Measure proposed by Kalogirou and Foley (2006).}

\value{a vector with accessibility measures for all locations (defined by the length of p).}

\references{
Kalogirou, S., and Foley, R. (2006). Health, place and Hanly: Modelling accessibility to hospitals in Ireland. Irish Geography, 39(1), 52-68.

Kalogirou, S. (2017). Spatial inequality in the accessibility to hospitals in Greece, FOSS4G Europe, Paris, July 18th to 22nd 2017.

Kalogirou, S., Spatial analysis of accessibility to public hospitals using GIS, in Medical Geographical Information - Applications, Analysis and Mapping, SPRINGER, under publication.}

\author{Stamatis Kalogirou <skalo@hua.gr>}

\examples{
n<-1:4
p<-20:25
D<-matrix(1:24, ncol=4, nrow=6)
Acc.p <- ac(n, p, D)
}

\keyword{accessibility}
\keyword{spatial accessibility}
