\name{distance}
\alias{distance}
\title{Distance Matrix Computation}
\description{Computes Euclidean or Manhattan distances for all pairs of points between two sets of points.}
\usage{
distance(m1, m2, type = "euclidean")
}
\arguments{
  \item{m1}{a vector refering to a set of origin point coordinates}
  \item{m2}{a vector refering to a set of destination point coordinates}
  \item{type}{the type of distance to be computed. Two types are currently supported "euclidean" and "manhattan".}
}
\details{When m1 and m2 are idential the function is equivalent to the \code{\link{dist}} function of base R.}

\value{A distances matrix in which rows refer to origins and columns refer to destinations.}

\author{Stamatis Kalogirou <skalo@hua.gr>}


\seealso{
\code{\link{dist}} in stats.}

\examples{
m1<-cbind(c(1:10),c(1:10))
m2<-cbind(sample(20:80,4),sample(20:80,4))
d<-distance(m1,m2)
}
\keyword{distance matrix}
\keyword{accessibility}
