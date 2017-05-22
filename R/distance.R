distance <- function(m1, m2, type = "euclidean"){

  k<-nrow(m1)
  l<-nrow(m2)

  d<-matrix(data=NA, nrow=k, ncol=l)

  for (i in 1:k)
  {
    if(type == "euclidean") {d[i,]<-sqrt(((m1[i,1]-m2[,1])^2)+((m1[i,2]-m2[,2])^2))}
    else if (type == "manhattan") {d[i,]<-abs(m1[i,1]-m2[,1])+abs(m1[i,2]-m2[,2])}
  }
  return(d)
}

