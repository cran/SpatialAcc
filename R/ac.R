ac <- function(p, n, D, d0, power=2, family="SAM") {

  if (family == "SAM")
    {
      ac.matrix <- t(n/t(p*(D^power)))
      ac.measure <- apply(ac.matrix, 1, sum)
    }  else if (family == "2SFCA")
    {
      D[D>d0]<-0
      D[D>0]<-1
      step1 <- p*D
      Rj <- n / apply(step1, 2, sum)
      step2<-t(Rj*t(D))
      step2[step2=='NaN']<-0
      ac.measure<-apply(step2,1,sum)
    }
  else if (family == "KD2SFCA")
  {
    D[D>d0]<-0
    D0<-D
    D0[D>0]<-1
    #k<-colSums(D != 0)
    #step1 <- p*exp(t(t(-D^power)/k))
    step1 <- (p*exp(-D^power))
    step1<-step1*D0
    Rj <- n / apply(step1, 2, sum)
    step2<-t(Rj*t(exp(-D^power)))
    step2<-step2*D0
    step2[step2=='NaN']<-0
    ac.measure<-apply(step2,1,sum)
    }

    else if (family == "Hansen")
      {
        ac.matrix <- n*exp(-power*D)
        ac.measure <- apply(ac.matrix, 1, sum)
    }else if (family == "log")
    {
      ac.matrix <- log(n*exp(-power*D))/power
      ac.measure <- apply(ac.matrix, 1, sum)
    }
     else{stop("wrong family")}

return(ac.measure)
}
