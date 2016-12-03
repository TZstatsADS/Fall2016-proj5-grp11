
mean.tnorm<-function(mu,sd,lower,upper){
  ##return the expectation of a truncated normal distribution
  lower.std=(lower-mu)/sd
  upper.std=(upper-mu)/sd
  mean=mu+sd*(dnorm(lower.std)-dnorm(upper.std))/
    (pnorm(upper.std)-pnorm(lower.std))
  return(mean)
}

var.tnorm<-function(mu,sd,lower,upper){
  ##return the variance of a truncated normal distribution
  lower.std=(lower-mu)/sd
  upper.std=(upper-mu)/sd
  variance=sd^2*(1+(lower.std*dnorm(lower.std)-upper.std*dnorm(upper.std))/
                   (pnorm(upper.std)-pnorm(lower.std))-((dnorm(lower.std)-dnorm(upper.std))/
                                                          (pnorm(upper.std)-pnorm(lower.std)))^2)
  return(variance)
}



Fit.Truncated.Normal<-function(x){
  mu = mean(x)
  sdd = sd(x)
  return(c(mean.tnorm(mu,sdd,0,1),var.tnorm(mu,sdd,0,1)))
}