Get.Surface<-function(Surface.name,Matches){
  Mat<-Matches%>%filter(Surface == Surface.name)
  Serve = Mat%>%mutate(W.Serve.Win =(Winner.First.Won + Winner.Second.Won)/Winner.Serve)%>%mutate(L.Serve.Win = (Loser.First.Won + Loser.Second.Won)/Loser.Serve)
  Serve = cbind(Serve$W.Serve.Win,Serve$L.Serve.Win)
  Serve = Serve[!is.na(Serve)]
  Return = Mat%>%mutate(W.Receive.Win = (Loser.Serve - Loser.First.Won - Loser.Second.Won)/Loser.Serve)%>%mutate(L.Receive.Win = (Winner.Serve - Winner.First.Won - Winner.Second.Won)/Winner.Serve)
  Return = cbind(Return$W.Receive.Win,Return$L.Receive.Win)
  Return = Return[!is.na(Return)]
  Fit.Truncated.Normal<-function(x){
    mu = mean(x)
    sdd = sd(x)
    return(c(mean.tnorm(mu,sdd,0,1),var.tnorm(mu,sdd,0,1)))
  }
  return(list(Serve.mean = Fit.Truncated.Normal(Serve)[1], Return.mean = Fit.Truncated.Normal(Return)[1]))
}

