weightedVar <-
function(a,b=0.25){
  w<-NULL
  pw<-NULL
  pw2<-NULL
    for (i in 1:length(a))
      {
       w[i]<-exp(-(b/2)*(mad(a)^-2)*(a[i]-median(a))^2)
       pw[i]<-w[i]*a[i]
      }
    tpw<-sum(pw)
    tw<-sum(w)
    wm<-tpw/tw
    for (i in 1:length(a))
      {
       w[i]<-exp(-(b/2)*(mad(a)^-2)*(a[i]-wm)^2)
       pw2[i]<-w[i]*(a[i]-wm)^2
      }
    tw<-sum(w)
    tpw2<-sum(pw2)
    wtSig<- tpw2/tw
    return(wtSig)
}
