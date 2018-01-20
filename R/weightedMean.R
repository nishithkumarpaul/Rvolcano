weightedMean <-
function(a,b=0.2){
w<-NULL
pw<-NULL
for (i in 1:length(a))
{
#w[i]<-exp(-(a[i]-median(a))^2)
w[i]<-exp(-(b/2)*(mad(a)^-2)*(a[i]-median(a))^2)
pw[i]<-w[i]*a[i]
}
tpw<-sum(pw)
tw<-sum(w)
wm<-tpw/tw
dd<-c(tpw,tw,wm)
return(wm)
}
