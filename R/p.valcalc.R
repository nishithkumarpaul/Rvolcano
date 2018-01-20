p.valcalc <-
function(x,y){
t.stat<-(weightedMean(x)-weightedMean(y))/sqrt(((length(x)-1)*weightedVar(x)+(length(y)-1)*weightedVar(y))*(1/(length(x)+length(y)-2))*((1/length(x))+1/length(y)))
pval<-2*pt(abs(t.stat),(length(x)+length(y)-2),lower.tail=FALSE)
return(pval)
}
