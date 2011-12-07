ep.exp.test<-function(x)
{
DNAME <- deparse(substitute(x))
n<-length(x)
y<-x/mean(x)
ep<-sqrt(48*n)*sum(exp(-y)-1/2)/n
p.value<-2*(1-pnorm(abs(ep)))
RVAL<-list(statistic=c(EPn=ep), p.value=p.value, method="The test for exponentiality of Epps and Pulley",data.name = DNAME)
class(RVAL)<-"htest"
return(RVAL)
}