co.exp.test<-function(x)
{
DNAME <- deparse(substitute(x))
n<-length(x)
y<-x/mean(x)
y<-log(y)*(1-y)
co<-sum(y)+n
v<-sqrt(6/n)*co/pi
p.value<-2*(1-pnorm(abs(v)))
RVAL<-list(statistic=c(COn=co), p.value=p.value, method="Test for exponentiality based on the statistic of Cox and Oakes",data.name = DNAME)
class(RVAL)<-"htest"
return(RVAL)
}