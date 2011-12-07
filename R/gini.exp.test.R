gini.exp.test<-function(x)
{
DNAME <- deparse(substitute(x))
n<-length(x)
k<-0
a<-1:(n-1)
b<-(n-1):1
a<-a*b
k<-x[2:n]-x[1:(n-1)]
g<-sum(k*a)/((n-1)*sum(x))
v<-abs(g-0.5)*(sqrt(12*(n-1)))
p.value<-2*(1-pnorm(v))
RVAL<-list(statistic=c(Gn=g), p.value=p.value, method="Test for exponentiality based on the Gini statistic",data.name = DNAME)
class(RVAL)<-"htest"
return(RVAL)
}