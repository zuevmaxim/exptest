harris.exp.test<-function(x, R=length(x)/4)
{
DNAME <- deparse(substitute(x))
l<-0
R=round(R)
n<-length(x)
x<-sort(x)
x<-c(0,x)
D<-(n:1)*(x[2:(n+1)]-x[1:n])
t<-((sum(D[1:R])+sum(D[(n-R+1):n]))/(2*R))/((sum(D[(R+1):(n-R)]))/(n-2*R))
p.value<-2*min(pf(t,4*R,2*(n-2*R)),1-pf(t,4*R,2*(n-2*R)))
RVAL<-list(statistic=c(Q=t), p.value=p.value, method="Harris modification of Gnedenko's F-test",data.name = DNAME)
class(RVAL)<-"htest"
return(RVAL)
}