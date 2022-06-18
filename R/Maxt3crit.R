#' Find test statistic value and corresponding critical value
#'
#' More detailed description
#'
#' @param g1 a real vector
#' @param g2 a real vector
#' @param g3 a real vector
#' @param alpha real number between 0 and 1
#'
#' @return Numerical vector
#'
#' @examples
#' g1=c(0,1,2,4,2,1,3,4)
#' g2=c(1,2,4,6,1,6,0,-2,-5)
#' g3=c(1,2,4,0,1,4,6,2,6)
#' Maxt3crit(g1,g2,g3,0.05)
#'
#' @export
Maxt3crit<-function(g1,g2,g3,alpha)
{
  fun1<-function(n1,n2,n3,v1,v2,v3)
  {
    g1<-rnorm(n1,0,sqrt(v1))
    g2<-rnorm(n2,0,sqrt(v2))
    g3<-rnorm(n3,0,sqrt(v3))
    X1=mean(g1);X2=mean(g2);X3=mean(g3)
    S1=var(g1);S2=var(g2);S3=var(g3)
    v1<-sqrt(S1/n1+S2/n2);v2<-sqrt(S2/n2+S3/n3)
    T1=(X2-X1)/v1;T2=(X3-X2)/v2
    A=min(T1,T2,na.rm=FALSE)
    return(A)
  }
  fun2<-function(n1,n2,n3,alpha,v1,v2,v3)
  {
    x<-replicate(5000,fun1(n1,n2,n3,v1,v2,v3))
    y<-sort(x,decreasing=FALSE)
    m=(1-alpha)*5000
    c<-y[m]
    return(c)
  }
  fun3<-function(n1,n2,n3,alpha,v1,v2,v3)
  {
    z=replicate(10,fun2(n1,n2,n3,alpha,v1,v2,v3))
    cri=mean(z)
    return(cri)
  }
  fun4<-function(g1,g2,g3)
  {
    X1=mean(g1);X2=mean(g2);X3=mean(g3)
    n1=length(g1);n2=length(g2);n3=length(g3)
    S1=var(g1);S2=var(g2);S3=var(g3)
    v1<-sqrt(S1/n1+S2/n2);v2<-sqrt(S2/n2+S3/n3)
    T1=(X2-X1)/v1;T2=(X3-X2)/v2
    A=min(T1,T2,na.rm=FALSE)
    return(A)
  }
  set.seed(34)
  statistic_value=fun4(g1,g2,g3)
  n1=length(g1);n2=length(g2);n3=length(g3)
  v1=var(g1);v2=var(g2);v3=var(g3)
  crit_value=fun3(n1,n2,n3,alpha,v1,v2,v3)
  result<-c(statistic_value, crit_value)
  return(result)
}
