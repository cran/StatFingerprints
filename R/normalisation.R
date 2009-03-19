################################
#    Normalisation function    #
################################

"normalisation" <- function (mat,type)
{
  mat6<-mat
  mat6[]=NA
  layout(1:2,2,1)
  
  for (i in 1:dim(mat)[1])
  {
    plot(1:dim(mat)[2],mat[i,],col=i,type="l",ylim=c(0,max(mat)),ylab="Signal intensity",xlab="Scans of the fingerprint profiles",sub="Fingerprint profiles before normalisation")
    if (i!=dim(mat)[1])
      par(new=TRUE)
  }
  
  if (type==1) for(j in 1:dim(mat)[1]) {mat6[j,]<-(mat[j,]/sum(mat[j,]))}
  if (type==3) m=apply(mat,1,min)
  if (type==3) for(j in 1:dim(mat)[1]) {mat[j,]<-(mat[j,]-m[j])}
  if (type==3) for(j in 1:dim(mat)[1]) {mat6[j,]<-(mat[j,]/sum(mat[j,]))}
  if (type==2) mat6<-mat
  if (type==2) mat[mat<=0]<-0
  if (type==2) for(j in 1:dim(mat)[1]) {mat6[j,]<-(mat[j,]/sum(mat[j,]))}
  
  for (i in 1:dim(mat6)[1])
  {
    plot(1:dim(mat6)[2],mat6[i,],col=i,type="l",ylim=c(0,max(mat6)),ylab="Signal intensity",xlab="Scans of the fingerprint profiles",sub="Fingerprint profiles after normalisation")
    if (i!=dim(mat6)[1])
      par(new=TRUE)
  }
  
  return(mat6)
}
