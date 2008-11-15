#####################################################
#    "Define the range of the profiles" function    #
#####################################################

"trunck"<-function(mat)
{
  for (i in 1:dim(mat)[1])
  {
    plot(1:dim(mat)[2],mat[i,],col=i,type="l",ylim=c(0,max(mat)),ylab="Signal intensity",xlab="Scans of the fingerprint profiles",sub="Select the range of the fingerprint profiles (2 clicks)")
    if (i!=dim(mat)[1])
      par(new=TRUE)
  }
  
#### Define and cut the tails
  
  l=round(locator(2,type="p",pch=4)$x)
  mat5<-matrix(nc=(abs(l[1]-l[2])+1),nr=dim(mat)[1])
  layout(1:2,2,1)
    for (i in 1:dim(mat)[1])
  {
    plot(1:dim(mat)[2],mat[i,],col=i,type="l",ylim=c(0,max(mat)),ylab="Signal intensity",xlab="Scans of the fingerprint profiles",sub="The fingerprint profiles before having define the range")
    if (i!=dim(mat)[1])
      par(new=TRUE)
  }
 
  for (i in 1:dim(mat)[1])
  {
    mat5[i,]<-mat[i,c((l[1]):l[2])]
    plot(1:dim(mat5)[2],mat5[i,],col=i,type="l",ylim=c(0,max(mat)),ylab="Signal intensity",xlab="Scans of the fingerprint profiles",sub="The fingerprint profiles after having define the range")
    if (i!=dim(mat5)[1]) 
      par(new=TRUE)
  }
  return(mat5)
}
