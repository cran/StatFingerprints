#######################################################################
#    Function to transform profiles into presence/absence profiles    #
#######################################################################

"binary"<-function(mat,radius,int,lim,digit)
{
  for (i in 1:dim(mat)[1])
  {
    prof <- mat[i, ]
    pkparam <- peakparameters(prof=prof,radius=radius,int=int,lim=lim,digit=digit)
    pkparam<-sort(c(pkparam[1,],pkparam[2,]))
    nn<-vector(length=length(prof))
    nn[]<-0
    y=1
    
    for (j in 1:c(length(pkparam)/2))
    {
      nn[c(pkparam[y]:pkparam[y+1])]<-1
      y=y+2
    }
    
    mat[i,]<-nn
  }
  
  matpl<-mat;matpl[matpl==0]<-NA
  dev.off()
  j=dim(matpl)[1]
    
  for (i in 1:dim(matpl)[1])
  {
  sds<-matpl[i,]+j
  j=j-1
  plot(1:dim(matpl)[2],sds,type="l",yaxt="n",ylab="Fingerprint profiles",xlab="Scans of the fingerprint profiles",ylim=c(0,dim(matpl)[1]));if (i<=dim(matpl)[1]+1) par(new=TRUE)
  }
    
return(mat) 
}

