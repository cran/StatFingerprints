delete.background <-
function(mat,radius)
{  
  ma<-matrix(nc=dim(mat)[2],nr=dim(mat)[1])
  for (i in 1:dim(mat)[1])
  {
    ma[i,]<-rollball(prof=mat[i,], radius)
  }
  
  dev.off()
  mm<-mat-ma
  layout(1:2,2,1)

  for (i in 1:dim(mat)[1])
  {
    plot(1:dim(mat)[2],mat[i,],col=i,xlab="Scans of the fingerprints profiles",ylab="Signal intensity",ylim=c(0,max(mat)),type="l",main=("Before deleting background"));if(i<dim(mat)[1]) par(new=TRUE) 
  }

  for (i in 1:dim(mm)[1])
  {
    plot(1:dim(mm)[2],mm[i,],col=i,xlab="Scans of the fingerprints profiles",ylab="Signal intensity",ylim=c(0,max(mm)),type="l",main=("After deleting background"));if(i<dim(mm)[1]) par(new=TRUE) 
  }

  return(mm)
}

