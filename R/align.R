#######################################
#    Fingerprint profile alignment    #
#######################################

"align"<-function(mat,roxref,nam)
{
  rox=mat$rox[nam,]
  profil=mat$profil[nam,]
  layout(matrix(c(1,1),1,1))
  plot(c(1:length(rox)),rox,type="l",col="red",main=rownames(mat$profil)[nam],sub=c("ZOOM in the internal standard of your fingerprint profile (2 left clicks)"),xlab="Scans of your internal standard",ylab="Signal intensity")
  legend("topright",col=c("red"),c("Reference standard"),lty=c(1))

#### Zoom
 
  a=locator(2,type="p",pch=4)
  
  zoom=matrix(ncol=2,nrow=2)
  zoom[1,]=a$x
  zoom[2,]=a$y
  roxx=rox[zoom[1,1]:zoom[1,2]]
  profil=profil[zoom[1,1]:zoom[1,2]]
  plot(c(1:length(roxx)),roxx,type="l",col="red",sub=c("Select region of alignment (2 left clicks)"),xlab="Scans of your internal standard",ylab="Signal intensity",main=rownames(mat$profil)[nam])
  legend("topright",col=c("red"),c("Internal standard"),lty=c(1))

#### rox peak range
 
  loc=locator(2,type="p",pch=4)
  supp=matrix(ncol=2,nrow=2)
  supp[1,]=loc$x
  supp[2,]=loc$y
  supp=round(supp)
  rox=roxx[supp[1,1]:supp[1,2]]
  rox[rox<supp[2,1]]=0
  profil=profil[supp[1,1]:supp[1,2]]
  plot(c(1:length(rox)),rox,type="l",col="red",sub=c("select peaks to supress (left click before and after each peak). Right click to stop."),xlab="Scans of your internal standard",ylab="Signal intensity",main=rownames(mat$profil)[nam])
  legend("topright",col=c("red"),c("Internal standard"),lty=c(1))

#### peak supression
  
  loc=locator(type="p",pch=4)
  
  supp=matrix(ncol=length(loc$x),nrow=2)
  supp[1,]=loc$x
  supp[2,]=loc$y
  supp=round(supp)
  for (i in 1:((length(loc$x))/2))
  {
    m=i*2
    rox[supp[1,m-1]:supp[1,m]]<-0
  }

#### Alignment process

  rle.rox=rle(rox)
  cumsum.rox=cumsum(rle.rox$lengths)
  myruns=which(rle.rox$values == 0 & rle.rox$lengths >=3)
  ends=cumsum.rox[myruns]
  newindex = ifelse(myruns > 1, myruns - 1,0)
  starts = cumsum.rox[newindex] +1
  if (0 %in% newindex) 
    starts= c(1,starts)
  rox3<-vector(length=length(starts)-1)
  rox3<-as.numeric(rox3)
  for (i in 1:(length(starts)-1))
  {
    rox3[i]<-which.max(rox[ends[i]:starts[i+1]]) + ends[i]-1
  }

  asupp=rox3[1]-25
  rox3=rox3-asupp
  profil=profil[asupp:length(profil)]
  
#### Check alignment and number of peaks

  layout(matrix(c(1,2),2,1))
  if (length(rox3)==length(roxref)) plot(c(1:length(profil)),profil,type="l",col="blue",xlab="Scans of fingerprint profile",ylab="Signal intensity",main=paste(rownames(mat$profil)[nam],"successful:",length(rox3),"peaks have been detected"))
  if (length(rox3)!=length(roxref)) plot(c(1:length(profil)),profil,type="l",col="blue",xlab="Scans of fingerprint profile",ylab="Signal intensity",main=paste(rownames(mat$profil)[nam],"error:",length(rox3),"peaks detected. Reference standard =",length(roxref),"peaks"))
  if (length(rox3)!=length(roxref)) tkmessageBox(message=paste(rownames(mat$profil)[nam],"error:",length(rox3),"peaks detected. Reference standard =",length(roxref),"peaks. Please retry"))
  if (length(rox3)!=length(roxref)) stop( paste(rownames(mat$profil)[nam],"error:",length(rox3),"peaks detected. Reference standard =",length(roxref),"peaks. Please retry"))
  abline(v=rox3,col="green",lwd=10)
  vv=rox3
  vv[vv<10000]<-max(profil)
  text(rox3,vv,pos=1,paste("P",1:length(rox3)))
  par(new=TRUE)
  plot(c(1:length(rox)),rox,type="l",col="red",xlab=NA,ylab=NA,xaxt="n",yaxt="n")

  ecos=matrix(ncol=roxref[length(roxref)]-roxref[1],nrow=2)
  for (i in 1:(length(roxref)-1))
  {
    prof=profil[rox3[i]:rox3[i+1]]             
    roxo=roxref[i+1]-roxref[i]
    prof1=spline(c(1:length(prof)),prof,n=roxo)
    prof1$x=c(roxref[i]:(-1+roxref[i+1]))
    ecos[1,-24+((roxref[i]):(-1+(roxref[i+1])))]=prof1$x
    ecos[2,-24+((roxref[i]):(-1+(roxref[i+1])))]=prof1$y
  }
  
#### Display alignment  

  plot(ecos[1,],ecos[2,],type="l",col="blue",xlab="Scans of fingerprint profile",ylab="Signal intensity",main=c("changes of the alignment"),xlim=c(1,length(ecos[2,])))
  abline(v=roxref-24,col="red")
  par(new=TRUE)
  plot(c(1:dim(ecos)[2]),profil[1:dim(ecos)[2]],lty=3,col="blue",type="l",xlab=NA,ylab=NA,xaxt="n",yaxt="n")
  abline(v=rox3,col="red",lty=3)
  
  return(ecos[2,])
}
