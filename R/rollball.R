#######################
#    Rollball tool    #
#######################


"rollball"<-function(prof,radius)
{
  som=sum(prof)
  prof=prof/som
  long=length(prof)
  offsetx=c(1:radius)
  offsety=radius-sqrt(radius^2-offsetx^2)
  offsety=offsety*(max(prof)/long)
  profs=matrix(0:0,nrow=(2*radius+1),ncol=long)
  profs[1,]=prof
  
  for (cnt in 1:radius)
  {
    profs[1+cnt,1:(long-cnt)]=prof[(cnt+1):long]+offsety[cnt]
    profs[1+radius+cnt,(cnt+1):long]=prof[1:(long-cnt)]+offsety[cnt]
  }
  prof1=apply(profs,2,min)
  offsety=-offsety
  profs=matrix(0:0,nrow=(2*radius+1),ncol=long)
  profs[1,]=prof1

  for (cnt in 1:radius)
  {
    profs[1+cnt,1:(long-cnt)]=prof1[(cnt+1):long]+offsety[cnt]
    profs[1+radius+cnt,(cnt+1):long]=prof1[1:(long-cnt)]+offsety[cnt]
  }

####    Display result 
 
  backgr=apply(profs,2,max)
  layout(matrix(c(1,2,3),3,1))
  plot(c(1:length(prof)),prof,type="h",col="red",ylim=c(0,max(prof)),sub="Fingerprint profile with the selection of the rollball(red)",ylab="Signal intensity",xlab="Scans of fingerprint profile")
  par(new=TRUE);plot(c(1:long),backgr,type="h",col="white",ylim=c(0,max(prof)),xlab=NA,ylab=NA)
  par(new=TRUE);plot(c(1:long),backgr,type="l",col="red"  ,ylim=c(0,max(prof)),xlab=NA,ylab=NA)
  par(new=TRUE);plot(c(1:long),prof  ,type="l",col="blue" ,ylim=c(0,max(prof)),xlab=NA,ylab=NA)
  abline(h=0)
  plot(c(1:long),(prof-backgr),type="l",col="blue",ylim=c(0,max(prof-backgr)),ylab="Signal intensity",xlab="Scans of fingerprint profile",sub="Fingerprint profile without background")
  abline(h=0)
  return(backgr)
}
