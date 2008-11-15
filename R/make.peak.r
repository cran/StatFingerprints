###########################################
#    Function to make peak                #
###########################################
      
"make.peak"<-function(prof)
{
  tkmessageBox(message="WARNING, care must be taken when using this function. The job is better done if you select as reference peak a peak with the same width and height as the peak you want to redefine") 
  plot(1:length(prof),prof,type="l",col="blue",xlab="Scan of your fingerprint profile",ylab="Signal intensity",main="Zoom reference peak use to redefine peak (2 click)")

#### Zoom

  l=round(locator(2,type="p",pch=4)[[1]],digit=0)
  d=prof[(l[1]+1):l[2]]
  plot(1:(l[2]-l[1]),d,type="l",col="blue",xlab=NA,ylab=NA,xaxt="n",yaxt="n",main="Select the reference peak precisely (2 click)")
  lz<-l

#### Select x gaussien

  l=locator(2,type="p",pch=4)
  ly=which.max(round(l[[2]],digit=0))
  x2=round(l[[1]][ly],digit=0)
  if (ly==2) x1=length(which((which(d<d[x2]))<x2))
    else (x1=which(d<=d[x2])[which((which(d<=d[x2]))>=x2)[2]])
  if (x1>=x2) xf=c(x2,x1) 
    else xf=c(x1,x2)

  plot(1:(lz[2]-lz[1]),d,type="l",col="blue",xlab=NA,ylab=NA,xaxt="n",yaxt="n",main="Validate the area of the reference peak (rigth click)")
  abline(v=c(xf[1],xf[2]))
  abline(h=d[xf[2]])
  locator(1)

###################
#### Select peak to rebuild
#################

  plot(1:length(prof),prof,type="l",col="blue",xlab="Scan of your fingerprint profile",ylab="Signal intensity",main="Zoom peak with defects (2 click)")

#### Zoom

  l=round(locator(2,type="p",pch=4)[[1]],digit=0)##1er loc
  dd=prof[(l[1]+1):l[2]]
  plot(1:(l[2]-l[1]),dd,type="l",col="blue",xlab=NA,ylab=NA,xaxt="n",yaxt="n",main="Select the peak with defects precisely (2 click)")
  
#### Select x gaussien

  l1=round(locator(2,type="p",pch=4)[[1]],digit=0) ##2e loc
  plot(1:(l[2]-l[1]),dd,type="l",col="blue",xlab=NA,ylab=NA,xaxt="n",yaxt="n",main="Validate the area of the peak with defects (rigth click)")
  abline(v=c(l1[1],l1[2]))
  abline(h=dd[l1[2]])
  x1=l[1]+l1[1]+1
  x2=x1+l1[2]-l1[1]-1
  prof1 = spline(d[xf[1]:xf[2]],n=(length(x1:x2)))
  
  locator(1)

  er=prof
  prof1$y<-prof1$y-min(prof1$y)+min(er[x1:x2])
  er[x1:x2]<-prof1$y
  layout(1:2,2,1)
  plot(1:length(er),er,type="l",col="red",ylim=c(min(er),max(er)),xlab=NA,ylab=NA,xaxt="n",yaxt="n")
  par(new=TRUE); plot(1:length(prof),prof,type="l",col="blue",ylim=c(min(er),max(er)),xlab=NA,ylab=NA,xaxt="n",yaxt="n",main="Result")
  plot(l[1]:l[2],er[l[1]:l[2]],type="l",col="red",ylim=c(min(er[l[1]:l[2]]),max(er[l[1]:l[2]])),xlab=NA,ylab=NA,xaxt="n",yaxt="n")
  par(new=TRUE);plot(l[1]:l[2],prof[l[1]:l[2]],type="l",col="blue",ylim=c(min(er[l[1]:l[2]]),max(er[l[1]:l[2]])),xlab=NA,ylab=NA,xaxt="n",yaxt="n")
  return(er)
}
