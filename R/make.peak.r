"make.peak"<-function(prof)
{
  tkmessageBox(message="WARNING, care must be taken when using this function. The job is better done if you select as reference a peak with the same width and height as the peak you want to redefine") 

#  prof<-mat6[1,]
  layout(c(1,1),1,1)
  plot(1:length(prof),prof,type="l",col="blue",xlab="Scan of your fingerprint profile",ylab="Signal intensity",main="Zoom reference peak use to redefine peak (2 clicks)")
  l_ref=locator(2,type="p",pch=4)

  peak_ref=matrix(ncol=2,nrow=2)
  peak_ref[1,1]=floor(l_ref$x[1])
  peak_ref[1,2]=ceiling(l_ref$x[2])

  prof1=prof[peak_ref[1,1]:peak_ref[1,2]]
  plot(1:length(prof1),prof1,type="l",col="blue",xlab=NA,ylab=NA,xaxt="n",yaxt="n",main="Select the reference peak precisely (2 clicks)")

  l_ref_2=locator(2,type="p",pch=4)
  zoom_peak_ref<-c(ceiling(l_ref_2$x[1]),floor(l_ref_2$x[2]))

  plot(1:length(prof1),prof1,type="l",col="blue",xlab=NA,ylab=NA,xaxt="n",yaxt="n")
  points(zoom_peak_ref[1]+1,prof1[zoom_peak_ref[1]+1],pch=19,col="red")
  points(zoom_peak_ref[2]-1,prof1[zoom_peak_ref[2]-1],pch=19,col="red")

  prof1[1:zoom_peak_ref[1]]<-NA
  prof1[zoom_peak_ref[2]:length(prof1)]<-NA
  par(new=TRUE)
  lines(prof1,col="red",lwd="3")

  tkmessageBox(message="Next step, select the peak to correct")

  plot(1:length(prof),prof,type="l",col="blue",xlab="Scan of your fingerprint profile",ylab="Signal intensity",main="Zoom peak to correct (2 clicks)")
  l=locator(2,type="p",pch=4)

  peak=matrix(ncol=2,nrow=2)
  peak[1,1]=floor(l$x[1])
  peak[1,2]=ceiling(l$x[2])

  prof2=prof[peak[1,1]:peak[1,2]]
  plot(1:length(prof2),prof2,type="l",col="blue",xlab=NA,ylab=NA,xaxt="n",yaxt="n",main="Select precisely the peak to correct (2 clicks)")

  l_2=locator(2,type="p",pch=4)
  zoom_peak<-c(ceiling(l_2$x[1]),floor(l_2$x[2]))

  plot(1:length(prof2),prof2,type="l",col="blue",xlab=NA,ylab=NA,xaxt="n",yaxt="n")
  points(zoom_peak[1]+1,prof2[zoom_peak[1]+1],pch=19,col="red")
  points(zoom_peak[2]-1,prof2[zoom_peak[2]-1],pch=19,col="red")

  prof2[1:zoom_peak[1]]<-NA
  prof2[zoom_peak[2]:length(prof2)]<-NA

  par(new=TRUE)
  lines(prof2,col="red",lwd="3")

  tkmessageBox(message="Resulting profil")

  peak_1<-prof[as.numeric(peak_ref[1,1]+zoom_peak_ref[1]):as.numeric(peak_ref[1,2]-(length(prof1)-zoom_peak_ref[2]+1))]
  peak_2<-prof[as.numeric(peak[1,1]+zoom_peak[1]):as.numeric(peak[1,2]-(length(prof2)-zoom_peak[2]+1))]

####  SPLINE

  peak_corr<-spline(peak_1,n=length(peak_2))

  min_peak_corr<-min(peak_corr[[2]])
  peak_corr_2<-peak_corr[[2]]-min_peak_corr
  peak_2_corr<-peak_2+peak_corr_2
  prof_corr<-prof
  prof_corr[as.numeric(peak[1,1]+zoom_peak[1]):as.numeric(peak[1,2]-(length(prof2)-zoom_peak[2]+1))]<-peak_2_corr

  plot(1:length(prof_corr),prof_corr,type="l",col="red",xlab="Scan of your fingerprint profile",ylab="Signal intensity",main="Resulting profile")
  peak_2_corr<-peak_2_corr+zoom_peak_ref[1]
  
  prof3<-c(rep(NA,(as.numeric(peak[1,1]+zoom_peak[1]))-1),peak_2_corr)
  lines(prof,col="blue")

  return(prof_corr)
}
