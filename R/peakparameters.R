#################################
#    Peak detection function    #
#################################

"peakparameters"<-function(prof,radius,int,lim,digit)
{
  prof = prof/sum(prof)
  backgr = rollball(prof, radius)
  y = prof - backgr

####    get the maximum abscissa for each int

  nbint=trunc(length(y)/int)
  matmax<-matrix(nc=nbint,nr=4)
  rownames(matmax)=c("begin","end","xmax","ymax")
  for (i in 1:nbint)	{matmax[1,i]<-((i*int)-(int-1))}
  for (i in 1:nbint)	{matmax[2,i]<-(i*int)}
  for (i in 1:nbint)	{matmax[3,i]<-which.max(y[c(matmax[1,i]:matmax[2,i])])}
  for (i in 1:nbint)	{matmax[3,i]<-matmax[3,i]+(i*int)-(int-1)}
  for (i in 1:nbint)	{matmax[4,i]<-max(y[c(matmax[1,i]:matmax[2,i])])}

####    neighbours detection

  voisin1<-rep(0,dim(matmax)[2])
  
  for (i in 2:((dim(matmax)[2])-1))
  {
    if (matmax[4,i] > matmax[4,c(i-1)])
      voisin1[i]<-1
  }
  voisin2<-rep(0,dim(matmax)[2])
  
  for (i in 2:((dim(matmax)[2])-1))
  {
    if (matmax[4,i] > matmax[4,c(i+1)])
      voisin2[i]<-1
  }
  voisin<-rep(0,dim(matmax)[2])
  
  for (i in 1:length(voisin1))
  {
    voisin[i]<-sum(voisin1[i]+voisin2[i])
  }
  voisin<-which(voisin==2)
  
  matmaxfin=matrix(nc=length(voisin),nr=2)
  rownames(matmaxfin)=c("xmax","ymax")
  matmaxfin[1,]=matmax[3,voisin]
  matmaxfin[2,]=matmax[4,voisin]

####    normalise ymax to 1 and delete those below lim=0.005

  norm=matmaxfin[2,]/sum(matmaxfin[2,])
  seil<-which(norm>(lim/1000))
  matmaxfin1=matrix(nc=length(seil),nr=2)
  rownames(matmaxfin1)=c("xmax","ymax")
  matmaxfin1<-matmaxfin[,seil]

####    xmin and xmax detection

  rr=round(y,digit=digit)
  rr[1]=0
  rr[length(rr)]=0
  matmaxfin2=matrix(nc=dim(matmaxfin1)[2],nr=5)
  rownames(matmaxfin2)=c("xmin","xmax","x","y","area")
  matmaxfin2[3,]<-matmaxfin1[1,]
  matmaxfin2[4,]<-y[matmaxfin1[1,]]
  for (i in 1:dim(matmaxfin1)[2])
  {
    x=matmaxfin1[1,i]
    j=1
    while (rr[x-j]!=0) 
      j=j+1
    matmaxfin2[1,i]<-x-j+1
    j=1
    while (rr[x+j]!=0) 
      j=j+1
    matmaxfin2[2,i]<-x+j-1
  }

####    area

  for (i in 1:dim(matmaxfin2)[2])
  {
    matmaxfin2[5,i]<-sum(y[c(matmaxfin2[1,i]:matmaxfin2[2,i])])
  }

####    resulting plot 

  temp=matrix(nr=dim(matmaxfin2)[2],nc=10000)
  temp[]=0
  for (i in 1:dim(matmaxfin2)[2])
  {
    temp[i,c(1:length(matmaxfin2[1,i]:matmaxfin2[2,i]))]<-matmaxfin2[1,i]:matmaxfin2[2,i]
  }
  temp<-sort(c(temp)[which(c(temp)!=0)])

  plot(temp,prof[temp],type="h",col="red",xlim=c(1,length(prof)),xlab="Scans of the fingerprint profile",ylab="Signal Intensity",ylim=c(0,max(prof)))
  par(new=TRUE);plot(1:length(prof),prof,col="blue",type="l",xlab=NA,ylab=NA,ylim=c(0,max(prof)))
  abline(v=matmaxfin2[3,],col="black")
  return(matmaxfin2)
}