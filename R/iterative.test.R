#########################################################################
#    "Iterative tests (t test/Mann-Whitney/Fisher's exact)" function    #
#########################################################################

"iterative.test"<-function(profil,fact1,level,method)
{

  n1<-which(fact1==level[1])
  n2<-which(fact1==level[2])
  profn1=profil[n1,]
  profn2=profil[n2,]
  proan<-profil[c(n1,n2),]
  factt<-vector(length=length(fact1))
  factt[]<-NA
  factt[n1]<-"a"
  factt[n2]<-"b"
  factt<-na.omit(factt)
  factt<-factor(factt)
  z=vector(length=dim(profil)[2])
  z[]<-NA
  
  wil<-function(profil,profn1,profn2)
  {
    for (i in 1:dim(profil)[2]) 
    {
      a=wilcox.test(profn1[,i],profn2[,i])
      if (a[3]<0.05) 
        z[i]=0 
    }
    return(z)
  }

  an1<-function(profil,profn1,profn2)
  {
    for (i in 1:dim(profil)[2]) 
    {
      a=t.test(profn1[,i],profn2[,i])
      if (a[3]<0.05) 
        z[i]=0 
    }
    return(z)
  }

  iterative.fisher<-function(bin,fact,p)
  {
    m=matrix(nc=2,nr=2)
    colnames(m)=c("present","absent")
    rownames(m)=c(p[1],p[2])
    level1=which(fact==p[1])
    level2=which(fact==p[2])
    final=matrix(nc=dim(bin)[2],nr=3);rownames(final)=c(p[1],p[2],"significance")
    for (i in 1:dim(bin)[2])
    {
      m=matrix(nc=2,nr=2);colnames(m)=c("present","absent");rownames(m)=c(p[1],p[2])
      m[1,1]=sum(bin[level1,i])
      m[1,2]=length(bin[level1,i])- sum(bin[level1,i])
      m[2,1]=sum(bin[level2,i])
      m[2,2]=length(bin[level2,i])- sum(bin[level2,i])
      final[1,i]=(100*m[1,1])/length(bin[level1,i])
      final[2,i]=(100*m[2,1])/length(bin[level2,i])
      temp=fisher.test(m)[[1]]
      if (temp<0.05) final[3,i]=1 else (final[3,i]=0)
    }

  return(final)
  }
  
  aa1<-function(n11,n22,z)
  {
    plot(1:length(n11),n11,type="l",main=ma,ylab="Signal intensity",xlab="Scans of fingerprint profiles",col="red",ylim=c(0,max(rbind(n11,n22))))
    par(new=TRUE)
    plot(1:length(n22),n22,type="l",col="blue",ylab=NA,xlab=NA,ylim=c(0,max(rbind(n11,n22))))
    par(new=TRUE)
    plot(1:length(z),z,type="l",col="black",ylab=NA,xlab=NA,ylim=c(0,max(rbind(n11,n22))))
    legend("topright",c(paste("level:",level[1]),paste("level:",level[2]),"differences p<0.05"),lty=c(1,1,1),col=c("red","blue","black"))
  }

  if (method==2) z=wil(profil,profn1,profn2)
  if (method==1) z=an1(profil,profn1,profn2)
  if (method==3) z=iterative.fisher(bin=proan,fact=factt,p=c("a","b"))
  if (method==1) ma<-paste("t-test (Percent of differences", round(c((length(which(z==0))*100)/length(z)),digit=0),"%)")
  if (method==2) ma<-paste("Mann Whitney (Percent of differences", round(c((length(which(z==0))*100)/length(z)),digit=0),"%)")
  if (method==3) ma<-paste("Fisher's exact test (Percent of differences", round(c((length(which(z[3,]==1))*100)/dim(z)[2]),digit=0),"%)")
  n11=vector(length=length(n1))
  n11<-apply(profil[n1,],2,mean)
  n22=vector(length=length(n2))
  n22<-apply(profil[n2,],2,mean)
  if (method==2) aa1(n11,n22,z)
  if (method==1) aa1(n11,n22,z)
  if (method==3) plot(1:dim(profil)[2],z[3,],type="h",xlab="Scans of profiles",ylab=NA,yaxt="n",sub="Significant differences in black vertical boxes")
  print(ma)
  z<<-z

}