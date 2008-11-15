##############################################################
#    "SIMilarity PERcentages procedure (SIMPER)" function    #
##############################################################

"simper" <- function (a,fact,level,seuil)
{
  group1=which(fact==level[1])
  group2=which(fact==level[2])
  long11=group1
  long21=group2
  long2=length(group2)
  long1=length(group1)
  long=long1*long2

#### Total dissimilarity process with pairwise

  f=0
  t=dim(a)
  t2=t[2]
  mat=matrix(ncol=t2,nrow=long)
  for (s1 in 1:long1)
  {
    s11=long11[s1]
    for (s2 in 1:long2)
    {
      s21=long21[s2]
      f=f+1
      for (i in 1:t2)
      {
        mat[f,i]=a[s11,i]-a[s21,i]
      }
    }
  }

#### Dissimilarity process by variables

  mat=sqrt(mat*mat)
  som=sum(mat)
  fin=vector(length=t2)
  fin=apply(mat,2,sum)
  fin2=vector(length=t2)
  for (i in 1:t2)
    fin2[i]=((fin[i]*100)/som)

#### Add scan number

  long=length(fin2)
  contrib=matrix(ncol=3,nrow=long)
  colnames(contrib)=c("Scan number","Relative contribution","Cumulated contribution")
  contrib[,2]=sort(fin2,decreasing=TRUE)
  contrib[,1]=order(fin2,decreasing=TRUE)
  contrib[1,3]=contrib[1,2]
  for (i in 2:long)
    contrib[i,3]=contrib[i-1,3]+contrib[i,2]

#### Display result

  n=seuil
  fin3=fin2
  fin3[fin2<n]=0
  plot(fin3,type="h",col="red",ylim=c(0,max(contrib[,2])),ylab="Contribution to Euclidean distance (percent)",xlab="Scans of the fingerprint profiles")
  par(new=TRUE)
  plot(fin2,type="l",ylim=c(0,max(contrib[,2])),xlab=NA,ylab=NA,main=("Red indicates contribution under the threshold"))
  return(contrib[,c(1:3)])
}