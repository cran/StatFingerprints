baseline <-
function(mat)
{
	mat=na.omit(mat)
	basetemp=mat
	basetemp[]<-0
	
	####  Display all profiles  ####
	
	layout(matrix(c(1,1),1,1))
	for (i in 1:dim(mat)[1])
	{
		plot(c(1:dim(mat)[2]),mat[i,],ylim=c(0,max(mat)),col=i,type="l",ylab="Signal intensity",xlab="Scans of fingerprint profiles",sub="Left click before and after the beginning of the profiles")
		par(new=TRUE)
	}
	
	####  Select area to parse  ####
	
	loc=locator(2,type="p",pch=4)
	x=vector(length=4)
	x[1]<-1
	x[4]<-dim(mat)[2]
	xx=loc$x
	xx=round(xx)
	x[2]<-xx[1]
	x[3]<-xx[2]
	
	####  Compute baseline  ####
	
	for (i in 1:dim(mat)[1])
	{
		vec=mat[i,]
		b=c(vec[x[1]:x[2]],vec[x[3]:x[4]])
		Ar1=c((x[1]:x[2])/1000)
		Ar2=c((x[3]:x[4])/1000)
		Ar=c(Ar1,Ar2)
		one=Ar
		one[]=1
		A=matrix(ncol=3,nrow=length(one))
		A[,1]=one
		A[,2]=Ar
		A[,3]=Ar*Ar
		cff=qr.solve(A,b)
		xx=1:length(vec)
		xs=c(1:length(vec))
		xr=xs/1000
		one=xr
		one[]=1
		bass=matrix(ncol=3,nrow=length(one))
		bass[,1]=one*cff[1]
		bass[,2]=xr*cff[2]
		bass[,3]=(xr*xr)*cff[3]
		base<-apply(bass,1,sum)
		ys=vec[xs]-base
		basetemp[i,]=ys
	}
	
	####  Display all profiles before and after baseline transformation  ####
	
	layout(matrix(c(1,2),2,1))
	
	for (i in 1:dim(mat)[1])
	{
		plot(c(1:dim(mat)[2]),mat[i,],col=i,type="l",ylim=c(0,max(mat)),ylab="Signal intensity",xlab="Scans of fingerprint profiles",sub="Fingerprint profiles before common baseline processing")
		par(new=TRUE)
	}
	
	plot(c(1:dim(mat)[2]),mat[dim(mat)[1],],col=dim(mat)[1],type="l",ylim=c(0,max(mat)),ylab="Signal intensity",xlab="Scans of fingerprint profiles")
	abline(h=0,col="red")
	
	for (i in 1:dim(basetemp)[1])
	{
		plot(c(1:dim(basetemp)[2]),basetemp[i,],type="l",col=i,ylim=c(0,max(basetemp)),ylab="Signal intensity",xlab="Scans of fingerprint profiles",sub="Result of baseline processing")
		par(new=TRUE)
	}
	
	plot(c(1:dim(basetemp)[2]),basetemp[dim(basetemp)[1],],type="l",col=dim(basetemp)[1],ylim=c(0,max(basetemp)),ylab="Signal intensity",xlab="Scans of fingerprint profiles")
	abline(h=0,col="red")
	
	####  return  resulting matrix  ####
	
	return(basetemp)

}

