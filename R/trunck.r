trunck <-
function(mat)
{
	layout(matrix(c(1,1),1,1))  
	for (i in 1:dim(mat)[1])
	{
		plot(1:dim(mat)[2],mat[i,],col=i,type="l",ylim=c(0,max(mat)),ylab="Signal intensity",xlab="Scans of the fingerprint profiles",sub="Select the range of the fingerprint profiles (2 clicks)")
		if (i!=dim(mat)[1])
			par(new=TRUE)
	}
	
	#### Define and cut the tails
	
	l=round(locator(2,type="p",pch=4)$x)
	mat.range<-matrix(nc=(abs(l[1]-l[2])+1),nr=dim(mat)[1])
	layout(1:2,2,1)
	for (i in 1:dim(mat)[1])
	{
		plot(1:dim(mat)[2],mat[i,],col=i,type="l",ylim=c(0,max(mat)),ylab="Signal intensity",xlab="Scans of the fingerprint profiles",sub="The fingerprint profiles before having define the range")
		if (i!=dim(mat)[1])
			par(new=TRUE)
	}
	
	for (i in 1:dim(mat)[1])
	{
		mat.range[i,]<-mat[i,c((l[1]):l[2])]
		plot(1:dim(mat.range)[2],mat.range[i,],col=i,type="l",ylim=c(0,max(mat)),ylab="Signal intensity",xlab="Scans of the fingerprint profiles",sub="The fingerprint profiles after having define the range")
		if (i!=dim(mat.range)[1]) 
			par(new=TRUE)
	}
	return(mat.range)
}

