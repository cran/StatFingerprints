best.ordination <-
function(mat, return.ordination,text=TRUE)
{  
	index<-"euclidean"
	
	all.ordination <- function (x,index,text=TRUE)
	{
		dist1=newdist(x,index)
		re=max(dist1)
		dist_ini=dist1/re
		pca1=prcomp(x,scaled=TRUE,centered=TRUE)
		a=pca1$x[,1]
		b=pca1$x[,2]
		layout(1:2,2,1)
		plot(a,b,main="Principal Components Analysis (scaled and centered)",ylab=NA,xlab=NA,xaxt="n",yaxt="n")
		if(text==TRUE)
			(text(a,b,rownames(x),pos=2))
		mds1=isoMDS(newdist(x,index),k=2); c=mds1$points[,1];d=mds1$points[,2]
		plot(c,d,main="non-metric MultiDimensional Scaling",ylab=NA,xlab=NA,xaxt="n",yaxt="n")
		if(text==TRUE)
			(text(c,d,rownames(x),pos=2))
		x11()
	}
	
	if(return.ordination==TRUE)    
		all.ordination(x=mat,index=index,text) 
	else
		dist1=0
	dist1=newdist(mat,index)
	re=max(dist1) 
	dist_ini=dist1/re
	
	pca1=prcomp(mat,centered=TRUE,scaled=TRUE)
	x=pca1$x[,1]
	y=pca1$x[,2]
	x1=matrix(1,length(x),2)
	x1[,1]=x
	dist_fin_x=newdist(x1,index="euclidean")
	y1=matrix(1,length(x),2)
	y1[,1]=y
	dist_fin_y=newdist(y1,index="euclidean")
	dist_fin1=dist_fin_y+dist_fin_x
	dist_fin=sqrt(dist_fin1)
	pca=dist_fin
	pca=pca/(max(pca))
	
	mds1=isoMDS(newdist(mat,index),k=2)
	x=mds1$points[,1]
	y=mds1$points[,2]
	x1=matrix(1,length(x),2)
	x1[,1]=x
	dist_fin_x=newdist(x1,index="euclidean")
	y1=matrix(1,length(x),2)
	y1[,1]=y
	dist_fin_y=newdist(y1,index="euclidean")
	dist_fin1=dist_fin_y+dist_fin_x
	dist_fin=sqrt(dist_fin1)
	mds=dist_fin
	mds=mds/(max(mds))
	
	plot(c(0,1),c(0,1),type="l",lty=2,lwd=1,xlab="Computed euclidean distances by the 2D ordination",ylab="Initial euclidean distances")
	par(new=TRUE)
	plot(mds,dist_ini,col=1,pch=1,cex=0.2,xlab="Computed euclidean distances by the 2D ordination",ylab="Initial euclidean distances",xaxt="n",yaxt="n",xlim=c(0,1),ylim=c(0,1))
	lm=glm(dist_ini~mds)
	reg.line(lm,col=1,lwd=1)
	lm1=glm(dist_ini~pca)
	reg.line(lm1,col=2,lwd=1)
	
	coeff.deter=function(x)
	{ 
		a=1-(x$deviance/x$null.deviance)
		return(a)
	}
	
	r1=round(coeff.deter(lm),digit=2)
	r2=round(coeff.deter(lm1),digit=2)
	par(new=TRUE)
	if (r2>=r1)
		plot(pca,dist_ini,pch="+",cex=0.3,main=paste("PCA R2=",r2,"> nMDS R2=",r1),xlab="Computed euclidean distances by the 2D ordination",ylab="Initial euclidean distances",col=2,xaxt="n",yaxt="n",xlim=c(0,1),ylim=c(0,1)) 
	if (r1>r2)
		plot(pca,dist_ini,pch="+",cex=0.3,main=paste("nMDS R2=",r1,"> PCA R2=",r2),xlab="Computed euclidean distances by the 2D ordination",ylab="Initial euclidean distances",col=2,xaxt="n",yaxt="n",xlim=c(0,1),ylim=c(0,1)) 
	legend("bottomright",col=c("black",1,2),lwd=c(1,1,1),lty=c(2,1,1),c("Identity function line","nMDS regression line","PCA regression line"))
}

