newdist <-
function(mat,index)
{
	ndist=0
	if (index=="euclidean")	ndist<-dist(mat,index)
	if (index=="maximum")	ndist<-dist(mat,index)
	if (index=="manhattan") 	ndist<-dist(mat,index)
	if (index=="canberra")    	ndist<-dist(mat,index)
	if (index=="minkowski")  	ndist<-dist(mat,index)
	
	if (index=="Pearson")		ndist<-as.dist(1-abs(cor(t(mat))))
	
	if (index=="bray/curtis") 	ndist<-dsvdis(mat,index)
	if (index=="chisq")       	ndist<-dsvdis(mat,index)
	if (index=="ruzicka")     	ndist<-dsvdis(mat,index)
	if (index=="roberts")     	ndist<-dsvdis(mat,index)
	
	if (index=="jaccard")     	ndist<-vegdist(mat,index)
	if (index=="sorensen")    	ndist<-dsvdis(mat,index)
	if (index=="ochiai") 	    	ndist<-dsvdis(mat,index)
	if (index=="steinhaus")   	ndist<-dsvdis(mat,index)
	
	return(ndist)
}

