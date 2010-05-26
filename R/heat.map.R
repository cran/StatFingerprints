heat.map <-
function(b,index,algo,myp)
{
	heatmap(b, Colv=NA,distfun=function(b1) as.dist(newdist(b1,index)),hclustfun=function (b1) hclust(b1, method=algo),labCol=c(""), cexRow=1,cexCol=0.1,col=myp,main=paste('METHOD =',index,' ALGORITHM =',algo))
}

