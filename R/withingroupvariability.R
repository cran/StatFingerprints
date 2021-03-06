withingroupvariability <-
function(mat,fact1,index,metth)
{
	dis=newdist(mat,index)
	x <- as.dist(dis)
	grouping <- as.factor(fact1)
	irow <- as.vector(as.dist(row(matrix(nrow = attributes(x)$Size, ncol = attributes(x)$Size))))
	icol <- as.vector(as.dist(col(matrix(nrow = attributes(x)$Size, ncol = attributes(x)$Size))))
	matchedd <- function(irow, icol, grouping) {grouping[irow] == grouping[icol]}
	withinn <- matchedd(irow, icol, grouping)
	cl.vec <- rep("Between", length(x))
	take <- as.numeric(irow[withinn])
	cl.vec[withinn] <- levels(grouping)[grouping[take]]
	cl.vec <- factor(cl.vec, levels = c("Between", levels(grouping)))
	m<- tapply(x, cl.vec, mean)
	sds<-tapply(x, cl.vec, sd)
	nn1<-split(mat[,1],fact1)
	nn<-1
	
	for (i in 1:length(nn1)) 
	{
		nn[i]<-length(nn1[[i]])
	}
	
	cl=which(cl.vec=="Between")
	p0=TukeyHSD(aov(x[-cl]~cl.vec[-cl]),type="II")
	p1=Anova(aov(x[-cl]~cl.vec[-cl]),type="II")
	print("DESCRIPTIVE STATISTIC")
	print("Number:")
	print(nn)
	print("Mean:")
	print(m)
	print("Standard Deviation:")
	print(sds)
	print("")
	print("RESULTS OF ANOVA")
	print(p1)
	print("")
	print("RESULTS OF HSD TUKEY POST HOC TEST") 
	print(p0)
	
	if (metth==1) boxplot(x~cl.vec,xlab=names(fact)[sel],ylab="Proximity measure",main=paste("Boxplot of the within group variability using",index,"method"))
	if (metth==3) lineplot.CI(cl.vec, x, type="b", cex = 2,xlab = names(fact)[sel], ylab = "Proximity measure", ci.fun= function(x) c(mean(x)-sd(x), mean(x)+sd(x)),main=paste("Points, Lines and SD using",index,"method"))
	if (metth==2) lineplot.CI(cl.vec, x, type="p", cex = 2,xlab = names(fact)[sel], ylab = "Proximity measure", ci.fun= function(x) c(mean(x)-sd(x), mean(x)+sd(x)),main=paste("Points and SD using",index,"method"))
	
}

