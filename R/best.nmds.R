best.nmds <-
function (mat,index,k,itr,maxit=100)
{
	dis <- newdist(mat,index)	
	best <- 0
	strss <- rep(0,itr)
	minstr <- 99999
	
	####  Compute nMDS  ####
	
	for (i in 1:itr) 
	{
		waitGUI(i,itr)
		dis<-newdist(mat,index)
		tmp <- isoMDS(dis,k=k,y = matrix(runif(k * attr(dis,"Size")), ncol = k), maxit = maxit, trace=F)
		strss[i] <- tmp$stress
		if (tmp$stress < minstr)
		{
			minstr <- tmp$stress
			best <- i
			out <- tmp
		}
	}
	
	####  Print best stress (minimum)  ####
	
	print(strss)
	print(paste("best result = ",best))
	out
}

