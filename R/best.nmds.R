###############################################################
#    "Non-Metric Multidimensional Scaling (nMDS)" function    #
###############################################################

"best.nmds" <- function (mat,index,k,itr=20,maxit=100)
{
  dis <- newdist(mat,index)	
  best <- 0
  strss <- rep(0,itr)
  minstr <- 99999
  for (i in 1:itr) 
  {
    dis<-newdist(mat,index)
    tmp <- isoMDS(dis,k=k,y = matrix(runif(k * attr(dis,"Size")), ncol = k), maxit = maxit)
    strss[i] <- tmp$stress
    if (tmp$stress < minstr)
    {
      minstr <- tmp$stress
      best <- i
      out <- tmp
    }
  }
  print(strss)
  print(paste("best result = ",best))
  out
}

