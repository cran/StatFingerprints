anosim_sf <-
function (dat, grouping, permutations = 1000, strata,wait)
{
	x <- as.dist(dat)
	sol <- c(call = match.call())
	grouping <- as.factor(grouping)
	
	matched <- function(irow, icol, grouping) 
	{
		grouping[irow] == grouping[icol]
	}
	
	x.rank <- rank(x)
	N <- attributes(x)$Size
	div <- length(x)/2
	irow <- as.vector(as.dist(row(matrix(nrow = N, ncol = N))))
	icol <- as.vector(as.dist(col(matrix(nrow = N, ncol = N))))
	within <- matched(irow, icol, grouping)
	aver <- tapply(x.rank, within, mean)
	statistic <- -diff(aver)/div
	cl.vec <- rep("Between", length(x))
	take <- as.numeric(irow[within])
	cl.vec[within] <- levels(grouping)[grouping[take]]
	cl.vec <- factor(cl.vec, levels = c("Between", levels(grouping)))
	if (permutations) 
	{
		perm <- rep(0, permutations)
		for (i in 1:permutations) 
		{
			if(wait==1) waitGUI(i, permutations)
				take <- permuted.index(N, strata)
			cl.perm <- grouping[take]
			tmp.within <- matched(irow, icol, cl.perm)
			tmp.ave <- tapply(x.rank, tmp.within, mean)
			perm[i] <- -diff(tmp.ave)/div
		}
		p.val <- sum(perm >= statistic)/permutations
		sol$signif <- p.val
		sol$perm <- perm
	}
	sol$permutations <- permutations
	sol$statistic <- as.numeric(statistic)
	sol$class.vec <- cl.vec
	sol$dis.rank <- x.rank
	sol$dissimilarity <- attr(dat, "method")
	if (!missing(strata)) 
	{
		sol$strata <- deparse(substitute(strata))
		sol$stratum.values <- strata
	}
	class(sol) <- "anosim"
	sol
}

