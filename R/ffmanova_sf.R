ffmanova_sf <-
function(formula, data, stand = TRUE, nSim = 0, verbose = TRUE) 
{
	
	rotationtest_sf <-function (modelData, errorData, simN = 999, dfE = -1, dispsim = TRUE,nb_itt,azer)
	{
		if (simN == 0)
			dispsim <- FALSE
		dfH = dim(modelData)[1]
		if (dfE < 0)
			dfE = dim(errorData)[1]
		Y = rbind(modelData, errorData)
		q = dim(Y)[2]
		dfT = dfH + dfE
		dfT_ = dim(Y)[1]
		X = matrix(0, dfT, dfH)
		X[1:dfH, 1:dfH] = diag(dfH)
		if (dfH == 0 | dfE == 0 | q == 0) 
		{
			pAdjusted = rep(NaN, q)
			pAdjFDR = rep(NaN, q)
			return(list(pAdjusted = pAdjusted, pAdjFDR = pAdjFDR, simN = simN, azer))
		}
		ss = rep(0, q)
		sss = rep(0, q)
		normY <- sqrt(colSums(Y^2))
		for (j in 1:q) 
		{
			if (normY[j] > 0)
				Y[, j] = Y[, j]/normY[j]
			ss[j] = sum(Y[1:dfH, j]^2)
		}
		sortindex = order(ss)
		ss = ss[sortindex]
		Ys = Y[, sortindex, drop = FALSE]
		sizeX_12 = dim(X)[1] * dim(X)[2]
		sizeX_1 = dim(X)[1]
		m = rep(1, q)
		mFDR = rep(1, q)
		pAdjusted = rep(1, q)
		pAdjFDR = rep(1, q)
		if (dispsim) 
		{
			cat("\n")
			nLoop = ceiling(simN/100)
		}
		repsim = 0
		if ((dfT/dfT_) > 10)
			repsim = 10
		end
		if ((dfT/dfT_) > 100)
			repsim = 100
		end
		repindex = 0
		i = 0
		plass_p0 = (q + 1):(2 * q)
		divisor = seq(q, 1)
		while (i < simN) 
		{
			azer<-azer+1
			azer<<-azer
			waitGUI(azer,nb_itt)
			if (dispsim)
				if (i%%nLoop == 0) 
				{
					if (i%%(10 * nLoop) == 0)
						cat("")
					else cat("")
					flush.console()
				}
			i = i + 1
			if (repsim) 
			{
				if (repindex == 0)
					Xs = qr.Q(qr(matrix(rnorm(sizeX_12), nrow = sizeX_1), LAPACK = TRUE))
				Z <- crossprod(Xs[(repindex * dfT_ + 1):((repindex + 1) * dfT_), , drop = FALSE], Ys)
				repindex = (repindex + 1)%%repsim
			}
			else 
			{
				Xs = qr.Q(qr(matrix(rnorm(sizeX_12), nrow = sizeX_1), LAPACK = TRUE))
				Z = crossprod(Xs[1:dfT_, , drop = FALSE], Ys)
			}
			sss = colSums(Z * Z)
			sss_cummax = cummax(sss)
			m = m + as.numeric(sss_cummax > ss)
			o2 = order(c(ss, sss))
			plassering = (1:(2 * q))[o2 <= q]
			adj <- (plass_p0 - plassering)/divisor
			adj[adj > 1] <- 1
			mFDR <- mFDR + adj
		}
		if (dispsim)
			cat("\n")
		for (j in 1:q) 
			pAdjusted[j] = m[j]/(simN + 1)
		for (j in 2:q) 
			pAdjusted[q + 1 - j] = max(pAdjusted[(q +	1 - j):(q + 2 - j)])
		pAdjusted = pAdjusted[order(sortindex)]
		pAdjFDR = rep(1, q)
		
		for (j in 1:q) pAdjFDR[j] = mFDR[j]/(simN + 1)
		for (j in 2:q) pAdjFDR[j] = min(pAdjFDR[(j - 1):j])
		
		pAdjFDR = pAdjFDR[order(sortindex)]
		list(pAdjusted = pAdjusted, pAdjFDR = pAdjFDR, simN = simN)
	}
	
	rotationtests_sf<-function (xyObj, nSim, verbose = TRUE,azer)
	{
		nTerms = length(xyObj$xObj$df_D_test)
		nYvar = dim(xyObj$Y)[2]
		pAdjusted = matrix(1, nTerms, nYvar)
		pAdjFDR = matrix(1, nTerms, nYvar)
		simN_ = c()
		nb_itt<-c(nSim*nTerms)[1]
		for (i in 1:nTerms) 
		{
			azer<-(i*nSim[1])-c(nSim[1]-1)
			if (isTRUE(verbose) && nSim[i] > 0)
				cat(xyObj$xObj$termNames[[i]], "  -  ", nSim[i], "rotation simulations")
			if (is.list(xyObj$errorObs)) 
			{
				res <- rotationtest_sf(xyObj$hypObs[[i]], xyObj$errorObs[[1]], nSim[i], xyObj$errorObs[[2]], dispsim = verbose,nb_itt=nb_itt,azer=azer)
			}
			else 
			{
				res <- rotationtest_sf(xyObj$hypObs[[i]], xyObj$errorObs, nSim[i], dispsim = verbose,nb_itt=nb_itt,azer=azer)
			}
			pAdjusted[i, ] = res$pAdjusted
			pAdjFDR[i, ] = res$pAdjFDR
			simN_ = c(simN_, res$simN)
		}
		list(pAdjusted = pAdjusted, pAdjFDR = pAdjFDR, simN = simN_,azer)
	}
	
	
	azer<-0
	mf <- match.call(expand.dots = FALSE)
	m <- match(c("formula", "data", "subset", "na.action"), names(mf), 0)
	mf <- mf[c(1, m)]
	mf[[1]] <- as.name("model.frame")
	mf <- eval(mf, parent.frame())
	mt <- attr(mf, "terms")
	mm <- model.matrix(mt, mf)
	Y <- as.matrix(model.response(mf, "numeric"))
	if (stand)
		Y <- stdize(Y, center = FALSE, avoid.zero.divisor = TRUE)
	mOld = attr(mt, "factors")
	mNew = fixModelMatrix(mOld)
	mNew = cbind(`(Intercept)` = 0, mNew)
	model = t(mNew)
	termNr = attr(mm, "assign") + 1
	D = vector("list", max(termNr))
	for (i in seq(along = D))
		D[[i]] <- mm[, termNr == i, drop = FALSE]
	xObj <- x_Obj(D, model)
	xyObj = xy_Obj(xObj, Y)
	nTerms = length(xyObj$xObj$df_D_test)
	res1 = manova5050(xyObj, stand)
	res2 = rotationtests_sf(xyObj, rep(nSim, length.out = nTerms), verbose = verbose,azer)
	res3 = unitests(xyObj)
	structure(c(res1, res2, res3), class = "ffmanova")
}

