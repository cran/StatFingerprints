diversities <-
function (mat,radius,int,lim,digit,method)
{
	
	if(method=="high of peaks") method<-4
	if(method=="area") method<-5
	diver=matrix(nr=dim(mat)[1],nc=7,dimnames = list(c(rownames(mat)),c("Number_of_Peaks","minus_log_Simpson","1_minus_Simpson","Shannon","Buzas_Gibson_evenness","Equitability","Background_area")))
	
	for (i in 1:dim(mat)[1]) 
	{ 
		prof <- mat[i, ]
		pkparam <- peakparameters(prof=prof,radius=radius,int=int,lim=lim,digit=digit)
		backgra<-cumsum(backgr)
		pkparam<-pkparam[method,]
		div<- sum(pkparam*pkparam) 
		diver[i,1] <- length(pkparam)                                     							#  "peak_number"
		diver[i,2] <- -1 * log(div)                                       								#  "-log Simpson"
		diver[i,3] <- 1-div                                               									#  "1-Simpson"
		diver[i,4] <- -sum(pkparam*log(pkparam))                          					#  "Shannon"
		diver[i,5] <- (exp(-sum(pkparam*log(pkparam))))/(length(pkparam)) 	#  "Buzas & Gibson's evenness"
		diver[i,6] <- (-sum(pkparam*log(pkparam)))/(log(length(pkparam))) 	#  "Equitability"
		diver[i,7] <- backgra[length(backgra)] 													#  "background area"
	}
	
	if (length(mat.binary[1,])==2)
		return(diver)                
	if (length(mat.binary[1,])>2)
		diver<-diver[,1]

	return(diver)                
	
}

