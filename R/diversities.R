############################################
#    "Compute diversity index" function    #
############################################
     
"diversities"<-function (mat,radius,int,index,lim,digit,method)
{
  if(method=="high of peaks") method<-4
  if(method=="area") method<-5
  diver = vector(length= dim(mat)[1])
  names(diver) = rownames(mat)
  for (i in 1:dim(mat)[1]) 
  { 
    prof <- mat[i, ]
    pkparam <- peakparameters(prof=prof,radius=radius,int=int,lim=lim,digit=digit)
    pkparam<-pkparam[method,]
    div<- sum(pkparam*pkparam) 
    if (index==1) diver[i] <- length(pkparam)                                     # "peak_number"
    if (index==2) diver[i] <- -1 * log(div)                                       #"-log Simpson"
    if (index==3) diver[i] <- 1-div                                               # "1-simpson"
    if (index==4) diver[i] <- -sum(pkparam*log(pkparam))                          #  "Shannon"
    if (index==5) diver[i] <- (exp(-sum(pkparam*log(pkparam))))/(length(pkparam)) #"Buzas & Gibson's evenness"
    if (index==6) diver[i] <- (-sum(pkparam*log(pkparam)))/(log(length(pkparam))) #  "Equitability"
  }
  return(diver)                               
}

