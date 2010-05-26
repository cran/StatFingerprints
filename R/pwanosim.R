pwanosim <-
function(mat,param,level,index,permutations)
{
	levels.pwanosim<-function(mat,param,level,index,permutations)
	{  
		l1<-which(param==level[1])
		l2<-which(param==level[2])
		l<-sort(c(l1,l2))
		matn<-mat[l,]
		para<-param[l]  
		res=anosim_sf(dat=newdist(matn,index), grouping=para, permutations,wait=1)
		print(res)
	}
	
	all.pwanosim<-function(mat,param,index,permutations)
	{
		lev=levels(param)
		finar=matrix(ncol=length(lev)-1,nrow=length(lev)-1)
		rownames(finar)=lev[2:length(lev)]
		colnames(finar)=lev[1:length(lev)-1]
		finap<-finar
		for (i in 2:(length(lev)))
		{
			for (j in 1:(length(lev)-1))
			{
				vec1=vector(length=param)
				for (k in 1:length(param))
				{
					if (param[k]==lev[i]) 
						vec1[k]<-k
					else
						(vec1[k]<-NA)
				}
				vec1=na.omit(vec1)
				vec2=vector(length=param)
				for (k in 1:length(param))
				{
					if (param[k]==lev[j])
						vec2[k]<-k 
					else
						(vec2[k]<-NA)
				}
				vec2=na.omit(vec2)
				matric=mat[c(vec1,vec2),]
				anovaa=vector(length=length(vec1)+length(vec2))
				anovaa[]="b"
				anovaa[1:length(vec1)]<-"a"
				anovaa=factor(anovaa)
				an<-anosim_sf(newdist(matric, index),anovaa,permutations,wait=2)
				finar[i-1,j]<-round(an$statistic,digits=3)
				finap[i-1,j]<-an$signif
				l=l+1
				waitGUI(i=l,itr=c(dim(finar)[1]*dim(finar)[2]))
			}
		}
		for (i in 1:length(lev)-1)
		{
			for (j in 1:length(lev)-1)
			{
				if (i<j) finar[i,j]<-NA
				if (i<j) finap[i,j]<-NA
			}
		}
		finalee<- list(finap,finar)
		names(finalee)<-c("Pairwise Significance p","Pairwise R-statistic")
		print(finalee)
	}
	print(c(level))
	l<-0
	if (level[1]!="pw45pw") levels.pwanosim(mat,param,level,index,permutations)    
	if (level[1]=="pw45pw") all.pwanosim(mat,param,index,permutations)     
}

