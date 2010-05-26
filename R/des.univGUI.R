des.univGUI <-
function()
{
	
	checkparam()
	
	if(sum(param[1,],na.rm=TRUE)!=length(param[1,]) & sum(div[1,],na.rm=TRUE)!=length(div[1,]))
	{
		param2<-matrix(nr=dim(mat.analyse)[1],nc=dim(param)[2]+7)
		param2<-cbind(div,param)
	}
	
	if(sum(param[1,],na.rm=TRUE)!=length(param[1,]) & sum(div[1,],na.rm=TRUE)==length(div[1,]))
	{
		param2<-matrix(nr=dim(mat.analyse)[1],nc=dim(param)[2])
		param2<-param
	}
	
	if(sum(param[1,],na.rm=TRUE)==length(param[1,]) & sum(div[1,],na.rm=TRUE)!=length(div[1,]))
	{
		param2<-matrix(nr=dim(mat.analyse)[1],nc=dim(div)[2])
		param2<-div
	}
	
	tt <- tktoplevel()
	tkwm.title(tt,"Mean, Standard deviation, Distribution")
	tkgrid(tklabel(tt, text = "                                                                                                                                              "))
	t1<-tkframe(tt)
	text1<-tklabel(t1, text = "Quantitative variable")
	repe <- c(colnames(param2))
	repee <- tkwidget(t1, "ComboBox", editable = FALSE, values = repe)
	tkpack(text1,repee,side="left")
	tkgrid(t1)
	t2<-tkframe(tt)
	text2<-tklabel(t2, text = "Qualitative variable")
	repe1 <- c("none",names(fact))
	repee1 <- tkwidget(t2, "ComboBox", editable = FALSE, values = repe1)
	tkpack(text2,repee1,side="left")
	tkgrid(t2)
	t3<-tkframe(tt) 
	text3<-tklabel(t3, text = "Plot representation")
	indexx<-c("Boxplot","Points and SD","Points, Lines and SD")
	indexx1<-tkwidget(t3,"ComboBox",editable=FALSE,values=indexx,width=30,height=3)
	tkpack(text3,indexx1,side="left")
	tkgrid(t3)
	
	compute.descriptive.stat<-function()
	{
		para <- unlist(as.numeric(tcl(repee, "getvalue"))+1)
		fa <- unlist(as.numeric(tcl(repee1, "getvalue")))
		graph <- unlist(as.numeric(tcl(indexx1, "getvalue"))+1)
		pa=0
		
		pa<-param2[,para]
		pa.na<-na.omit(pa)
		a<-attributes(pa.na)
		b<-rownames(param2[a$na.action,])
		b<<-b
		if (length(b!=0))
		{
			fact<-fact[-a$na.action,]
			tkmessageBox(message=paste("!! missing value in ",b))
		}
		
		if (fa==0) print(paste("Mean", mean(pa.na)))
		if (fa==0) print(paste("Standard deviation", sd(pa.na)))
		if (fa==0 & para!=0)  boxplot(pa.na,main="Boxplot",ylab=colnames(param2)[para])
		if (fa==0 & para==0) boxplot(pa.na,main="Boxplot",ylab="")
		
		if (fa!=0) s<-split(pa.na,fact[,fa])
		if (fa!=0) ss<-matrix(nc=length(s),nr=3)
		if (fa!=0) colnames(ss)<-names(s)
		if (fa!=0) rownames(ss)<-c("Number","Mean","Standard Deviation")
		if (fa!=0)  for (i in 1:length(s)) {ss[1,i]<-length(na.omit(s[[i]]))}
		if (fa!=0)  for (i in 1:length(s)) {ss[2,i]<-mean(s[[i]],na.rm=TRUE)}
		if (fa!=0) for (i in 1:length(s)) {ss[3,i]<-sd(s[[i]],na.rm=TRUE)}
		if (fa!=0) print(ss)
		
		if (fa!=0 & para==0) boxplot(pa~factor(fact[,fa]),main="Boxplot",ylab="Diversity index")
		if (fa!=0 & graph==1) boxplot(pa.na~factor(fact[,fa]),main="Boxplot",ylab=colnames(param2)[para])
		if (fa!=0 & graph==2) lineplot.CI(fact[,fa], pa.na, type="p", cex = 2, xlab=names(fact[,fa]), ylab = colnames(param2)[para],main=paste("Points and SD using"))
		if (fa!=0 & graph==3) lineplot.CI(fact[,fa], pa.na, type="b", cex = 2, xlab=names(fact[,fa]), ylab = colnames(param2)[para],main=paste("Points, Lines and SD"))
	}
	
	compute.shapiro.test<-function()
	{
		para <- unlist(as.numeric(tcl(repee, "getvalue")))
		pa=0
		if (para==0) pa<-div
		if (para!=0) pa<-param2[,para]
		print(shapiro.test(pa))
		qqnorm(pa)
	}
	
	compute.bartlett.test<-function()
	{
		para <- unlist(as.numeric(tcl(repee, "getvalue")))
		fa <- unlist(as.numeric(tcl(repee1, "getvalue")))
		pa=0
		if (para==0) pa<-div
		if (para!=0) pa<-param2[,para]
		if (fa==0) tkmessageBox(message="No qualitative variable. Can not compute Bartlett test")
		if (fa==0) stop("No qualitative variable. Can not compute Bartlett test")
		if (fa!=0) print(bartlett.test(pa~factor(fact[,fa])))
	} 
	
	tkgrid(tklabel(tt, text = ""))
	t4<-tkframe(tt)
	b1<-tkbutton(t4,text="Calculate mean and standard deviation",command=compute.descriptive.stat)
	b2<-tkbutton(t4,text="Cancel",command=function() tkdestroy(tt))
	tkpack(b1,b2,side="left")
	tkgrid(t4)
	tkgrid(tklabel(tt,text="   "))
	t5<-tkframe(tt)    
	b1<-tkbutton(t5,text="Normality Shapiro-Wilk test",command=compute.shapiro.test)
	b2<-tkbutton(t5,text="Variances homogeneity",command=compute.bartlett.test)
	tkpack(b1,b2,side="left")
	tkgrid(t5)
	tkgrid(tklabel(tt,text="   "))     
	tkfocus(tt)
	
}

