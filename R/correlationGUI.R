correlationGUI <-
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
	tt <- tktoplevel()
	tkwm.title(tt, "Simple correlation of Pearson")
	tkgrid(tklabel(tt, text = ""))
	
	t1<-tkframe(tt)
	text1<-tklabel(t1, text = "First quantitative variable")
	repee <- tkwidget(t1, "ComboBox", editable = FALSE, values = c(names(param2)),height=length(names(param2)))
	tkpack(text1,repee,side="left")
	tkgrid(t1)
	
	t2<-tkframe(tt)
	text2<-tklabel(t2, text = "Second quantitative variable")
	repee1 <- tkwidget(t2, "ComboBox", editable = FALSE, values = c(names(param2)),height=length(names(param2)))
	tkpack(text2,repee1,side="left")
	tkgrid(t2)
	
	compute.correlation <- function() 
	{
		facte1 <- unlist(as.numeric(tcl(repee, "getvalue"))+1)
		facte2 <- unlist(as.numeric(tcl(repee1, "getvalue"))+1)
		zz<-formula(param2[,facte1]~param2[,facte2])  
		z<-lm(zz)
		print(paste("Equation:", names(param2)[facte1] ," = ",z$coefficients[[2]] ," * ", names(param2)[facte2] ," + ",z$coefficients[[1]],sep=""))
		z<-cor.test(param2[,facte1],param2[,facte2]) 
		print(paste("p-value:   ",z$p.value))
		print(paste("Pearson r-squared:   ",(z$estimate)^2))  
	}
	
	tkgrid(tklabel(tt, text = ""))
	t3<-tkframe(tt)
	b1<- tkbutton(t3, text = "Compute correlation", command = compute.correlation)
	b2<-tkbutton(t3,text="Cancel",command=function() tkdestroy(tt))
	tkpack(b1,b2,side="left")
	tkgrid(t3)
	tkgrid(tklabel(tt, text = ""))
	tkfocus(tt)
}

