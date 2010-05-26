ffcorGUI <-
function()
{
	checkprofile()
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
	
	param2<-as.data.frame(param2)
	
	tt <- tktoplevel()
	tkwm.title(tt, "Multivariate correlation with the structure of the community")
	tkgrid(tklabel(tt, text = "                                                                                                                                                                   "))
	tkgrid(tklabel(tt, text = "Quantative variable to correlate"))
	repee <- tkwidget(tt, "ComboBox", editable = FALSE, values = colnames(param2),height=length(colnames(param2)))
	tkgrid(repee)
	
	tt2<-tkframe(tt)
	text2<-tklabel(tt2,text="Number of random start?")
	nb <- tclVar("10")
	starte<- tkentry(tt2,width=8,textvariable=nb)
	tkpack(text2,starte,side="left")
	tkgrid(tt2)
	
	compute.ffmanove <- function() 
	{
		facte <- unlist(as.numeric(tcl(repee, "getvalue")) + 1)
		nb<- as.numeric(tclvalue(nb))
		qq<-as.formula(paste("mat.analyse~",colnames(param2)[facte],sep=""))
		z<-ffmanova_sf(aov(qq,data=param2),nSim=nb,stand=FALSE)
		print(z)
	}
	
	tkgrid(tklabel(tt, text = " "))
	tt4<-tkframe(tt)
	b1<- tkbutton(tt4, text = "Compute", command = compute.ffmanove)
	b2<-tkbutton(tt4,text="Cancel",command=function() tkdestroy(tt))
	tkpack(b1,b2,side="left")
	tkgrid(tt4)
	tkgrid(tklabel(tt,text="   "))
	tkgrid(tklabel(tt, text = paste("see http://www.matforsk.no/ola/ for details")))
	tkgrid(tklabel(tt,text="   "))
	tkfocus(tt)
}

