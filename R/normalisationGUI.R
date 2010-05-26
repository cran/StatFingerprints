normalisationGUI <-
function()
{
	checkprofile()	
	
	if(sum(mat.baseline)==length(mat.baseline))
	{
		tkmessageBox(message="Baseline must be defined before normalisation")
		stop()
	}
	
	if(sum(mat.range)==length(mat.range))
	{
		tkmessageBox(message="You better trunk your profiles (range function) before normalisation")
		stop()
	}
	
	if(sum(mat.range)!=length(mat.range))
		m<-mat.range
	
	tt <- tktoplevel()
	tkwm.title(tt,"Normalisation of the fingerprint profiles")
	tkgrid(tklabel(tt,text="                                                                                          "))
	tkgrid(tklabel(tt,text="Algorithm of normalisation"))
	
	####    Help to normalise
	
	help.choose.normalisation<-function()
	{
		tt1<-tktoplevel()
		tkwm.title(tt1,"Algorithm of normalisation                                              ")
		tkgrid(tklabel(tt1,text="  "))
		zzz<-file.path(paste(.libPaths(), "/StatFingerprints/norm2.GIF",sep=""))
		icnn<-tkimage.create("photo", file = zzz)
		tcltklab <- tklabel(tt1, image = icnn)
		tkgrid(tcltklab)
		tkgrid(tkbutton(tt1,text="Cancel",command=function() tkdestroy(tt1)))
		tkgrid(tklabel(tt1,text="  "))
		tkfocus(tt1)
	}
	
	t1<-tkframe(tt)
	b1<-tkbutton(t1,text="Help to choose algorithm of normalisation",command=help.choose.normalisation)
	sep <- c("Normalize without taking into account negative values","Normalize with all negative values equal to 0","Normalize with the minimum value equal to 0")
	sepe <- tkwidget(t1,"ComboBox",editable=FALSE,values=sep,height=3,width=50)
	tkpack(sepe,b1,side="left")
	tkgrid(t1)
	
	####    Display result of normalisation
	
	compute.normalisation<-function()
	{
		if(sum(mat.binary)!=length(mat.binary)) 
		{
			mat.binary<-matrix(nr=5,nc=2)
			mat.binary[]<-1
			mat.binary<<-mat.binary
		}
		
		if(sum(mat.background)!=length(mat.background)) 
		{
			mat.background<-matrix(nr=5,nc=2)
			mat.background[]<-1
			mat.background<<-mat.background
		}
		
		sep1 <-unlist(as.numeric(tcl(sepe,"getvalue"))+1 )
		m<-normalisation(mat=m,type=sep1)
		rownames(m)<-rownames(mat.range)
		mat.analyse<<-m
		mat.normalise<<-m
		tkmessageBox(message="Normalization successfully done")
		dev.off()
		tkdestroy(tt)
	}
	
	help.normalisation<-function()
	{
		ter <- tktoplevel()
		tkgrid(tklabel(ter,text=""))
		tkwm.title(ter,"Align fingerprint profile")
		zzz<-file.path(paste(.libPaths(), "/StatFingerprints/norm1.GIF",sep=""))
		icnn<-tkimage.create("photo", file = zzz)
		tcltklab <- tklabel(ter, image = icnn)
		tkgrid(tcltklab)
		tkgrid(tklabel(ter,text=""))
		tkgrid(tkbutton(ter,text="Cancel",command=function() tkdestroy(ter)))
		tkgrid(tklabel(ter,text=""))
	}
	
	tkgrid(tklabel(tt,text="      "))
	t2<-tkframe(tt)
	b1<-tkbutton(t2,text="Normalize all",command=compute.normalisation)
	b2<-tkbutton(t2,text="Cancel",command=function() tkdestroy(tt))
	b3<-tkbutton(t2,text="Help picture",command=help.normalisation)
	tkpack(b1,b2,b3,side="left")
	tkgrid(t2)
	tkgrid(tklabel(tt,text="  "))
	tkfocus(tt)
}

