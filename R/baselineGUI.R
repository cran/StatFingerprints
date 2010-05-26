baselineGUI <-
function()
{
	checkprofile()
	
	####  Warning : alignment must be done  ####
	if (sum(mat.align)==length(mat.align)) 
	{
		tkmessageBox(message="Profiles must be aligned before processing baseline")
		stop()
	}
	if (sum(mat.align)!=length(mat.align))
		m<-mat.align
	
	
	tt <- tktoplevel()
	tkwm.title(tt,"Define a common baseline of all fingerprint profiles")
	tkgrid(tklabel(tt,text="                                                                                                                                                    ")) 
	
	####  Call baseline()  ####
	
	compute.baseline<-function()
	{
		mat.baseline<-baseline(m)
		mat.baseline<<-mat.baseline 
		mat.analyse<<-mat.baseline
		tkmessageBox(message="Common baseline successfully defined")
		dev.off()
		tkdestroy(tt)
	}
	
	####  Display help baseline picture  ####
	
	help.baseline<-function()
	{
		ter <- tktoplevel()
		tkgrid(tklabel(ter,text=""))
		tkwm.title(ter,"Baseline correction")
		zzz<-file.path(paste(.libPaths(), "/StatFingerprints/baseline.GIF",sep=""))
		icnn<-tkimage.create("photo", file = zzz)
		tcltklab <- tklabel(ter, image = icnn)
		tkgrid(tcltklab)
		tkgrid(tklabel(ter,text=""))
		tkgrid(tkbutton(ter,text="Cancel",command=function() tkdestroy(ter)))
		tkgrid(tklabel(ter,text=""))
	}
	
	####  Main Window  ####
	
	t1<-tkframe(tt)
	b1<-tkbutton(t1,text="Define baseline",command=compute.baseline)
	b2<-tkbutton(t1,text="Cancel",command=function() tkdestroy(tt))
	b3<-tkbutton(t1,text="Help picture",command=help.baseline)
	tkpack(b1,b2,b3,side="left")
	tkgrid(t1)
	tkgrid(tklabel(tt,text="  "))
	tkfocus(tt)
}

