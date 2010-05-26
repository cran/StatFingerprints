trunckGUI <-
function()
{
	checkprofile()
	
	if(sum(mat.align)==length(mat.align))
	{
		tkmessageBox(message="Attention: profiles are not aligned")
		stop()
	}
	
	if(sum(mat.baseline)==length(mat.baseline)) 
	{
		tkmessageBox(message="Attention: baseline must be proceed before range to be efficient")
		stop()
	}
	
	if(sum(mat.align)!=length(mat.align)) m<-mat.align
	
	tt <- tktoplevel()
	tkwm.title(tt,"Define the range of all fingerprint profiles")
	tkgrid(tklabel(tt,text="                                                                                                                                                                                                                        "))
	
	define.range<-function()
	{  
		m<-trunck(mat=m)
		rownames(m)<-rownames(mat.baseline)
		mat.range<-m
		mat.range<<-mat.range
		mat.analyse<-m
		mat.analyse<<-mat.analyse
		tkmessageBox(message="Range of the fingerprint profiles successfully defined")
		dev.off()
		tkdestroy(tt)		
	}
	
	help.define.range<-function()
	{
		ter <- tktoplevel()
		tkgrid(tklabel(ter,text=""))
		tkwm.title(ter,"Define the range of the profiles")
		zzz<-file.path(paste(.libPaths(), "/StatFingerprints/range.GIF",sep=""))
		icnn<-tkimage.create("photo", file = zzz)
		tcltklab <- tklabel(ter, image = icnn)
		tkgrid(tcltklab)
		tkgrid(tklabel(ter,text=""))
		tkgrid(tkbutton(ter,text="Cancel",command=function() tkdestroy(ter)))
		tkgrid(tklabel(ter,text=""))
	}
	
	t1<-tkframe(tt)
	b1<-tkbutton(t1,text="Define range of the fingerpint profiles",command=define.range)
	b2<-tkbutton(t1,text="Cancel",command=function() tkdestroy(tt))
	b3<-tkbutton(t1,text="Help picture",command=help.define.range)
	tkpack(b1,b2,b3,side="left")
	tkgrid(t1)
	tkgrid(tklabel(tt,text="  "))
	tkfocus(tt)
}

