make.peakGUI <-
function()
{
	checkprofile()
	
	mat.rebuilt<<-mat.analyse
	mat.rebuilt<<-mat.rebuilt
	
	tt <- tktoplevel()
	tkwm.title(tt,"Rebuild peak")
	tkgrid(tklabel(tt,text=""))
	scr <- tkscrollbar(tt, repeatinterval=5,command=function(...)tkyview(tl,...))
	tl<-tklistbox(tt,height=10,width =50 ,selectmode="single",yscrollcommand=function(...)tkset(scr,...),background="white")
	tkgrid(tklabel(tt,text="On which profile do you want to rebuild peak?"))
	tkgrid(tklabel(tt,text=""))
	tkgrid(tl,scr)
	tkgrid.configure(scr,rowspan=10,sticky="nsw")
	
	prof <- rownames(mat.raw$profil)
	for (i in 1:length(prof))
	{
		tkinsert(tl,"end",prof[i])
	}
	tkselection.set(tl,0)
	
	compute.peak.modification<-function()
	{
		if(sum(mat.normalise)==length(mat.normalise))
		{
			tkmessageBox(message="Attention: normalisation must be done before rebuilding peak")
			stop()
		}
		if(sum(mat.normalise)!=length(mat.normalise))
			m<-mat.normalise
		if(length(which(mat.rebuilt!=1))!=0)
			m<-mat.rebuilt
		mat.rebuilt<-m
		mat.rebuilt[]<-1
		sel1<- as.numeric(tkcurselection(tl))+1
		ssa<-make.peak(prof=m[sel1,])
		sel<<-sel1 
		ss<<-ssa 
	}
	
	save.peak<-function()
	{
		mat.rebuilt[sel,]<-ss
		rownames(mat.rebuilt)<-rownames(mat.raw$profil)
		mat.rebuilt<<-mat.rebuilt 
		mat.analyse<<-mat.rebuilt
		tkdestroy(tt)
		make.peakGUI()
	}
	
	help.make.peak<-function()
	{
		ter <- tktoplevel()
		tkgrid(tklabel(ter,text=""))
		tkwm.title(ter,"Rebuild peak")
		zzz<-file.path(paste(.libPaths(), "/StatFingerprints/make.GIF",sep=""))
		icnn<-tkimage.create("photo", file = zzz)
		tcltklab <- tklabel(ter, image = icnn)
		tkgrid(tcltklab)
		tkgrid(tklabel(ter,text=""))
		tkgrid(tkbutton(ter,text="Cancel",command=function() tkdestroy(ter)))
		tkgrid(tklabel(ter,text=""))
	}
	
	tkgrid(tklabel(tt,text=""))
	t1<-tkframe(tt)
	b1<-tkbutton(t1,text="Compute peak modification",command=compute.peak.modification)
	b2<-tkbutton(t1,text="Cancel",command=function() tkdestroy(tt))
	b3<-tkbutton(t1,text="Help picture",command=help.make.peak)
	b4<-tkbutton(t1,text="Save the modified peak",command=save.peak)
	tkpack(b1,b4,b2,b3,side="left")
	tkgrid(t1)
	tkgrid(tklabel(tt,text=""))
	tkfocus(tt)
}

