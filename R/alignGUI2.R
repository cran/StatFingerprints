alignGUI2 <-
function()
{
	tt <- tktoplevel()
	tkgrid(tklabel(tt,text=""))
	tkwm.title(tt,"Align fingerprint profile")
	scr <- tkscrollbar(tt, repeatinterval=5, command=function(...)tkyview(tl,...))
	tl<-tklistbox(tt,height=20,width =50 ,selectmode="single",yscrollcommand=function(...)tkset(scr,...),background="white")
	tkgrid(tklabel(tt,text="Which profile do you want to align?"))
	tkgrid(tklabel(tt,text=""))
	tkgrid(tl,scr)
	tkgrid.configure(scr,rowspan=10,sticky="nsw")
	
	prof <- alig
	for (i in 1:length(prof))
	{
		tkinsert(tl,"end",prof[i])
	}
	tkselection.set(tl,0)
	
	select.profile<-function()
	{
		sel<- as.numeric(tkcurselection(tl))+1
		ss<-align(mat=mat.raw,roxref=rxref,nam=sel)
		mat.align[sel,]<-ss
		if (substr(alig[sel],1,3)!="Ali")  
			alig[sel]<-paste("Align_",alig[sel],sep="")
		rownames(mat.align)<-rownames(mat.raw$profil)
		alig<<-alig
		mat.align<<-mat.align
		mat.analyse<<-mat.align
		
		####  maintain the position in the selection list of the profiles
		
		tkdelete(tl,sel-1)
		prof[sel] <- alig[sel]
		tkinsert(tl,sel-1,prof[sel])
	}
	
	help.align<-function()
	{
		ter <- tktoplevel()
		tkgrid(tklabel(ter,text=""))
		tkwm.title(ter,"Align fingerprint profile")
		zzz<-file.path(paste(.libPaths(), "/StatFingerprints/align.GIF",sep=""))
		icnn<-tkimage.create("photo", file = zzz)
		tcltklab <- tklabel(ter, image = icnn)
		tkgrid(tcltklab)
		tkgrid(tklabel(ter,text=""))
		tkgrid(tkbutton(ter,text="Cancel",command=function() tkdestroy(ter)))
		tkgrid(tklabel(ter,text=""))
	}
	
	tkgrid(tklabel(tt,text=""))
	t1<-tkframe(tt)
	b1<-tkbutton(t1,text="Align the fingerprint profile",command=select.profile)
	b2<-tkbutton(t1,text="Cancel",command=function() tkdestroy(tt))
	b3<-tkbutton(t1,text="Help picture",command=help.align)
	tkpack(b1,b2,b3,side="left")
	tkgrid(t1)
	tkgrid(tklabel(tt,text="    "))
	tkfocus(tt)
}

