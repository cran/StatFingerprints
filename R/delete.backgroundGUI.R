delete.backgroundGUI <-
function()                                  
{
####  Delete background on the profile

  checkprofile()

  if (sum(mat.range)==length(mat.range))
  {
	  tkmessageBox(message="Attention: range function must be done before deleting background")
	  stop()
  }
  matt<-mat.normalise
   
  tt <- tktoplevel()
  tkwm.title(tt,"Delete background under fingerprint profiles")
  tkgrid(tklabel(tt,text="                                                                                                                                                   "))
  
  tt2<-tkframe(tt)
  text2<-tklabel(tt2,text="Radius of the rollball?")
  it <- tclVar("10")
  starte <- tkentry(tt2,width=8,textvariable=it)

  tkpack(text2,starte,side="left")
  tkgrid(tt2)
  
  load.delete.background<-function()
  {      
    radius=as.numeric(tclvalue(it))
    ma<-delete.background(mat=matt,radius=radius)
    mat.background<<-ma
    mat.analyse<<-ma
	mat.binary<-matrix(nr=5,nc=2)
	mat.binary[]<-1
	mat.binary<<-mat.binary
    tkmessageBox(message="Background successfully deleted")
    dev.off()
	tkdestroy(tt)
  }
  
  tkgrid(tklabel(tt,text=" "))

  help.rollball<-function()
  {
    trr <- tktoplevel()
    tkwm.title(trr,"What is the radius of the rollball")
    tkgrid(tklabel(trr,text=""))
    zzz<-file.path(paste(.libPaths(), "/StatFingerprints/radius.GIF",sep=""))
    icnn<-tkimage.create("photo", file = zzz)
    tcltklab <- tklabel(trr, image = icnn)
    tkgrid(tcltklab)
    tkgrid(tklabel(trr,text=""))
  }
  
  tkgrid(tkbutton(tt,text="Help to define the rollball",command=help.rollball))
  tkgrid(tklabel(tt,text=" ") )
  
  t1<-tkframe(tt)
  b1<-tkbutton(t1,text="Delete background",command=load.delete.background)
  b2<-tkbutton(t1,text="Cancel",command=function() tkdestroy(tt))
  tkpack(b1,b2,side="left")
  tkgrid(t1)
  tkgrid(tklabel(tt,text="    "))

  tkfocus(tt)
}

