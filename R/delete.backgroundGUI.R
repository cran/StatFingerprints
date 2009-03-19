################################################################
#    GUI for the "Delete background under profiles" function   #
################################################################

"delete.backgroundGUI"<-function()                                  
{
####  Delete background on the profile

  checkprofile()

  tt <- tktoplevel()
  tkwm.title(tt,"Delete background under fingerprint profiles")
  tkgrid(tklabel(tt,text="                                                                                                                                                   "))
  
  tt2<-tkframe(tt)
  text2<-tklabel(tt2,text="Radius of the rollball?")
  it <- tclVar("10")
  starte <- tkentry(tt2,width=8,textvariable=it)

  tkpack(text2,starte,side="left")
  tkgrid(tt2)
  
  matt<-mat$profil
  if (sum(mat1[1,])!=length(mat1[1,])) matt<-mat1

  f<-function()
  {      
    radius=as.numeric(tclvalue(it))
    ma<-delete.background(mat=matt,radius=radius)
    mat2<<-ma
    mat6<<-ma
    print("Background successfully deleted")
    tkfocus(MainMenu)
  }
  
  tkgrid(tklabel(tt,text=" "))

  helpp<-function()
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
  
  tkgrid(tkbutton(tt,text="Help to define the rollball",command=helpp))
  tkgrid(tklabel(tt,text=" ") )
  
  t1<-tkframe(tt)
  b1<-tkbutton(t1,text="Delete background",command=f)
  cc<-function()
  {
    tkdestroy(tt)
  }
  b2<-tkbutton(t1,text="Cancel",command=cc)
  tkpack(b1,b2,side="left")
  tkgrid(t1)
  tkgrid(tklabel(tt,text="    "))

  tkfocus(tt)
}
