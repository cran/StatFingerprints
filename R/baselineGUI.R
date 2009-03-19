##########################################################################
#    GUI for the "Define a common baseline for all profiles" function    #
##########################################################################

"baselineGUI"<-function()
{
  checkprofile()
  
  if (sum(mat1)!=length(mat1)) m<-mat1
  if (sum(mat2)!=length(mat2)) m<-mat2
  tt <- tktoplevel()
  tkwm.title(tt,"Define a common baseline of all fingerprint profiles")
  tkgrid(tklabel(tt,text="                                                                                                                                                    ")) 
    
  mm<-function()
  {
    mat3<-baseline(m)
    mat3<<-mat3 
    mat6<<-mat3
    print("Common baseline successfully defined")
    tkdestroy(tt)
    tkfocus(MainMenu)
  }
  
  ht<-function()
  {
    ter <- tktoplevel()
    tkgrid(tklabel(ter,text=""))
    tkwm.title(ter,"Align fingerprint profile")##
    zzz<-file.path(paste(.libPaths(), "/StatFingerprints/baseline.GIF",sep=""))
    icnn<-tkimage.create("photo", file = zzz)
    tcltklab <- tklabel(ter, image = icnn)
    tkgrid(tcltklab)
    tkgrid(tklabel(ter,text=""))
    
    close<-function()
    {
      tkdestroy(ter)
    }
    
    tkgrid(tkbutton(ter,text="Cancel",command=close))
    tkgrid(tklabel(ter,text=""))
  }
  
  t1<-tkframe(tt)
  b1<-tkbutton(t1,text="Define baseline",command=mm)
  
  close<-function()
  {
    tkdestroy(tt)
  }
  
  b2<-tkbutton(t1,text="Cancel",command=close)
  b3<-tkbutton(t1,text="Help picture",command=ht)
  tkpack(b1,b2,b3,side="left")
  tkgrid(t1)
  tkgrid(tklabel(tt,text="  "))
  tkfocus(tt)
}
