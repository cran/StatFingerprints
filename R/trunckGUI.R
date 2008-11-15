#################################################################
#    GUI for the "Define the range of the profiles" function    #
#################################################################

"trunckGUI"<-function()
{
  m<-mat$profil
  if(mat1[1,1]!=1) m<-mat1
  if(mat2[1,1]!=1) m<-mat2
  if(mat3[1,1]!=1) m<-mat3
  if(mat4[1,1]!=1) m<-mat4 
  
  tt <- tktoplevel()
  tkwm.title(tt,"Define a common baseline of all fingerprint profiles")
  tkgrid(tklabel(tt,text="                                                                                                                                                                                                                        "))
  
  mm<-function()
  {  
    m<-trunck(mat=m)
    rownames(m)<-rownames(mat4)
    mat5<-m
    mat5<<-mat5
    mat6<-m
    mat6<<-mat6
    print("Range of the fingerprint profiles successfully defined")
    tkdestroy(tt)
    tkfocus(MainMenu)
  }
  
  ht<-function()
  {
    ter <- tktoplevel()
    tkgrid(tklabel(ter,text=""))
    tkwm.title(ter,"Align fingerprint profile")
    zzz<-file.path(paste(.libPaths(), "/StatFingerprints/range.GIF",sep=""))
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
  b1<-tkbutton(t1,text="Define range of the fingerpint profiles",command=mm)
  
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





