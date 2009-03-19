############################################
#    GUI for the normalisation function    #
############################################

"normalisationGUI"<-function()
{
  checkprofile()

  tt <- tktoplevel()
  tkwm.title(tt,"Normalisation of the fingerprint profiles")
  tkgrid(tklabel(tt,text="                                                                                          "))
  tkgrid(tklabel(tt,text="Algorithm of normalisation"))
   
####    Help to normalise

  helpp<-function()
  {
    tt1<-tktoplevel()
    tkwm.title(tt1,"Algorithm of normalisation                                              ")
    tkgrid(tklabel(tt1,text="  "))
    zzz<-file.path(paste(.libPaths(), "/StatFingerprints/norm2.GIF",sep=""))
    icnn<-tkimage.create("photo", file = zzz)
    tcltklab <- tklabel(tt1, image = icnn)
    tkgrid(tcltklab)

    close<-function()
    {
      tkdestroy(tt1)
    }
    
    tkgrid(tkbutton(tt1,text="Cancel",command=close))
    tkgrid(tklabel(tt1,text="  "))
    tkfocus(tt1)
  }
  
  t1<-tkframe(tt)
  b1<-tkbutton(t1,text="Help to choose algorithm of normalisation",command=helpp)
  sep <- c("Normalize without taking into account negative values","Normalize with all negative values equal to 0","Normalize with the minimum value equal to 0")
  sepe <- tkwidget(t1,"ComboBox",editable=FALSE,values=sep,height=3,width=50)
  tkpack(sepe,b1,side="left")
  tkgrid(t1)

  m<-mat$profil
  if(sum(mat1)!=length(mat1)) m<-mat1
  if(sum(mat2)!=length(mat2)) m<-mat2
  if(sum(mat3)!=length(mat3)) m<-mat3
  if(sum(mat4)!=length(mat4)) m<-mat4
  if(sum(mat5)!=length(mat5)) m<-mat5
  if(sum(mat8)!=length(mat8)) m<-mat8
  
####    Display result of normalisation

  mm<-function()
  {
    mat9<-matrix(nr=dim(mat$profil)[1],nc=2)
    mat9[]<-1   
    rownames(mat9)<-rownames(mat$profil) 
    mat9<<-mat9
    sep1 <-unlist(as.numeric(tcl(sepe,"getvalue"))+1 )
    m<-normalisation(mat=m,type=sep1)
    rownames(m)<-rownames(mat5)
    mat6<<-m
    mat7<<-m
    print("Normalization successfully done")
    tkdestroy(tt)
    tkfocus(MainMenu)
  }
    
  ht<-function()
  {
    ter <- tktoplevel()
    tkgrid(tklabel(ter,text=""))
    tkwm.title(ter,"Align fingerprint profile")
    zzz<-file.path(paste(.libPaths(), "/StatFingerprints/norm1.GIF",sep=""))
    icnn<-tkimage.create("photo", file = zzz)
    tcltklab <- tklabel(ter, image = icnn)
    tkgrid(tcltklab)
    tkgrid(tklabel(ter,text=""))
    
    close<-function()
    {
      tkdestroy(ter)
    }
    
    tkgrid(tkbutton(ter,text="Cancel",command=close))
    tkgrid(tklabel(ter,text=""))}
  
  tkgrid(tklabel(tt,text="      "))
  t2<-tkframe(tt)
  b1<-tkbutton(t2,text="Normalize all",command=mm)
 
  close<-function()
  {
    tkdestroy(tt)
  }

  b2<-tkbutton(t2,text="Cancel",command=close)
  b3<-tkbutton(t2,text="Help picture",command=ht)
  tkpack(b1,b2,b3,side="left")
  tkgrid(t2)
  tkgrid(tklabel(tt,text="  "))
  tkfocus(tt)
}
