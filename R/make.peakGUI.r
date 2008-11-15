###########################################
#    GUI for the function to make peak    #
###########################################

"make.peakGUI"<-function()
{
  def<-function(mat6){mat8<-mat6
 mat8<<-mat8}
  def(mat6)  
  
  tt <- tktoplevel()
  tkwm.title(tt,"Rebuild peak")
  tkgrid(tklabel(tt,text=""))
  scr <- tkscrollbar(tt, repeatinterval=5,command=function(...)tkyview(tl,...))
  tl<-tklistbox(tt,height=10,width =50 ,selectmode="single",yscrollcommand=function(...)tkset(scr,...),background="white")
  tkgrid(tklabel(tt,text="On which profile do you want to rebuild peak?"))
  tkgrid(tklabel(tt,text=""))
  tkgrid(tl,scr)
  tkgrid.configure(scr,rowspan=10,sticky="nsw")

  prof <- rownames(mat$profil)
  for (i in 1:length(prof))
  {
    tkinsert(tl,"end",prof[i])
  }
  tkselection.set(tl,0)

  mm<-function()
  {
    if(mat1[1,1]!=1) m<-mat1
    if(mat2[1,1]!=1) m<-mat2
    if(mat3[1,1]!=1) m<-mat3
    if(mat4[1,1]!=1) m<-mat4
    if(mat5[1,1]!=1) m<-mat5
    if(mat7[1,1]!=1) m<-mat7
    if(length(which(mat8!=1))!=0) m<-mat8
    mat8<-m;mat8[]<-1
    sel1<- as.numeric(tkcurselection(tl))+1
    ssa<-make.peak(prof=m[sel1,])
    sel<<-sel1 
    ss<<-ssa 
  }
  
  sav<-function()
  {
    mat8[sel,]<-ss
    rownames(mat8)<-rownames(mat$profil)
    mat8<<-mat8 
    mat6<<-mat8
    tkdestroy(tt)
    make.peakGUI()
  }
  
  ht<-function()
  {
    ter <- tktoplevel()
    tkgrid(tklabel(ter,text=""))
    tkwm.title(ter,"Rebuild peak")
    zzz<-file.path(paste(.libPaths(), "/StatFingerprints/make.GIF",sep=""))
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
  
  tkgrid(tklabel(tt,text=""))
  t1<-tkframe(tt)
  b1<-tkbutton(t1,text="Compute peak modification",command=mm)
 
  close<-function()
  {
    tkdestroy(tt)
  }

  b2<-tkbutton(t1,text="Cancel",command=close)
  b3<-tkbutton(t1,text="Help picture",command=ht)
  b4<-tkbutton(t1,text="Save the modified peak",command=sav)
  tkpack(b1,b4,b2,b3,side="left")
  tkgrid(t1)
  tkgrid(tklabel(tt,text=""))
  tkfocus(tt)
}