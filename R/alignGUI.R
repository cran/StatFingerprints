###############################################
#    GUI for the "Align profiles" function    #
###############################################

"alignGUI"<-function()
{
####     Warning
####     Rox must be define first

  if(length(which(mat$rox==mat$profil)==TRUE)==dim(mat$profil)[1]*dim(mat$profil)[2]) tkmessageBox(message="You have import an ecological table. Alignment cannot be performed", icon="info", type="ok")
  if(length(which(mat$rox==mat$profil)==TRUE)==dim(mat$profil)[1]*dim(mat$profil)[2]) stop("You have import an ecological table. Alignment cannot be performed")
  if (rxref[1]==0) tkmessageBox(message="First, you must define the reference, on which profiles will be aligned. See Data transformation/Rox", icon="info", type="ok")
  if (rxref[1]==0) stop("First, you must define the reference, on which profiles will be aligned. See Data transformation/Rox")

  create<-function() 
  {
    mat1<-matrix(nc=rxref[length(rxref)]-rxref[1],nr=dim(mat$rox)[1])
    mat1<<-mat1
  }

  if (dim(mat1)[2]==2)
    mat1<-create()
  mat1<<-mat1 
  
  alignGUI2()
}

####    List of profiles to align

alignGUI2<-function()
{
  tt <- tktoplevel()
  tkgrid(tklabel(tt,text=""))
  tkwm.title(tt,"Align fingerprint profile")
  scr <- tkscrollbar(tt, repeatinterval=5, command=function(...)tkyview(tl,...))
  tl<-tklistbox(tt,height=10,width =50 ,selectmode="single",yscrollcommand=function(...)tkset(scr,...),background="white")
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

  mm<-function()
  {  
    sel<- as.numeric(tkcurselection(tl))+1
    ss<-align(mat=mat,roxref=rxref,nam=sel)
    mat1[sel,]<-ss
    if (substr(alig[sel],1,3)!="Ali")  
      alig[sel]<-paste("Align_",alig[sel],sep="")
    rownames(mat1)<-rownames(mat$profil)
    alig<<-alig
   mat1<<-mat1
    mat6<<-mat1 
    tkdestroy(tt)
    alignGUI2()
  }
  
  ht<-function()
  {
    ter <- tktoplevel()
    tkgrid(tklabel(ter,text=""))
    tkwm.title(ter,"Align fingerprint profile")
    zzz<-file.path(paste(.libPaths(), "/StatFingerprints/align.GIF",sep=""))
    icnn<-tkimage.create("photo", file = zzz)
    tcltklab <- tklabel(ter, image = icnn)
    tkgrid(tcltklab)
    tkgrid(tklabel(ter,text=""))
    close<-function(){tkdestroy(ter)}
    tkgrid(tkbutton(ter,text="Cancel",command=close))
    tkgrid(tklabel(ter,text=""))
  }
  
  tkgrid(tklabel(tt,text=""))
  t1<-tkframe(tt)
  b1<-tkbutton(t1,text="Align the fingerprint profile",command=mm)
  cc<-function() {tkdestroy(tt)}
  b2<-tkbutton(t1,text="Cancel",command=cc)
  b3<-tkbutton(t1,text="Help picture",command=ht)
  tkpack(b1,b2,b3,side="left")
  tkgrid(t1)
  tkgrid(tklabel(tt,text="    "))
  tkfocus(tt)
}
