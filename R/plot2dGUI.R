#############################
#    Plot profiles in 2D    #
#############################

"plot2dGUI"<-function()
{
  
  checkprofile() 
  
  tt <- tktoplevel()
  tkwm.title(tt,"Plot in 2 dimensions")
  tkgrid(tklabel(tt,text="    "))
  scr <- tkscrollbar(tt, repeatinterval=5,command=function(...)tkyview(tl,...))
  tl<-tklistbox(tt,height=10,width =50 ,selectmode="extended",yscrollcommand=function(...)tkset(scr,...),background="white")
  tkgrid(tklabel(tt,text="Which profile(s) do you want to plot?"))
  tkgrid(tklabel(tt,text="    "))
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
    sel<- as.numeric(tkcurselection(tl))+1
    if (length(sel)==1)
    plot(1:dim(mat6)[2],mat6[sel,],main=rownames(mat6)[sel],type="l",ylab="Relative abundance",col=1,xlab=paste("community",rownames(mat$profil)[sel]),yaxt="n") 
    else
    for (i in 1:length(sel))
    {
    plot(1:dim(mat6)[2],mat6[sel[i],],type="l",ylab="Relative abundance",col=i,xlab="community",yaxt="n")
    if (i!=length(sel))
    par(new=TRUE)
    }
  }
  
  tkgrid(tklabel(tt,text="    "))
  t1<-tkframe(tt)
  b1<-tkbutton(t1,text="Plot in 2D",command=mm)

  nw<-function()
  {
    x11()
  }

  b2<- tkbutton(t1, text = "New plot window...", command = nw)
  
  close<-function()
  {
    tkdestroy(tt)
  }

  b3<-tkbutton(t1,text="Cancel",command=close)
  tkpack(b1,b2,b3,side="left")
  tkgrid(t1)
  tkgrid(tklabel(tt,text="    "))
  tkfocus(tt)
}
