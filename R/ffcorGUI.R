#######################################################################
#    "Multivariate correlation (50-50 F-test & rotation)" function    #
#######################################################################

"ffcorGUI"<-function()
{
  if (sum(na.omit(param))==6) tkmessageBox(message="Error, no quantitative variables to compute multivariate correlation")
  if (sum(na.omit(param))==6) stop("Error, no quantitative variables to compute multivariate correlation")

  tt <- tktoplevel()
  tkwm.title(tt, "Multivariate correlation with the structure of the community")
  tkgrid(tklabel(tt, text = "                                                                                                                                                                   "))
  tkgrid(tklabel(tt, text = "Quantative variable to correlate"))
  repee <- tkwidget(tt, "ComboBox", editable = FALSE, values = names(param),height=length(names(param)))
  tkgrid(repee)
  
  tt2<-tkframe(tt)
  text2<-tklabel(tt2,text="Number of random start?")
  nb <- tclVar("100")
  starte<- tkentry(tt2,width=8,textvariable=nb)
  tkpack(text2,starte,side="left")
  tkgrid(tt2)
     
  mm <- function() 
  {
    facte <- unlist(as.numeric(tcl(repee, "getvalue")) + 1)
    nb<- as.numeric(tclvalue(nb))
    qq<-formula(paste("mat6~",colnames(param)[facte],sep=""))
  
    tt1 <- tktoplevel()
    tkwm.title(tt1,"Working")
    tkgrid(tklabel(tt1,font="arial 12",text="Please wait...\n This operation may take several minutes                           "))
    tkfocus(tt1)
    tkconfigure(tt1,cursor="watch")
    z<-ffmanova(aov(qq,data=param),nSim=nb,stand=FALSE)
    tkdestroy(tt1)
    print(z)
  }
  
  tkgrid(tklabel(tt, text = " "))
  tt4<-tkframe(tt)
  b1<- tkbutton(tt4, text = "Compute", command = mm)
  
  close<-function()
  {
    tkdestroy(tt)
  }
  
  b2<-tkbutton(tt4,text="Cancel",command=close)
  tkpack(b1,b2,side="left")
  tkgrid(tt4)
  tkgrid(tklabel(tt,text="   "))
  tkgrid(tklabel(tt, text = paste("see http://www.matforsk.no/ola/ for details")))
  tkgrid(tklabel(tt,text="   "))
  tkfocus(tt)
}