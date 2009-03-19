#######################################################################
#    "Multivariate correlation (50-50 F-test & rotation)" function    #
#######################################################################

"ffcorGUI"<-function()
{
  checkprofile()
  checkparam()

  tt <- tktoplevel()
  tkwm.title(tt, "Multivariate correlation with the structure of the community")
  tkgrid(tklabel(tt, text = "                                                                                                                                                                   "))
  tkgrid(tklabel(tt, text = "Quantative variable to correlate"))
  repee <- tkwidget(tt, "ComboBox", editable = FALSE, values = names(param),height=length(names(param)))
  tkgrid(repee)
  
  tt2<-tkframe(tt)
  text2<-tklabel(tt2,text="Number of random start?")
  nb <- tclVar("10")
  starte<- tkentry(tt2,width=8,textvariable=nb)
  tkpack(text2,starte,side="left")
  tkgrid(tt2)
     
  mm <- function() 
  {
    facte <- unlist(as.numeric(tcl(repee, "getvalue")) + 1)
    nb<- as.numeric(tclvalue(nb))
    qq<-formula(paste("mat6~",colnames(param)[facte],sep=""))
    z<-ffmanova(aov(qq,data=param),nSim=nb,stand=FALSE)
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
