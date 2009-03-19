##########################################################################
#    GUI for the "SIMilarity PERcentages procedure (SIMPER)" analysis    #
##########################################################################

"simperGUI" <-function ()
{
  checkprofile()
  checkfact()
 
  tt <- tktoplevel()
  tkwm.title(tt, "SIMilarity PERcentages procedure")
  tkgrid(tklabel(tt, text = "                                                                                                                              "))
  
  tt2<-tkframe(tt)
  text2<-tklabel(tt2, text = "Choose the qualitative variable:")
  repee <- tkwidget(tt2, "ComboBox", editable = FALSE, values = names(fact),height=length(names(fact)),width=20)
  tkpack(text2,repee,side="left")
  tkgrid(tt2)
  tkgrid(tklabel(tt, text = ""))

  mm <- function() 
  {
    sel <- unlist(as.numeric(tcl(repee, "getvalue")) + 1)
    ae<<-sel
    tkdestroy(tt)
    simperGUI1()
  }
  
  tt11<-tkframe(tt)
  b1<-tkbutton(tt11, text = "Select", command = mm)
  
  close<-function()
  {
    tkdestroy(tt)
  }
  
  b2<-tkbutton(tt11,text="Cancel",command=close)
  tkpack(b1,b2,side="left")
  tkgrid(tt11)
  tkgrid(tklabel(tt, text = ""))
  tkfocus(tt)
}

"simperGUI1"<-function ()
{
  t2 <- tktoplevel()
  tkwm.title(t2, "SIMilarity PERcentages procedure")
  tkgrid(tklabel(t2, text = "                                                                                      "))
  
  tt3<-tkframe(t2)
  text3<-tklabel(tt3, text = "First level")
  repee <- tkwidget(tt3, "ComboBox", editable = FALSE, values = levels(fact[,ae]),height=length(levels(fact[,ae])))
  tkpack(text3,repee,side="left")
  tkgrid(tt3)
  
  tt4<-tkframe(t2)
  text4<-tklabel(tt4, text = "Second level")
  repee1 <- tkwidget(tt4, "ComboBox", editable = FALSE, values = levels(fact[,ae]),height=length(levels(fact[,ae])))
  tkpack(text4,repee1,side="left")
  tkgrid(tt4)
     
  tt2<-tkframe(t2)
  text2<-tklabel(tt2,text="Threshold in permille of contribution taking into account?")
  it <- tclVar("5")
  starte <- tkentry(tt2,width=8,textvariable=it)
  tkpack(text2,starte,side="left")
  tkgrid(tt2)
  tkgrid(tklabel(t2, text = "  "))
    
  mm <- function() 
  {
    repee <- unlist(as.numeric(tcl(repee, "getvalue"))+1)
    repee1 <- unlist(as.numeric(tcl(repee1, "getvalue"))+1)
    niv1=levels(fact[,ae])
    niv<-c(niv1[repee],niv1[repee1])
    nb<- as.numeric( tclvalue(it) )
    nb=as.numeric(nb)/100
    a<-simper(a=mat6,fact=fact[,ae],level=niv, seuil=nb)
    print(a)    
  }                                                         
  
  tt5<-tkframe(t2)
  b1<- tkbutton(tt5, text = "Compute", command = mm)
  
  close<-function()
  {
    tkdestroy(t2)
  }
  
  b2<-tkbutton(tt5,text="Cancel",command=close)
  tkpack(b1,b2,side="left")
  tkgrid(tt5)
  tkgrid(tklabel(t2, text = ""))
  tkfocus(t2)
}
