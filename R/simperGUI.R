simperGUI <-
function ()
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

  load.simper <- function() 
  {
    sel <- unlist(as.numeric(tcl(repee, "getvalue")) + 1)
    ae<<-sel
    tkdestroy(tt)
    simperGUI1()
  }
  
  tt11<-tkframe(tt)
  b1<-tkbutton(tt11, text = "Select", command = load.simper)
  b2<-tkbutton(tt11,text="Cancel",command=function() tkdestroy(tt))
  tkpack(b1,b2,side="left")
  tkgrid(tt11)
  tkgrid(tklabel(tt, text = ""))
  tkfocus(tt)
  }

