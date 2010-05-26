iterative.testGUI <-
function()
{  
	checkprofile() 
	checkfact()
	
	tt <- tktoplevel()
	tkwm.title(tt, "Iterative test")
	tkgrid(tklabel(tt, text = "                                                                                      "))
	
	tt2<-tkframe(tt)
	text2<-tklabel(tt2, text = "Choose the qualitative variable:")
	repee <- tkwidget(tt2, "ComboBox", editable = FALSE, values = names(fact),height=length(names(fact)),width=20,height=length(names(fact)))
	tkpack(text2,repee,side="left")
	tkgrid(tt2)
	tkgrid(tklabel(tt, text = ""))
	
	select.factor <- function() 
	{
		sel <- unlist(as.numeric(tcl(repee, "getvalue")) + 1)
		ae<<-sel
		iterative.testGUI1(fact=fact,ae=ae,mat.binary=mat.binary,mat.analyse=mat.analyse)
		tkdestroy(tt)
	}
	
	t1<-tkframe(tt)
	b1<- tkbutton(t1, text = "Select", command = select.factor)
	b2<-tkbutton(t1,text="Cancel",command=function() tkdestroy(tt))
	tkpack(b1,b2,side="left")
	tkgrid(t1)
	tkgrid(tklabel(tt, text = ""))
	tkfocus(tt)
}

