deletefactGUI <-
function()
{
  checkprofile()
  checkfact()
  
  thhh<-tktoplevel()
  tkwm.title(thhh, "Select profiles according to a level of a qualitative variable")
  tkgrid(tklabel(thhh, text = "                                                                                                                                                          "))
  tkgrid(tklabel(thhh,font="arial 11", text ="Take care when using this function. Profiles will be definitively deleted of the project."))
  tkgrid(tklabel(thhh,font="arial 11", text ="Don't forget to save your project before and after having deleted profiles."))
  tkgrid(tklabel(thhh, text = " "))

  select.factor<-function()
  {
    checkfact()

    tt <- tktoplevel()
    tkwm.title(tt, "Select profiles according to a level of a qualitative variable")
    tkgrid(tklabel(tt, text = "                                                                                                                                                          "))
    tt2<-tkframe(tt)
    text2<-tklabel(tt2, text = "Choose the qualitative variable:")
    repee <- tkwidget(tt2, "ComboBox", editable = FALSE, values = names(fact),height=length(names(fact)),width=20)
    tkpack(text2,repee,side="left")
    tkgrid(tt2)
    
    select.level<-function ()
    {
      th <- tktoplevel()
      tkwm.title(th, "Select profiles according to a level of a qualitative variable")
      tkgrid(tklabel(th, text = "                                                                                                                                                          "))
      tkgrid(tklabel(th, text = "Which level ?"))
      repe=levels(fact[,ae])
      repee <- tkwidget(th, "ComboBox", editable = FALSE, values = repe)
      tkgrid(repee)
       tkgrid(tklabel(th, text = "                                                                                                                                                          "))
      tkgrid(tklabel(th, text = "Keep or delete ?"))
      repea=c("Keep","Delete")
      repeaa <- tkwidget(th, "ComboBox", editable = FALSE, values = repea)
      tkgrid(repeaa)
    
      window.factor <- function() 
      {
        repee <- unlist(as.numeric(tcl(repee, "getvalue")) + 1)
        typp <- unlist(as.numeric(tcl(repeaa, "getvalue")) + 1)
        
		relevels <- function(ff)
		{
			temp<-ff
			for (i in 1:length(ff))
			{
				temp[,i]<-factor(as.character(ff[,i]))
			}
			return(temp)
		} 
		
        delete.selected<-function()
		{
          sel=which(fact[,ae]==levels(fact[,ae])[repee])
          mat.raw$profil<<-mat.raw$profil[-sel,]
          mat.raw$rox<<-mat.raw$rox[-sel,] 
          div<<-div[-sel,]
          fact<-fact[-sel,]
          if (fact[1,1]!=1) fact<-relevels(fact)
          fact<<-fact 
          param<<-param[-sel,]
          alig<<-alig[-sel]
          mat.align<<-mat.align[-sel,]
          mat.background<<-mat.background[-sel,]
          mat.baseline<<-mat.baseline[-sel,]
          mat.range<<-mat.range[-sel,]
          mat.analyse<<-mat.analyse[-sel,]
          mat.normalise<<-mat.normalise[-sel,]
          mat.rebuilt<<-mat.rebuilt[-sel,]
          mat.binary<<-mat.binary[-sel,]
	    }
        
        keep.selected<-function()
		{
          sel=which(fact[,ae]==levels(fact[,ae])[repee])
          mat.raw$profil<<-mat.raw$profil[sel,]
          mat.raw$rox<<-mat.raw$rox[sel,] 
          div<<-div[sel]
          fact<-fact[sel,]
          if (fact[1,1]!=1) fact<-relevels(fact)
          fact<<-fact 
          param<<-param[sel,]
          alig<<-alig[sel]
          mat.align<<-mat.align[sel,]
          mat.background<<-mat.background[sel,]
          mat.baseline<<-mat.baseline[sel,]
          mat.range<<-mat.range[sel,]
          mat.analyse<<-mat.analyse[sel,]
          mat.normalise<<-mat.normalise[sel,]
          mat.rebuilt<<-mat.rebuilt[sel,]
          mat.binary<<-mat.binary[sel,]
	    }
         
        if (typp==1) keep.selected()
        if (typp==2) delete.selected()
        
        tkdestroy(th)
        tkfocus(MainMenu)
        if (typp==2) tkmessageBox(message="Profiles successfully deleted")
        if (typp==1) tkmessageBox(message="Profiles successfully selected")
      }
     
      ff<-tkframe(th)
      tkgrid(tklabel(th, text = ""))
      b1<- tkbutton(ff, text = "OK", command = window.factor)
      b2<-tkbutton(ff,text="Cancel",command=function() tkdestroy(th))
      tkpack(b1,b2,side="left")
      tkgrid(ff)
      tkgrid(tklabel(th, text = ""))
      tkfocus(th)
    }
  
    window.level <- function()
    {
      sel <- unlist(as.numeric(tcl(repee, "getvalue")) + 1)
      ae<<-sel
      select.level()
      tkdestroy(tt)
    }

    tkgrid(tklabel(tt, text = " "))
    ff1<-tkframe(tt)
    b1<- tkbutton(ff1, text = "OK", command = window.level)
	b2<-tkbutton(ff1,text="Cancel",command=function() tkdestroy(tt))
    tkpack(b1,b2,side="left")
    tkgrid(ff1)
    tkgrid(tklabel(tt, text = ""))
    tkfocus(tt)
  }

  thhh2<-tkframe(thhh)
  b<-tkbutton(thhh2,font="arial 12",text="Select levels",command=select.factor)
  tkpack(b)
  tkgrid(thhh2)
  tkgrid(tklabel(thhh, text = "  "))
  
  tkgrid(tkbutton(thhh,text="Cancel",command=function() tkdestroy(thhh)))
  tkgrid(tklabel(thhh,text=""))
}

