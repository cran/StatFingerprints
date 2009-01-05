##############################################################
#    Function to "Select profiles using levels of factor"    #
##############################################################

"deletefactGUI"<-function()
{
  thhh<-tktoplevel()
  tkwm.title(thhh, "Delete profiles according to a level of a qualitative variable")
  tkgrid(tklabel(thhh, text = "                                                                                                                                                          "))
  tkgrid(tklabel(thhh, text ="Take care when using this function. Profiles will be definitively deleted of the project."))
  tkgrid(tklabel(thhh, text ="Don't forget to save your current project and after having deleted profiles to save the new project in another file directory."))
  tkgrid(tklabel(thhh, text = " "))

  deletefactGUI1<-function()
  {
    if (fact[1,1]==1) tkmessageBox(message="Error, no qualitative variables to compute ANOSIM")
    if (fact[1,1]==1) stop("Error, no qualitative variables to compute ANOSIM")
    tkmessageBox
    tt <- tktoplevel()
    tkwm.title(tt, "Delete profiles according to a level of a qualitative variable")
    tkgrid(tklabel(tt, text = "                                                                                                                                                          "))
    tt2<-tkframe(tt)
    text2<-tklabel(tt2, text = "Choose the qualitative variable:")
    repee <- tkwidget(tt2, "ComboBox", editable = FALSE, values = names(fact),height=length(names(fact)),width=20)
    tkpack(text2,repee,side="left")
    tkgrid(tt2)
    
    deletefactGUI2<-function ()
    {
      th <- tktoplevel()
      tkwm.title(th, "Delete profiles according to a level of a qualitative variable")
      tkgrid(tklabel(th, text = "                                                                                                                                                          "))
      tkgrid(tklabel(th, text = "Level to delete"))
      repe=levels(fact[,ae])
      repee <- tkwidget(th, "ComboBox", editable = FALSE, values = repe)
      tkgrid(repee)
    
      mm <- function() 
      {
        repee <- unlist(as.numeric(tcl(repee, "getvalue")) + 1)
        sel=which(fact[,ae]==levels(fact[,ae])[repee])
        mat$profil<<-mat$profil[-sel,]
        mat$rox<<-mat$rox[-sel,] 
        div<<-div[-sel]
        fact<-fact[-sel,]
        
        relevels<-function(ff)
        {
          for (i in 1:dim(ff)[2])
          {
            f<-ff[,i]
            a=vector(length=length(levels(f)))
            
            for (j in 1:length(levels(f)))
            {
              a[j]<-length(which(f==levels(f)[j]))
            }
            
            a=which(a==0)
            j<-vector(length=length(f))
            j<-factor(j)
            if (length(a)!=0) levels(j)<-levels(f)[-a]
            if (length(a)==0) levels(j)<-levels(f)
            j[]<-f
            ff[,i]<-j
          }
        return(ff)
        }   
        
        if (fact[1,1]!=1) fact<-relevels(fact)
        fact<<-fact 
        param<<-param[-sel,]
        alig<<-alig[-sel]
        mat1<<-mat1[-sel,]
        mat2<<-mat2[-sel,]
        mat3<<-mat3[-sel,]
        mat4<<-mat4[-sel,]
        mat5<<-mat5[-sel,]
        mat6<<-mat6[-sel,]
        mat7<<-mat7[-sel,]
        mat8<<-mat8[-sel,]
        mat9<<-mat9[-sel,]
        tkdestroy(th)
        tkfocus(MainMenu)
        tkmessageBox(message="Profiles successfully deleted")
    }
    
    ff<-tkframe(th)
    tkgrid(tklabel(th, text = ""))
    b1<- tkbutton(ff, text = "OK", command = mm)
    
    close<-function()
    {
      tkdestroy(th)
    }
    
    b2<-tkbutton(ff,text="Cancel",command=close)
    tkpack(b1,b2,side="left")
    tkgrid(ff)
    tkgrid(tklabel(th, text = ""))
    tkfocus(th)
}







mm <- function(){
  sel <- unlist(as.numeric(tcl(repee, "getvalue")) + 1)
  ae<<-sel
  deletefactGUI2()
  tkdestroy(tt)
  }
tkgrid(tklabel(tt, text = " "))
ff1<-tkframe(tt)
b1<- tkbutton(ff1, text = "OK", command = mm)
cc<-function(){tkdestroy(tt)}
b2<-tkbutton(ff1,text="Cancel",command=cc)
tkpack(b1,b2,side="left")
tkgrid(ff1)
tkgrid(tklabel(tt, text = ""))
tkfocus(tt)
}

thhh2<-tkframe(thhh)
  b1<-tkbutton(thhh2,text="Step 1 : Save the current project",command=saveGUI)
  b2<-tkbutton(thhh2,text="Step 2 : Delete the profiles",command=deletefactGUI1)
  b3<-tkbutton(thhh2,text="Step 3 : Save the new project",command=saveasGUI)
  tkpack(b1,b2,b3,side="left")
  tkgrid(thhh2)
  tkgrid(tklabel(thhh, text = "  "))
  cc<-function(){tkdestroy(thhh)}
  tkgrid(tkbutton(thhh,text="Cancel",command=cc))
  tkgrid(tklabel(thhh,text=""))
}