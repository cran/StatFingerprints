#####################################################################################
#    GUI for the "Iterative tests (t test/Mann-Whitney/Fisher's exact)" function    #
#####################################################################################

"iterative.testGUI"<-function()
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
  
  mm <- function() 
  {
    sel <- unlist(as.numeric(tcl(repee, "getvalue")) + 1)
    ae<<-sel
    iterative.testGUI1(fact=fact,ae=ae,mat9=mat9,mat6=mat6)
    tkdestroy(tt)
  }

  t1<-tkframe(tt)
  b1<- tkbutton(t1, text = "Select", command = mm)
  
  close<-function()
  {
    tkdestroy(tt)
  }
  
  b2<-tkbutton(t1,text="Cancel",command=close)
  tkpack(b1,b2,side="left")
  tkgrid(t1)
  tkgrid(tklabel(tt, text = ""))
  tkfocus(tt)
}
                                            
####    Choose levels
                                             
iterative.testGUI1<-function (fact,ae,mat9,mat6)
{
  t2 <- tktoplevel()
  tkwm.title(t2, "Iterative test")
  tkgrid(tklabel(t2, text = "                             "))
  
  tt2<-tkframe(t2)
  text2<-tklabel(tt2, text = "First level")
  repee <- tkwidget(tt2, "ComboBox", editable = FALSE, values = levels(fact[,ae]),height=length(levels(fact[,ae])))
  tkpack(text2,repee,side="left")
  tkgrid(tt2)
  
  tt3<-tkframe(t2)
  text3<-tklabel(tt3, text = "Second level")
  repee1 <- tkwidget(tt3, "ComboBox", editable = FALSE, values = levels(fact[,ae]),height=length(levels(fact[,ae])))
  tkpack(text3,repee1,side="left")
  tkgrid(tt3)
  
  tt4<-tkframe(t2)
  text4<-tklabel(tt4, text = "Choose your iterative test")
  test <- tkwidget(tt4, "ComboBox", editable = FALSE, values = c("t-test (parametric)", "Mann Whitney (non-parametric)","Fisher's exact (presence/absence profiles)"),width=40,height=3)
  tkpack(text4,test,side="left")
  tkgrid(tt4)
  a<<-1  

  mm <- function() 
  {
    a<<-0
    repee <- unlist(as.numeric(tcl(repee, "getvalue"))+1)
    repee1 <- unlist(as.numeric(tcl(repee1, "getvalue"))+1)
    test <- unlist(as.numeric(tcl(test, "getvalue"))+1)
    if (test!=3 & mat9[1,1]!=1) tkmessageBox(message="You can only compute Fisher's exact test as you have presence/absence profiles") 
    if (test!=3 & mat9[1,1]!=1) stop("You can only compute Fisher's exact test as you have presence/absence profiles") 
    if (test==3 & mat9[1,1]==1) tkmessageBox(message="You can not compute Fisher's exact test as you have qualitative profiles") 
    if (test==3 & mat9[1,1]==1) stop("You can not compute Fisher's exact test as you have qualitative profiles") 
   
    niv1=levels(fact[,ae])
    niv<<-c(niv1[repee],niv1[repee1])
    a=iterative.test(profil=mat6,fact1=fact[,ae],level=niv,method=test)
    
  }                                                         
  
  tkgrid(tklabel(t2, text = ""))
  tt5<-tkframe(t2)
  b1<- tkbutton(tt5, text = "Compute", command = mm)
  
  close<-function()
  {
    tkdestroy(t2)
  }
  
  b2<-tkbutton(tt5,text="Cancel",command=close)
  
  vis<-function()
  {
    if (a==1) tkmessageBox(message="First press the button Compute")
    if (a==1) stop("First press the button Compute")
    loc = round(locator(1, type = "p", pch = 4)[[1]],digit=0)
    n1<-which(fact[,ae]==niv[1])
    n2<-which(fact[,ae]==niv[2])
    proan<-mat6[c(n1,n2),loc]
    factt<-fact[c(n1,n2),ae]
    factt<-factor(factt)
    x11()
    boxplot(proan~factt,main=paste("Boxplot of the scan ",loc,sep=""),ylab="Signal intensity")
  }
  
  b3<-tkbutton(t2,text="Visualize distribution of a scan",command=vis)
    
  tkpack(b1,b2,side="left")
  tkgrid(tt5)
  tkgrid(tklabel(t2, text = ""))
  tkgrid(b3)
  tkgrid(tklabel(t2, text = ""))
  tkfocus(t2)
}
