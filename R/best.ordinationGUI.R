#######################################################
#    GUI for the "Comparison of PCA/nMDS" analysis    #
#######################################################

"best.ordinationGUI"<-function()
{
  checkprofile()
  
  tt<-tktoplevel()
  tkwm.title(tt,"Compare PCA vs nMDS with Euclidean metric")
  tkgrid(tklabel(tt,text="                                                                                                                              "))
  tkgrid(tklabel(tt,text="Parameters of the plot"))  

  tt1<-tkframe(tt) 
  text1<-tklabel(tt1,text="Return plot of PCA and nMDS?")
  repe<-c("Yes","No")
  repee<- tkwidget(tt1,"ComboBox",editable=FALSE,values=repe,width=8,height=2)
  tkpack(text1,repee,side="left")
  tkgrid(tt1)

  tt2<-tkframe(tt) 
  text2<-tklabel(tt2,text="Return label of the plot of ordinations?")
  tex<-c("Yes","No")
  texte<- tkwidget(tt2,"ComboBox",editable=FALSE,values=tex,width=8,height=2)
  tkpack(text2,texte,side="left")
  tkgrid(tt2)

  tkgrid(tklabel(tt,text="")) 
                        
  mm<-function()
  {
    repe1<-unlist(as.numeric(tcl(repee,"getvalue"))+1)
    if (repe1==1) 
      repe<-TRUE
    if(repe1==2)
      repe<-FALSE
    tex1<-unlist(as.numeric(tcl(texte,"getvalue"))+1)
    if(tex1==1) 
      tex<-TRUE
    if(tex1==2)
      tex<-FALSE
    best.ordination(mat=mat6, return.ordination=repe,text=tex)
  }

  t1<-tkframe(tt)
  b1<-tkbutton(t1,text="Compare PCA/nMDS",command=mm)
  
  close<-function()
  {
    tkdestroy(tt)
  }  
  
  b2<-tkbutton(t1,text="Cancel",command=close)
  tkpack(b1,b2,side="left")
  tkgrid(t1)
  tkgrid(tklabel(tt,text="   "))
  tkfocus(tt)
}
