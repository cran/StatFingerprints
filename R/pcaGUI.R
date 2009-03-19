######################
#    PCA function    #
######################

"pcaGUI"<-function ()
{ 
  checkprofile()
  
  tt <- tktoplevel()
  tkwm.title(tt, "Compute Principal Components Analysis")
  tkgrid(tklabel(tt,text="                                                                                                                   "))

  tkgrid(tklabel(tt,text="Parameters to compute PCA"))
  tt1<-tkframe(tt)
  text1<-tklabel(tt1, text = "PCA centred?")
  repe <- c("Yes", "No")
  cent <- tkwidget(tt1, "ComboBox", editable = FALSE, values = repe,width=8,height=2)
  tkpack(text1,cent,side="left")
  tkgrid(tt1)

  tt2<-tkframe(tt)
  text2<-tklabel(tt2, text = "PCA scaled?")
  repe <- c("Yes", "No")
  scal <- tkwidget(tt2, "ComboBox", editable = FALSE, values = repe,width=8,height=2)
  tkpack(text2,scal,side="left")
  tkgrid(tt2)
  
  tkgrid(tklabel(tt,text="  "))
  tkgrid(tklabel(tt,text="Parameters of the 2 dimensional PCA plot")) 

####    Plot PCA
  
  plotd<-function()
  {
    plot(prcomp(x=mat6))
  }
  
  plotd<- tkbutton(tt, text = "Plot proportion of the principal components", command = plotd)
  tkgrid(plotd)
  tt3<-tkframe(tt)
  SliderValue1 <- tclVar("1")
  SliderValueLabel1 <- tklabel(tt3, text = as.character(tclvalue(SliderValue1)))
  text3<-tklabel(tt3, text = "First axis : ")
  tkconfigure(SliderValueLabel1, textvariable = SliderValue1)
  slider1 <- tkscale(tt3, from = 1, to = 5, showvalue = FALSE, variable = SliderValue1, resolution = 1, orient = "horizontal")
  tkpack(text3,slider1,SliderValueLabel1,side="left") 
  tkgrid(tt3) 

  tt4<-tkframe(tt)    
  SliderValue2 <- tclVar("2")
  SliderValueLabel2 <- tklabel(tt4, text = as.character(tclvalue(SliderValue2)))
  text4<-tklabel(tt4, text = "Second axis : ")
  tkconfigure(SliderValueLabel2, textvariable = SliderValue2)
  slider2 <- tkscale(tt4, from = 1, to = 5, showvalue = FALSE, variable = SliderValue2, resolution = 1, orient = "horizontal")
  tkpack(text4,slider2,SliderValueLabel2,side="left") 
  tkgrid(tt4)
    
  tt5<-tkframe(tt) 
  text5<-tklabel(tt5, text = "Return label")
  repe <- c("Yes", "No")
  repee <- tkwidget(tt5, "ComboBox", editable = FALSE, values = repe,width=8,height=2)
  tkpack(text5,repee,side="left")
  tkgrid(tt5)
  
  tkgrid(tklabel(tt,text=""))
 
  cal <- function() 
  {
    repe1 <- unlist(as.numeric(tcl(repee, "getvalue")) + 1)
    d1 <- as.numeric(as.character(tclvalue(SliderValue1)))
    d2 <- as.numeric(as.character(tclvalue(SliderValue2)))
    centr <- unlist(as.numeric(tcl(cent, "getvalue")) + 1)
    if (centr==1) centre<-TRUE
    if (centr==2) centre<-FALSE
    scalee <- unlist(as.numeric(tcl(scal, "getvalue")) + 1)
    if (scalee==1) scal2<-TRUE
    if (scalee==2) scal2<-FALSE
    
    pca<- prcomp(x=mat6,center=centre,scale=scal2)
    pcaf<<-pca
    plot(pca$x[,d1],pca$x[,d2],xlab="1st PC",ylab="2nd PC",main="Principal Components Analysis")
    if (repe1==1) 
      text(pca$x[,d1],pca$x[,d2],rownames(pca$x))
    tkmessageBox(message="You can improve your PCA plot: save the PCA and use Plot saved nMDS function in the Plot menu") 
  }
  
  t10<-tkframe(tt)
  b1 <- tkbutton(t10, text = "Plot the PCA", command = cal)
  
  save.pca<-function()  
  {
    fileName<-tclvalue(tkgetSaveFile())
    filename<-paste(fileName,".Rdata",sep="")
    save(pcaf,file=filename)
    print("Your PCA has been successfully saved")
    tkdestroy(tt)
  }

  b2<-tkbutton(t10,text="Save the PCA",command=save.pca)
  
  close<-function()
  {
    tkdestroy(tt)
  }
  
  b3<-tkbutton(t10,text="Cancel",command=close)
  tkpack(b1,b2,b3,side="left")
  tkgrid(t10)
  tkgrid(tklabel(tt,text="   "))
  tkgrid(tklabel(tt,text="Note: advanced tools to plot PCA is available on the Plot menu"))
  tkfocus(tt)
}
