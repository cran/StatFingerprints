#################################################
#    Function to compute simple correlations    #
#################################################

"correlationGUI"<-function()
{
  checkparam()

  tt <- tktoplevel()
  tkwm.title(tt, "Simple correlation of Pearson")
  tkgrid(tklabel(tt, text = ""))
  
  t1<-tkframe(tt)
  text1<-tklabel(t1, text = "First quantitative variable")
  repee <- tkwidget(t1, "ComboBox", editable = FALSE, values = c("Diversity index",names(param)),height=length(c("Diversity index",names(param))))
  tkpack(text1,repee,side="left")
  tkgrid(t1)
 
  t2<-tkframe(tt)
  text2<-tklabel(t2, text = "Second quantitative variable")
  repee1 <- tkwidget(t2, "ComboBox", editable = FALSE, values = c("Diversity index",names(param)),height=length(c("Diversity index",names(param))))
  tkpack(text2,repee1,side="left")
  tkgrid(t2)
  
  mm <- function() 
  {
    facte1 <- unlist(as.numeric(tcl(repee, "getvalue")) )
    facte2 <- unlist(as.numeric(tcl(repee1, "getvalue")))
    if (facte1==0) zz<-formula(div~param[,facte2])
    if (facte2==0) zz<-formula(param[,facte1]~div)
    if (facte1!=0 & facte2!=0) zz<-formula(param[,facte1]~param[,facte2])  
    z<-lm(zz)
    if (facte1==0) print(paste("Equation:  div = ",z$coefficients[[2]]," * ", names(param)[facte2] ," + ",z$coefficients[[1]],sep=""))
    if (facte2==0) print(paste("Equation:", names(param)[facte1] ," = ",z$coefficients[[2]]," * div + ",z$coefficients[[1]],sep=""))
    if (facte1!=0 & facte2!=0) print(paste("Equation:", names(param)[facte1] ," = ",z$coefficients[[2]] ," * ", names(param)[facte2] ," + ",z$coefficients[[1]],sep=""))
    if (facte1==0) z<-cor.test(div,param[,facte2])
    if (facte2==0) z<-cor.test(param[,facte1],div)
    if (facte1!=0 & facte2!=0) z<-cor.test(param[,facte1],param[,facte2]) 
   
    print(paste("p-value:   ",z$p.value))
    print(paste("Pearson r-squared:   ",(z$estimate)^2))  
  }
  
  tkgrid(tklabel(tt, text = ""))
  t3<-tkframe(tt)
  b1<- tkbutton(t3, text = "Compute correlation", command = mm)
  close<-function()
  {
    tkdestroy(tt)
  }

  b2<-tkbutton(t3,text="Cancel",command=close)
  tkpack(b1,b2,side="left")
  tkgrid(t3)
  tkgrid(tklabel(tt, text = ""))
  tkfocus(tt)
}