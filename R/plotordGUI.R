######################################################
#    Plot explorative statistic graph saved in 2D    #
######################################################

"plotordGUI"<-function()
{
  fil=if (interactive()) choose.files(filters = Filters["All",])

  z=load(file=fil)

  tt <- tktoplevel()
  tkwm.title(tt, "Plot ordination in 2 dimensions")
  tkgrid(tklabel(tt,text="                                                                                         "))
  tkgrid(tklabel(tt,text="Select the axes"))  
  
  t1<-tkframe(tt)
  SliderValue1 <- tclVar("1")
  SliderValueLabel1 <- tklabel(t1, text = as.character(tclvalue(SliderValue1)))
  text1<-tklabel(t1, text = "First axis : ")
  tkconfigure(SliderValueLabel1, textvariable = SliderValue1)
  slider1 <- tkscale(t1, from = 1, to = 5, showvalue = FALSE,
  variable = SliderValue1, resolution = 1, orient = "horizontal")
  tkpack(text1, slider1,SliderValueLabel1,side="left")
  tkgrid(t1)

  t2<-tkframe(tt)
  SliderValue2 <- tclVar("2")
  SliderValueLabel2 <- tklabel(t2, text = as.character(tclvalue(SliderValue2)))
  text2<-tklabel(t2, text = "Second axis : ")
  tkconfigure(SliderValueLabel2, textvariable = SliderValue2)
  slider2 <- tkscale(t2, from = 1, to = 5, showvalue = FALSE,
  variable = SliderValue2, resolution = 1, orient = "horizontal")
  tkpack(text2, slider2,SliderValueLabel2,side="left")
  tkgrid(t2)
  
  t3<-tkframe(tt)
  text3<-tklabel(t3, text = "Return label of the points")
  repe <- c("Yes", "No")
  repee <- tkwidget(t3, "ComboBox", editable = FALSE, values = repe,height=2)
  tkpack(text3,repee,side="left")
  tkgrid(t3)
  
  tkgrid(tklabel(tt,text="")) 
  t4<-tkframe(tt)
  text4<-tklabel(t4, text = "Plot each point according to qualitative variable")
  ff=c("none",names(fact))
  repz <- tkwidget(t4, "ComboBox", editable = FALSE, values = ff,height=length(ff))
  tkpack(text4,repz,side="left")
  tkgrid(t4)
  
  t5<-tkframe(tt)  
  text5<-tklabel(t5, text = "Plot with contour lines of correlation according to a quantitative variable")
  fff=c("none",names(param))
  repzz <- tkwidget(t5, "ComboBox", editable = FALSE, values = fff,height=length(fff))
  tkpack(text5,repzz,side="left")
  tkgrid(t5)
  
  cal <- function()
  {
    repe1 <- unlist(as.numeric(tcl(repee, "getvalue")) +1)
    if (z=="bestnmds") ord=bestnmds$points
    if (z=="pcaf") ord=pcaf$x
    d1 <- as.numeric(as.character(tclvalue(SliderValue1)))
    d2 <- as.numeric(as.character(tclvalue(SliderValue2)))
    facte <- unlist(as.numeric(tcl(repz, "getvalue")) )
    parame<- unlist(as.numeric(tcl(repzz, "getvalue")) )
    if (z=="bestnmds" & facte==0) plot(ord[,d1],ord[,d2],xlab=NA,ylab=NA,xaxt="n",yaxt="n",main="Non-metric multidimensional scaling",sub=paste("Stress=",c(round(bestnmds$stress,digit=3))))
    if (z=="bestnmds" & facte!=0) plot(ord[,d1],ord[,d2],xlab=NA,ylab=NA,xaxt="n",yaxt="n",col=as.numeric(fact[,facte]),pch=as.numeric(fact[,facte]),main="Non-metric multidimensional scaling",sub=paste("Stress=",c(round(bestnmds$stress,digit=3))))
    if (z=="pcaf" & facte!=0) plot(ord[,d1],ord[,d2],xlab=NA,ylab=NA,xaxt="n",yaxt="n",col=as.numeric(fact[,facte]),pch=as.numeric(fact[,facte]),main="Principal components analysis")
    if (z=="pcaf" & facte==0) plot(ord[,d1],ord[,d2],xlab=NA,ylab=NA,xaxt="n",yaxt="n",main="Principal components analysis")
    if (repe1==1) text(ord[,d1],ord[,d2],rownames(ord))
    if (parame!=0)  glm1<- glm(param[,parame]~ord[,d1]+I(ord[,d1]^2)+ord[,d1]*ord[,d2]+ord[,d2]+I(ord[,d2]^2),na.action=na.omit)
    if (parame!=0) contour(interp(ord[,d1],ord[,d2],fitted(glm1)),add=TRUE,col="black",lty=3,labcex=1.2,font=3,lwd=1.2,nlevels=5)
    if (facte!=0) legend("topleft",legend=c(levels(fact[,facte])),col=c(1:length((levels(fact[,facte])))),pch=c(1:length(levels(fact[,facte]))))
  tkdestroy(tt)
  }

  nw<-function()
  {
    x11()
  }
  
  tkgrid(tklabel(tt,text=""))
  t6<-tkframe(tt)  
  b1 <- tkbutton(t6, text = "Plot", command = cal)
  b2<- tkbutton(t6, text = "New plot window...", command = nw)
  
  close<-function()
  {
    tkdestroy(tt)
  }
  
  b3<-tkbutton(t6,text="Cancel",command=close)
  tkpack(b1,b2,b3,side="left")
  tkgrid(t6)
  tkgrid(tklabel(tt,text=""))
  tkfocus(tt)
}

nmds.contour <- function (x,fact, nbcont=5,text=TRUE)
{
  plot(x$points[,1],x$points[,2])
  glm1<- glm(fact~x$points[,1]+I(x$points[,1]^2)+x$points[,1]*x$points[,2]+x$points[,2]+I(x$points[,2]^2),na.action=na.omit)
  contour(interp(x$points[,1],x$points[,2],fitted(glm1)),add=TRUE,col="black",lty=3,labcex=1.2,font=3,lwd=1.2,nlevels=nbcont)
  a=vector(length=2)
  a[1]=min(x$points[,1])
  a[2]=min(x$points[,2])
  j=mean(x$points[,1])
  j=j-a[1]
  j=j/2
  j=j+a[1]
  
  coeff.deter=function(x)
  {
    a=1-(x$deviance/x$null.deviance)
    return(a)
  }

  r=coeff.deter(glm1)
  r=round(r,digit=3)
  text(j,a[2],paste("R2 =",r))
  if(text==TRUE) (text(x$points[,1],x$points[,2],rownames(x$points),pos=2))
}

