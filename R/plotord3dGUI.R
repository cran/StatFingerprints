######################################################
#    Plot explorative statistic graph saved in 3D    #
######################################################

"plotord3dGUI"<-function()
{
  require(rgl) || stop("rgl library not available")
  fil=if (interactive()) choose.files(filters = Filters["All",])
  
  z=load(file=fil)

  tt<-tktoplevel()
  tkwm.title(tt, "Plot ordination in 3 dimensions")
  tkgrid(tklabel(tt,text="                                                                                                                           "))
  tkgrid(tklabel(tt,text="Select the axes"))  
  
  t1<-tkframe(tt)
  SliderValue1 <- tclVar("1")
  SliderValueLabel1 <- tklabel(t1, text = as.character(tclvalue(SliderValue1)))
  text1<-tklabel(t1, text = "First axis : ")
  tkconfigure(SliderValueLabel1, textvariable = SliderValue1)
  slider1 <- tkscale(t1, from = 1, to = 5, showvalue = FALSE,
  variable = SliderValue1, resolution = 1, orient = "horizontal")
  tkpack(text1, slider1 ,SliderValueLabel1 ,side="left")
  tkgrid(t1)

  t2<-tkframe(tt)
  SliderValue2 <- tclVar("2")
  SliderValueLabel2 <- tklabel(t2, text = as.character(tclvalue(SliderValue2)))
  text2<-tklabel(t2, text = "Second axis : ")
  tkconfigure(SliderValueLabel2, textvariable = SliderValue2)
  slider2 <- tkscale(t2, from = 1, to = 5, showvalue = FALSE,
  variable = SliderValue2, resolution = 1, orient = "horizontal")
  tkpack(text2, slider2 ,SliderValueLabel2,side="left")
  tkgrid(t2)

  t3<-tkframe(tt)
  SliderValue3 <- tclVar("3")
  SliderValueLabel3 <- tklabel(t3, text = as.character(tclvalue(SliderValue3)))
  text3<-tklabel(t3, text = "Third axis : ")
  tkconfigure(SliderValueLabel3, textvariable = SliderValue3)
  slider3 <- tkscale(t3, from = 1, to = 5, showvalue = FALSE,
  variable = SliderValue3, resolution = 1, orient = "horizontal")
  tkpack(text3, slider3 ,SliderValueLabel3,side="left")
  tkgrid(t3)
    
  t4<-tkframe(tt)
  text4<-tklabel(t4, text = "Return label of the points")
  repe <- c("Yes", "No")
  repee <- tkwidget(t4, "ComboBox", editable = FALSE, values = repe,height=2)
  tkpack(text4,repee,side="left")
  tkgrid(t4)
    
  t5<-tkframe(tt)
  text5<-tklabel(t5, text = "Plot each point according to qualitative variable")
  ff=c("none",names(fact))
  repz1 <- tkwidget(t5, "ComboBox", editable = FALSE, values = ff,height=length(ff))
  tkpack(text5,repz1,side="left")
  tkgrid(t5)
 
  t6<-tkframe(tt)
  text6<-tklabel(t6, text = "Color if no factor selected")
  fff=c("black","red","green","blue","sligth blue","pink","yellow","grey")
  repzz <- tkwidget(t6, "ComboBox", editable = FALSE, values = fff,height=length(fff))
  tkpack(text6,repzz,side="left")
  tkgrid(t6)
  tkgrid(tklabel(tt,text=" "))
  cal <- function()
  {
    repe1 <- unlist(as.numeric(tcl(repee, "getvalue")) + 1)
    if (z=="bestnmds") ord=bestnmds$points
    if (z=="pcaf") ord=pcaf$x
    if (dim(ord)[2]<=2) tkmessageBox(message=paste("You have only ",dim(ord)[2],"dimensions. Please compute another nMDS with  or more dimensions.",sep=""),icon="error",type="ok")
    if (dim(ord)[2]<=2) stop(paste("You have only ",dim(ord)[2],"dimensions",sep=""))
    if (dim(ord)[2]<=2) tkdestroy(tt)
    d1 <- as.numeric(as.character(tclvalue(SliderValue1)))
    d2 <- as.numeric(as.character(tclvalue(SliderValue2)))
    d3 <- as.numeric(as.character(tclvalue(SliderValue3)))
    colo<- unlist(as.numeric(tcl(repzz, "getvalue")) +1)
    facte <- unlist(as.numeric(tcl(repz1, "getvalue")) )
    if (facte==0 & z=="bestnmds") plot3d(ord[,d1],ord[,d2],ord[,d3],type="s",size=0.5,col=colo,xlab="1st PC",ylab="2nd PC", zlab="3rd PC",sub=paste("Stress=",c(round(bestnmds$stress,digit=3))),main="Non-metric multidimensional scaling")
    if (facte==0 & z!="bestnmds") plot3d(ord[,d1],ord[,d2],ord[,d3],type="s",size=0.5,col=colo,xlab="1st PC",ylab="2nd PC", zlab="3rd PC",main="Principal components analysis")
    if (facte!=0 & z=="bestnmds") plot3d(ord[,d1],ord[,d2],ord[,d3],type="s",size=0.5,col=as.numeric(fact[,facte]),xlab="1st PC",ylab="2nd PC", zlab="3rd PC",sub=paste("Stress=",c(round(bestnmds$stress,digit=3))),main="Non-metric multidimensional scaling")
    if (facte!=0 & z!="bestnmds") plot3d(ord[,d1],ord[,d2],ord[,d3],type="s",size=0.5,col=as.numeric(fact[,facte]),xlab="1st PC",ylab="2nd PC", zlab="3rd PC",main="Non-metric multidimensional scaling")
    if (facte!=0) plot(1,1,col="white",xlab=NA,ylab=NA,xaxt="n",yaxt="n",axes=FALSE)
    if (facte!=0) legend("topleft",title="Legend of the 3-dimensional plot",legend=c(levels(fact[,facte])),col=c(1:length((levels(fact[,facte])))),pch=rep(16,length(levels(fact[,facte]))))
    if (repe1==1) text3d(ord[,d1],ord[,d2],ord[,d3],rownames(ord))
  }

  nw<-function()
  {
    open3d()
  }

  t7<-tkframe(tt)
  b1 <- tkbutton(t7, text = "Plot", command = cal)
  b3<- tkbutton(t7, text = "New plot window...", command = nw)
  
  ss<-function()
  {
    fileName<-tclvalue(tkgetSaveFile())
    filename<-paste(fileName,".png",sep="")
    if (filename == "")
    return()
    rgl.snapshot(filename=filename, fmt="png", top=TRUE )
  }
  
  b2<-tkbutton(t7,text="Save picture",command=ss)
  
  close<-function()
  {
    tkdestroy(tt)
    }

  b4<-tkbutton(t7,text="Cancel",command=close)
  tkpack(b1,b2,b3,b4,side="left")
  tkgrid(t7)
  tkgrid(tklabel(tt,text=""))
  tkfocus(tt)
}


