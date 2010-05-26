plotordGUI <-
function()
{

  fil=if (interactive()) choose.files(filters = Filters["All",])

  z=load(file=fil)
### DEBUT  -> CA ca marche pas pb avec div
  #if(sum(param_temp[1,],na.rm=TRUE)!=length(param_temp[1,]) & sum(div[1,],na.rm=TRUE)!=length(div[1,]))
  #{
         # param2<-matrix(nr=dim(mat.analyse)[1],nc=dim(param_temp)[2]+7)
          #param2<-cbind(div,param_temp)
  #}
  
 # if(sum(param_temp[1,],na.rm=TRUE)!=length(param_temp[1,]) & sum(div[1,],na.rm=TRUE)==length(div[1,]))
  #{
          param2<-matrix(nr=dim(mat.analyse)[1],nc=dim(param_temp)[2])
          param2<-param_temp
  #}
###FIN -> CA ca marche pas pb avec div  
  
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
  ff=c("none",names(fact_temp))
  repz <- tkwidget(t4, "ComboBox", editable = FALSE, values = ff,height=length(ff))
  tkpack(text4,repz,side="left")
  tkgrid(t4)
  
  t5<-tkframe(tt)  
  text5<-tklabel(t5, text = "Plot with contour lines of correlation according to a quantitative variable")
  fff=c("none",names(param2))
  repzz <- tkwidget(t5, "ComboBox", editable = FALSE, values = fff,height=length(fff))
  tkpack(text5,repzz,side="left")
  tkgrid(t5)
  
  plotord <- function()
  {
    repe1 <- unlist(as.numeric(tcl(repee, "getvalue")) +1)
    if (z=="bestnmds") ord=bestnmds$points
    if (z=="pcaf") ord=pcaf$x
    d1 <- as.numeric(as.character(tclvalue(SliderValue1)))
    d2 <- as.numeric(as.character(tclvalue(SliderValue2)))
    facte <- unlist(as.numeric(tcl(repz, "getvalue")) )
    colo<<-as.numeric(fact[,facte])
    pche<<- as.numeric(fact[,facte])
	
	zz<-function()
	{
		colo[colo==1]<-11
		colo[colo==2]<-22
		colo[colo==3]<-33
		
		colo[colo==11]<-3
		colo[colo==22]<-1    
		colo[colo==33]<-2  
		pche<<-colo
		colo<<-colo
	}
	zz()
	
    parame<- unlist(as.numeric(tcl(repzz, "getvalue")) )
    if (z=="bestnmds" & facte==0) plot(ord[,d1],ord[,d2],xlab=NA,ylab=NA,xaxt="n",yaxt="n",main="Non-metric multidimensional scaling",sub=paste("Stress=",c(round(bestnmds$stress,digit=3))))
    if (z=="bestnmds" & facte!=0) plot(ord[,d1],ord[,d2],xlab=NA,ylab=NA,xaxt="n",yaxt="n",col=colo, pch=pche, main="Non-metric multidimensional scaling",sub=paste("Stress=",c(round(bestnmds$stress,digit=3))))
    if (z=="pcaf" & facte!=0) plot(ord[,d1],ord[,d2],xlab="1st PC",ylab="2nd PC",col=as.numeric(fact[,facte]),pch=as.numeric(fact[,facte]),main="Principal components analysis")
    if (z=="pcaf" & facte==0) plot(ord[,d1],ord[,d2],xlab="1st PC",ylab="2nd PC",main="Principal components analysis")
    if (repe1==1) pointLabel(ord[,d1],ord[,d2],rownames(ord),offset = 0, cex = 0.7) 
    if (parame>0)  glm1<- glm(param2[,parame]~ord[,d1]+I(ord[,d1]^2)+ord[,d1]*ord[,d2]+ord[,d2]+I(ord[,d2]^2),na.action=na.omit)
    if (parame>0) contour(interp(ord[,d1],ord[,d2],fitted(glm1)),add=TRUE,col="black",lty=3,labcex=1.2,font=3,lwd=1.2,nlevels=5)
    if (facte!=0) legend("topleft",legend=c(levels(fact[,facte])),col=c(1:length((levels(fact[,facte])))),pch=c(1:length(levels(fact[,facte]))))
    tkdestroy(tt)
  }

  new.window<-function()
  {
    x11()
  }
  
  tkgrid(tklabel(tt,text=""))
  t6<-tkframe(tt)  
  b1 <- tkbutton(t6, text = "Plot", command = plotord)
  b2<- tkbutton(t6, text = "New plot window...", command = new.window)
  b3<-tkbutton(t6,text="Cancel",command=function() tkdestroy(tt))
  tkpack(b1,b2,b3,side="left")
  tkgrid(t6)
  tkgrid(tklabel(tt,text=""))
  tkfocus(tt)
  }

