########################################################
#    GUI for the "define your own standard" function   #
########################################################

"roxnewGUI"<-function()
{
  tm <- tktoplevel()
  tkwm.title(tm, "Define peaks using your own standard")
  vec<-0
  run <- function()
  {
  
####    Enter your peaks
  
    vec <- tclvalue(tkget(txt,"0.0","end"))
    a<-strsplit(vec,",")[[1]]
    a[length(a)]<-substr(a[length(a)],1,(nchar(a[length(a)])-1))
    a=as.numeric(a)
    roxref<<-sort(a)
    
####    Select range peaks
    
    defroxref<-function(roxref)
    {
      plot(0,0,col="white",xlim=c(0,roxref[length(roxref)]),ylim=c(0,1),main="Reference standard")
      abline(v=roxref,col="red",main="Reference standard")
      loca=locator(2,type="p",pch=4) 
      x1<-round(loca$x[1])
      x2<-round(loca$x[2])
      for (i in 1:length(roxref))
      {
        if (x1>roxref[i])  x11<-c(1:length(roxref))[i+1]
      }
      for (i in 1:length(roxref))
      {
        if (x2>roxref[i]) x22<-c(1:length(roxref))[i]
      }
      roxref=roxref[x11:x22]
      abline(v=roxref,col="green",lty=3)
      roxref=roxref-roxref[1]+25
      return(roxref)
      tkfocus(MainMenu)
      
    }
    
    r<-defroxref(roxref)
    rxref<<-r
    print("Reference standard successfully defined")
    print(rxref)
    tkdestroy(tm)
  }

####  Help to define peaks

  hhe<-function()
  {
    plot(1:dim(mat$rox)[2],mat$rox[1,],type="l",col="red",xaxt="n",yaxt="n",ylab=NA,xlab=NA,main="Left click to select point, rigth click to get their values")
    loc<-locator(type="p",pch=4)
    loc<-round((loc$x),digit=0)
    tkgrid(tklabel(tm,text="values are"))
    tkgrid(tklabel(tm,text=paste(loc,",",sep="")))
    tkinsert(txt,"end",paste(loc,",",sep=""))
  }  
  
  tkgrid(tklabel(tm,text=""))
  d1<-tkframe(tm)
  a1<-(tkbutton(d1,text="Define reference standard",command=run))
  a2<-(tkbutton(d1,text="Help to define peaks of reference standard",command=hhe))
  tkpack(a1,a2,side="left")
  tkgrid(d1)
  tkgrid(tklabel(tm,text=""))
  txt<-tktext(tm, height=5)
  tkgrid(txt)
  tkfocus(tm) 
}