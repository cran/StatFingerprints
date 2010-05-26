roxnewGUI <-
function()
{
	tm <- tktoplevel()
	tkwm.title(tm, "Define peaks using your own standard")
	vec<-0
	define.new.rox <- function()
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
			x11<-which(roxref>x1)
			x22<-which(roxref<x2)
			roxref<-roxref[intersect(x11,x22)]
			abline(v=roxref,col="green",lty=3)
			roxref=roxref-roxref[1]+25
			return(roxref)
			tkfocus(MainMenu)
		}
		
		r<-defroxref(roxref)
		rxref<<-r
		print("Reference standard successfully defined")
		print(roxref)
		tkdestroy(tm)
		
		tkmessageBox(message=paste("Your reference standard has been defined with",length(rxref)," peaks"))
		dev.off()
	}
	
	####  Help to define peaks
	
	help.define.new.rox<-function()
	{
		
		rox=mat.raw$rox[1,]
		profil=mat.raw$profil[1,]
		
		#### Zoom 
		
		tkmessageBox(message="Zoom area to select rox peaks")
		plot(1:length(profil),profil,type="l",col="blue",xlab="",ylab="",lty=2)
		par(new=T)
		plot(1:length(rox),rox,type="l",col="red",xlab="Rox profile",ylab="Intensity")
		y1=locator(1,type="p",pch=4)
		marge<-y1$x
		segments(y1$x,y1$y,length(rox),y1$y)
		segments(y1$x,y1$y,y1$x,min(rox))
		y2=locator(1,type="p",pch=4)
		segments(1,y2$y,y2$x,y2$y)
		segments(y2$x,y2$y,y2$x,max(rox))
		
		#### Peaks selection
		
		tkmessageBox(message="Select rox peaks")
		plot(y1$x:y2$x,profil[y1$x:y2$x],lty=2,type="l",pch=0.1,col="blue",ylab="",xlab="")
		par(new="T")
		plot(y1$x:y2$x,rox[y1$x:y2$x],ylim=c(y2$y-1,y1$y+1),type="l",col="red",ylab="",xlab="")
		loc<-locator(type="p",pch=4)
		loc<-loc
		loc<-round((loc$x),digit=0)
		tkgrid(tklabel(tm,text="values are"))
		tkgrid(tklabel(tm,text=paste(loc,",",sep="")))
		tkinsert(txt,"end",paste(loc,",",sep=""))		
	}  
	
	tkgrid(tklabel(tm,text=""))
	d1<-tkframe(tm)
	a1<-(tkbutton(d1,text="Define reference standard",command=define.new.rox))
	a2<-(tkbutton(d1,text="Help to define peaks of reference standard",command=help.define.new.rox))
	tkpack(a1,a2,side="left")
	tkgrid(d1)
	tkgrid(tklabel(tm,text=""))
	txt<-tktext(tm, height=5)
	tkgrid(txt)
	tkfocus(tm) 
}

