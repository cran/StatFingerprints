roxdefault <-
function()
{
	####    ROX HD400 - 21 peaks
	{
		for (i in c(1:length(.libPaths()))) 
		{
			roxfilepath=paste(.libPaths()[i],"/StatFingerprints", sep="")
			if (length(which(dir(roxfilepath)=="Rox.ref"))!=0)
			{
				roxfilename=paste(roxfilepath,"/Rox.ref",sep="")
				roxdata=read.table(roxfilename,sep=",")
			}
		}
		if (exists("roxdata")) roxref=as.numeric(roxdata)
		if (!exists("roxdata")) 
		{
			tkmessageBox(message="Unable to found the file 'Rox.ref' in the directory of the StatFingerprints library. Please check !",icon="warning", type="ok")
			stop()
#			tt3 <- tktoplevel()
#			tkwm.title(tt3,"Error: File 'Rox.ref' not found!")
#			label.widget <- tklabel(tt3, text="Unable to found the file 'Rox.ref' in the directory of the StatFingerprints library. Please check !")
#			button.widget <- tkbutton(tt3, text="OK", command=function()tkdestroy(tt3))
#			tkpack(label.widget, button.widget)
		} 
		####  roxref=c(4907,5214,6053,6293,7249,7681,8207,8588,8964,9298,9921,10432,10734,11124,11800,12119,12693,13095,13475,15298,17348)
		roxref<<-roxref
		define.rox.ref<-function(roxref)
		{	
			####    Select range peaks        
			
			plot(0,0,col="white",xlim=c(0,roxref[length(roxref)]),ylim=c(0,1),main="Click before the first and after the last peaks used to align",xlab="",ylab="")
			abline(v=roxref,col="red",main="rox ref")
			loca=locator(2,type="p",pch=4) 
			roxref1<-roxref
			loca<-sort(loca$x)
			x1<-round(loca[1])
			x2<-round(loca[2])
			
			x11<-which(roxref>x1)
			x11<-min(x11)
			x22<-which(roxref<x2)
			x22<-max(x22)
			
			roxref=roxref[x11:x22]
			plot(0,0,col="white",xlim=c(0,roxref1[length(roxref1)]),ylim=c(0,1),main=paste("You have selected ",length(roxref)," peaks"),xlab="",ylab="")
			abline(v=roxref1,col="red",main="rox ref")
			abline(v=roxref,col="green",lty=3)
			roxref=roxref-roxref[1]+25
			return(roxref)
		}
		
		if (roxref!=0)
		{
			r<-define.rox.ref(roxref)
			rxref<<-r
			print("Reference standard successfully defined")
			print(rxref)
		}
		tkmessageBox(message=paste("You have selected ",length(rxref)," peaks"))
		dev.off()
		tkfocus(MainMenu)
		
	}
}

