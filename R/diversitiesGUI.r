diversitiesGUI <-
function()
{ 
	checkprofile()
	if(sum(mat.binary)!=length(mat.binary))
		tkmessageBox(message="Be careful, profiles transformed in binary. First normalize your profiles to compute diversity indices")
	
	tt <- tktoplevel()
	tkwm.title(tt,"Diversity index estimation")
	tkgrid(tklabel(tt,text="                                                                                           "))
	tkgrid(tklabel(tt,text="Characteristics of the detection of the peaks"))
	
	help.define.peak<-function()
	{
		trr <- tktoplevel()
		tkwm.title(trr,"What is the radius of the rollball")
		tkgrid(tklabel(trr,text=""))
		zzz<-file.path(paste(.libPaths(), "/StatFingerprints/radius.GIF",sep=""))
		icnn<-tkimage.create("photo", file = zzz)
		tcltklab <- tklabel(trr, image = icnn)
		tkgrid(tcltklab)
		tkgrid(tklabel(trr,text=""))	  
		tkgrid(tkbutton(trr,text="Cancel",command=function() tkdestroy(trr)))
		tkgrid(tklabel(trr,text=""))
		
		tr <- tktoplevel()
		tkgrid(tklabel(tr,text="                                                                                                                                   "))
		tkwm.title(tr,"Help to define characteristics of peak detection")
		tkgrid(tklabel(tr,text="Characteristics of the detection of the peaks"))
		
		t1<-tkframe(tr)
		text1<-tklabel(t1,text="Radius of the rollball?")
		s1 <- tclVar("10")
		slider1 <- tkentry(t1,width=8,textvariable=s1)
		tkpack(text1,slider1,side="left")
		tkgrid(t1)
		
		t2<-tkframe(tr)
		text2<-tklabel(t2,text="Delete peaks with height under (per mille)")
		s2 <- tclVar("5")
		slider2 <- tkentry(t2,width=8,textvariable=s2)
		tkpack(text2,slider2,side="left")
		tkgrid(t2)
		
		t3<-tkframe(tr)
		text3<-tklabel(t3,text="Wide peak area")
		s3 <- tclVar("8")
		slider3 <- tkentry(t3,width=8,textvariable=s3)
		tkpack(text3,slider3,side="left")
		tkgrid(t3)
		
		t4<-tkframe(tr)
		text4<-tklabel(t4,text="Interval size (in scans)")
		s4 <- tclVar("5")
		slider4 <- tkentry(t4,width=8,textvariable=s4)
		tkpack(text4,slider4,side="left")
		tkgrid(t4)
		
		tkgrid(tklabel(tr,text="   "))
		scr <- tkscrollbar(tr, repeatinterval=5,command=function(...)tkyview(tl,...))
		tl<-tklistbox(tr,height=10,width =50 ,selectmode="single",yscrollcommand=function(...)tkset(scr,...),background="white")
		tkgrid(tklabel(tr,text="Which profile do you want to plot?"))
		tkgrid(tklabel(tr,text=""))
		tkgrid(tl,scr)
		tkgrid.configure(scr,rowspan=10,sticky="nsw")
		prof <- rownames(mat.raw$profil)
		
		for (i in 1:length(prof)) 
		{
			tkinsert(tl,"end",prof[i])
		}
		tkselection.set(tl,0)
		
		count.peaks<-function()
		{
			radius	=as.numeric(tclvalue(s1))
			lim		=as.numeric(tclvalue(s2))
			digit		=as.numeric(tclvalue(s3))
			int		=as.numeric(tclvalue(s4))
			sel		<- as.numeric(tkcurselection(tl))+1 
			peakparameters(prof=mat.analyse[sel,],radius=radius,int=int,lim=lim,digit=digit)
		}
		
		tkgrid(tklabel(tr,text=""))
		tr1<-tkframe(tr)
		b1<-tkbutton(tr1,text="plot",command=count.peaks)
		b2<-tkbutton(tr1,text="new windows",command=function() x11())    
		b3<-tkbutton(tr1,text="Cancel",command=function() tkdestroy(tr))
		tkpack(b1,b2,b3,side="left")
		tkgrid(tr1)
		tkgrid(tklabel(tr,text=""))
		tkfocus(tr)
	}
	
	compute.diversity<-function()
	{
		radius=as.numeric(tclvalue(s1))
		lim=as.numeric(tclvalue(s2))
		digit=as.numeric(tclvalue(s3))
		int=as.numeric(tclvalue(s4))
		sep1<-unlist(as.numeric(tcl(sepe,"getvalue"))+1 )
		
		if(sep1==1) method<-"high of peaks"
		if(sep1==2) method<-"area"
		div<-1
		div<-diversities(mat=mat.analyse,radius=radius,int=int,lim=lim,digit=digit,method=method)
		div<<-as.matrix(div)
		tkmessageBox(message="Diversity indices successfully estimated")
		dev.off()
	}
	
	### Export diversities index
	
	export.diversity<-function()
	{
		fileName<-tclvalue(tkgetSaveFile())
		filename<-paste(fileName,".csv",sep="")
		nomscol=c("Sample",colnames(div))
		write.table(t(nomscol),file=filename,sep=";",dec=".",row.names=F,col.names=F)
		write.table(div,file=filename,sep=";",dec=".", col.names=F,append=T)
		tkmessageBox(message="Diversity indices successfully exported")
	}
	
	
	tkgrid(tkbutton(tt,text="Help to define characteristics of peak detection",command=help.define.peak))
	
	t1<-tkframe(tt)
	text1<-tklabel(t1,text="Radius of the rollball?")
	s1 <- tclVar("10")
	slider1 <- tkentry(t1,width=8,textvariable=s1)
	tkpack(text1,slider1,side="left")
	tkgrid(t1)
	
	t2<-tkframe(tt)
	text2<-tklabel(t2,text="Delete peaks with height under (per mille)")
	s2 <- tclVar("5")
	slider2 <- tkentry(t2,width=8,textvariable=s2)
	tkpack(text2,slider2,side="left")
	tkgrid(t2)
	
	t3<-tkframe(tt)
	text3<-tklabel(t3,text="Wide peak area")
	s3 <- tclVar("8")
	slider3 <- tkentry(t3,width=8,textvariable=s3)
	tkpack(text3,slider3,side="left")
	tkgrid(t3)
	
	t4<-tkframe(tt)
	text4<-tklabel(t4,text="Interval size (in scans)")
	s4 <- tclVar("5")
	slider4 <- tkentry(t4,width=8,textvariable=s4)
	tkpack(text4,slider4,side="left")
	tkgrid(t4)
	
	t5<-tkframe(tt)  
	text5<-tklabel(t5,text="Method of calculation")
	sep<-c("Use height of peaks","use area under peaks")
	sepe<-tkwidget(t5,"ComboBox",editable=FALSE,values=sep,height=2)
	tkpack(text5,sepe,side="left")
	tkgrid(t5)
	
	tkgrid(tklabel(tt,text=""))
	
	tkgrid(tklabel(tt,text=""))
	tkgrid(tklabel(tt,text="Indexes calculated are* :"))
	tkgrid(tklabel(tt,text="        Number of peaks"))
	tkgrid(tklabel(tt,text="        -log Simpson"))
	tkgrid(tklabel(tt,text="        1-simpson"))
	tkgrid(tklabel(tt,text="        Shannon"))
	tkgrid(tklabel(tt,text="        Buzas & Gibson's evenness"))
	tkgrid(tklabel(tt,text="        Equitability"))
	tkgrid(tklabel(tt,text="        Background area"))
	tkgrid(tklabel(tt,text="*Only peak number index is calculated when you have presence/absence data",font="arial 7"))
	tkgrid(tklabel(tt,text=""))
	tkgrid(tkbutton(tt,text="Calculate diversity index",command=compute.diversity))
	tkgrid(tklabel(tt,text=""))
	tkgrid(tkbutton(tt,text="Export calculated diversity indexes",command=export.diversity))  
	tkgrid(tklabel(tt,text=""))
	tkgrid(tkbutton(tt,text="Cancel",command=function() tkdestroy(tt)))
	tkgrid(tklabel(tt,text="")) 
	tkfocus(tt)
	
}

