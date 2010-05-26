convertGUI <-
function()
{
	
	####  Require DataFileConverter (Applied Biosystems)  ####
	
	tkmessageBox(message="This function needs Datafileconverter software (Applied Biosystems)")
	locate.file.exe<-if (interactive()) choose.dir(getwd(), "Select the folder of DataFileConverter")
	locate.file.exe<-paste(locate.file.exe,"\\DataFileConverter.exe",sep="")
	locate.file.exe<-shQuote(locate.file.exe,type="cmd")
	
	####  Import and convert fsa files in txt files  ####
	
	locate.dir.fsa<-if (interactive()) choose.dir(getwd(), "Select folder containing fsa files")
	
	tt1 <- tktoplevel()
	tkwm.title(tt1,"Loading")
	tkgrid(tklabel(tt1,font="arial 12",text="Please wait...\n This operation may take several minutes                           "))
	tkfocus(tt1)
	tkconfigure(tt1)
	
	file.fsa<-"\\*.fsa"
	dir.file.fsa<-paste(locate.dir.fsa,file.fsa,sep="")
	dir.file.fsa<-shQuote(dir.file.fsa,type="cmd")
	
	dir.txt<-"\\txt"
	dir.file.txt<-paste(locate.dir.fsa,dir.txt,sep="")
	dir.create(dir.file.txt)
	dir.txt2<-shQuote(dir.file.txt,type="cmd")
	
	args<-paste("-i", dir.file.fsa ,"-t -o", dir.txt2, " -d -s")
	system(paste(locate.file.exe,args))
	
	####  Create the list "raw.data" including the four channel of each sample and a vector with the name of the samples  ####
	
	raw.data<-list()
	channel<-list()
	
	####  Fill raw.data list  ####
	
	list.file.fsa<-list.files(dir.file.txt)
	delimiter<-"\\"
	list.file.txt<-paste(dir.file.txt,delimiter,list.file.fsa,sep="")
	
	a<-readLines(list.file.txt[1])
	no_channel<-grep("DATA",a)
	channel<-list()
	
	for (i in 1:length(list.file.txt))
	{
		a<-readLines(list.file.txt[i])
		for (j in 1:length(no_channel))
		{
			channel[[j]]<-unlist(strsplit(a[no_channel[j]]," "))
			channel[[j]]<-channel[[j]][-1]
		}
		raw.data[[i]]<-channel
	}
	
	raw.data[[length(list.file.txt)+1]]<-basename(list.file.txt)
	mat10<-raw.data
	mat10<<-mat10
	nn<-mat10[[length(mat10)]]
	mat11<-vector("list", length=c(length(mat10)-1))
	names(mat11)<-nn
	
	for (j in 1:c(length(mat10)-1))
	{
		mat11[[j]]<-matrix(nrow=length(mat10[[j]]),ncol=length(mat10[[j]][[1]]))
		for (i in 1:dim(mat11[[j]])[1])
		{
			mat11[[j]][i,c(1:length(mat10[[j]][[i]]))]<-as.numeric(mat10[[j]][[i]])
		} 
	}
	
	zf3<<-mat11
	
	tkdestroy(tt1)
	
	
	
	tt <- tktoplevel()
	tclRequire("BWidget")
	tkwm.title(tt,"Import fingerprint profiles")

	####  Displays profiles to select profile and rox channel  ####
	
	view.channels<-function()
	{
		tt11 <- tktoplevel()
		tclRequire("BWidget")
		tkwm.title(tt11,"Help to attribute channel")
		tkgrid(tklabel(tt11,text=""))
		tt111<-tkframe(tt11)
		text1<-tklabel(tt111,text="Which channel to plot ?")
		com <- paste("Column",c(1:dim(zf3[[1]])[1]),sep=" ")
		comu <- tkwidget(tt111,"ComboBox",editable=FALSE,values=com,height=dim(zf3[[1]])[1])
		tkpack(text1,comu,side="left")
		tkgrid(tt111)
		
		####  Plot the selected channel  ####
		
		plot.channel <- function()
		{
			com1 <-unlist(as.numeric(tcl(comu,"getvalue"))+1)
			plot(1:c(dim(zf3[[1]])[2]),zf3[[1]][com1,],type="l",col="blue")
			legend("topright",com[com1],lty=1,col="blue")
		}
		
		tkgrid(tklabel(tt11,text=""))
		tt3<-tkframe(tt11)
		b1<-tkbutton(tt3,text="plot",command=plot.channel)
		
		b2<-tkbutton(tt3,text="Cancel",command= function() tkdestroy(tt11))
		tkpack(b1,b2,side="left")
		tkgrid(tt3)
		tkgrid(tklabel(tt11,text=""))
		tkfocus(tt11)	
	}
	
	tkgrid(tklabel(tt,text=""))
	tkgrid(tkbutton(tt,text="How to choose community and internal standard location",command=view.channels))
	tkgrid(tklabel(tt,text=""))
	
	tt1<-tkframe(tt)
	text1<-tklabel(tt1,text="Community location in raw files?")
	com <- paste("Column",c(1:dim(zf3[[1]])[1]),sep=" ")
	comu <- tkwidget(tt1,"ComboBox",editable=FALSE,values=com,height=dim(zf3[[1]])[1])
	tkpack(text1,comu,side="left")
	tkgrid(tt1)
	
	tt2<-tkframe(tt)
	text2<-tklabel(tt2,text="Internal standard location in raw files ?")
	int <- paste("Column",c(1:dim(zf3[[1]])[1]),sep=" ")
	inte <- tkwidget(tt2,"ComboBox",editable=FALSE,values=int,height=dim(zf3[[1]])[1])
	tkpack(text2,inte,side="left")
	tkgrid(tt2)
	
	import.channels <- function()
	{
		com1 <-unlist(as.numeric(tcl(comu,"getvalue"))+1)
		ref1 <-unlist(as.numeric(tcl(inte,"getvalue"))+1 )
		mat<-list(profil=matrix(ncol=dim(zf3[[1]])[2],nrow=length(zf3)),rox=matrix(ncol=dim(zf3[[1]])[2],nrow=length(zf3)))
		rownames(mat$profil)<-names(zf3);rownames(mat$rox)<-names(zf3)
		for (i in 1:length(zf3))
		{
			mat$profil[i,]<-zf3[[i]][com1,]
		}
		for (i in 1:length(zf3))
		{
			mat$rox[i,]<-zf3[[i]][ref1,]
		}
		
		####  Initialize matrices and vectors  ####
		
		mat.raw<<-mat 
		mat.analyse<<-mat$profil
		rxref<-0
		rxref<<-rxref 
		alig<-vector(length=length(rownames(mat$profil)));		alig<-rownames(mat$profil);						alig<<-alig
		
		fact<-matrix(nr=dim(mat$profil)[1],nc=2);					fact[]<-1;						rownames(fact)<-rownames(mat$profil)					;fact<<-fact
		param<-matrix(nr=dim(mat$profil)[1],nc=2);				param[]<-1;					rownames(param)<-rownames(mat$profil)				;param<<-param
		div<-matrix(nr=dim(mat$profil)[1],nc=2);						div[]<-1;						names(div)<-rownames(mat$profil)							;div<<-div
		mat.align<-matrix(nr=dim(mat$profil)[1],nc=2);			mat.align[]<-1;				rownames(mat.align)<-rownames(mat$profil)			;mat.align<<-mat.align
		mat.background<-matrix(nr=dim(mat$profil)[1],nc=2);	mat.background[]<-1;	rownames(mat.background)<-rownames(mat$profil)	;mat.background<<-mat.background 
		mat.baseline<-matrix(nr=dim(mat$profil)[1],nc=2);		mat.baseline[]<-1;		rownames(mat.baseline)<-rownames(mat$profil);		mat.baseline<<-mat.baseline  
		mat.range<-matrix(nr=dim(mat$profil)[1],nc=2);			mat.range[]<-1;			rownames(mat.range)<-rownames(mat$profil)			;mat.range<<-mat.range 
		mat.normalise<-matrix(nr=dim(mat$profil)[1],nc=2);		mat.normalise[]<-1;		rownames(mat.normalise)<-rownames(mat$profil)		;mat.normalise<<-mat.normalise 
		mat.rebuilt<-matrix(nr=dim(mat$profil)[1],nc=2);			mat.rebuilt[]<-1;			rownames(mat.rebuilt)<-rownames(mat$profil)			;mat.rebuilt<<-mat.rebuilt 
		mat.binary<-matrix(nr=dim(mat$profil)[1],nc=2);			mat.binary[]<-1;			rownames(mat.binary)<-rownames(mat$profil)			;mat.binary<<-mat.binary 
		print("Successfully loaded!")
		tkdestroy(tt)
		tkfocus(MainMenu)
	}

	####  Main window ####
	
	tkgrid(tklabel(tt,text=""))
	tt3<-tkframe(tt)
	b1<-tkbutton(tt3,text="Import all",command=import.channels)
	b2<-tkbutton(tt3,text="Cancel",command=function() tkdestroy(tt))
	tkpack(b1,b2,side="left")
	tkgrid(tt3)
	tkgrid(tklabel(tt,text=""))
	tkfocus(tt)
}

