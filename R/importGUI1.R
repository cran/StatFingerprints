importGUI1 <-
function()
{

	####  Select field to import ASCII profiles  ####
	
	fil=if (interactive()) file.choose()
	tt <- tktoplevel()
	tkwm.title(tt,"Import fingerprint profiles")
	tkgrid(tklabel(tt,text="")) 
	
	t1<-tkframe(tt)
	text1<-tklabel(t1,text="What is the field separator")
	sep1<-c(", (comma)",";  (semicolon)",".  (dot)")
	sep2<-tkwidget(t1,"ComboBox",editable=FALSE,values=sep1,height=3)
	tkpack(text1,sep2,side="left")
	tkgrid(t1)
	
	t2<-tkframe(tt)
	text2<-tklabel(t2,text="What is the decimal symbol")
	dec1<-c(".  (dot)",", (comma)")
	dec2<-tkwidget(t2,"ComboBox",editable=FALSE,values=dec1,height=2)
	tkpack(text2,dec2,side="left")
	tkgrid(t2)
	
	t3<-tkframe(tt)
	text3<-tklabel(t3,text="Do you have header")
	hea1 <- c("yes","no")
	hea2 <- tkwidget(t3,"ComboBox",editable=FALSE,values=hea1,height=2)
	tkpack(text3,hea2,side="left")
	tkgrid(t3)
	tkgrid(tklabel(tt,text="")) 
	
	####  Preview channels  ####
	
	view.channels<-function()
	{
		sep3 <-unlist(as.numeric(tcl(sep2,"getvalue"))+1 )
		if(sep3==1)      sep4<-","
		if(sep3==2)      sep4<-";"
		if(sep3==3)      sep4<-"."
		dec3 <-unlist(as.numeric(tcl(dec2,"getvalue"))+1 )
		if(dec3==1)      dec4<-"."
		if(dec3==2)      dec4<-","
		hea3 <-unlist(as.numeric(tcl(hea2,"getvalue"))+1 )
		if(hea3==1)      hea4<-TRUE
		if(hea3==2)      hea4<-FALSE	
		
		a=read.table(fil[1],header=hea4,dec=dec4,sep=sep4)
		a<<-a 
		
		plot(1:dim(a)[1],a[,1],type="l",col="blue",xlab="Scan of your 1 st fingerprint profile",ylab="Signal intensity")
		par(new=TRUE)
		plot(1:dim(a)[1],a[,3],type="l",col="black",xaxt="n",yaxt="n",ylab=NA,xlab=NA)
		par(new=TRUE)
		plot(1:dim(a)[1],a[,4],type="l",col="red",xaxt="n",yaxt="n",ylab=NA,xlab=NA)
		par(new=TRUE)
		plot(1:dim(a)[1],a[,2],type="l",col="green",xaxt="n",yaxt="n",ylab=NA,xlab=NA)
		legend("topright",col=c("blue","green","black","red"),c("1 st column","2 nd column","3 rd column","4 th column"),lty=c(1,1,1,1))
	}
	
	tkgrid(tkbutton(tt,text="How to choose community and internal standard location",command=view.channels))
	tkgrid(tklabel(tt,text="")) 
	
	#### Choose community and reference channel  ####
	
	t4<-tkframe(tt)
	text4<-tklabel(t4,text="Community location in raw files?")
	com <- c("Column 1","Column 2","Column 3","Column 4")
	comu <- tkwidget(t4,"ComboBox",editable=FALSE,values=com,height=4)
	tkpack(text4,comu,side="left")
	tkgrid(t4)
	
	t5<-tkframe(tt)
	text5<-tklabel(t5,text="Internal standard location in raw files?")
	int <- c("Column 1","Column 2","Column 3","Column 4")
	inte <- tkwidget(t5,"ComboBox",editable=FALSE,values=int,height=4)
	tkpack(text5,inte,side="left")
	tkgrid(t5)
	
	####  Import channels  ####	
	
	import.channels <- function()
	{
		tt1 <- tktoplevel()
		tkwm.title(tt1,"Loading")
		tkgrid(tklabel(tt1,text="Please wait...                             "))
		tkfocus(tt1)
		tkconfigure(tt1,cursor="watch")
		
		com1 <-unlist(as.numeric(tcl(comu,"getvalue"))+1)
		ref1 <-unlist(as.numeric(tcl(inte,"getvalue"))+1 )
		sep3 <-unlist(as.numeric(tcl(sep2,"getvalue"))+1 )
		if(sep3==1)      sep4<-","
		if(sep3==2)      sep4<-";"
		if(sep3==3)      sep4<-"."
		dec3 <-unlist(as.numeric(tcl(dec2,"getvalue"))+1 )
		if(dec3==1)      dec4<-"."
		if(dec3==2)      dec4<-","
		hea3 <-unlist(as.numeric(tcl(hea2,"getvalue"))+1 )
		if(hea3==1)      hea4<-TRUE
		if(hea3==2)      hea4<-FALSE
		
		d<-dim(read.table(fil[1],header=hea4,dec=dec4,sep=sep4))[1]
		mat.raw<-list(profil=matrix(ncol=d[1],nrow=length(fil)),rox=matrix(ncol=d[1],nrow=length(fil)))
		
		for(i in 1:length(fil))
		{
			sscp<-read.table(fil[i],header=hea4,dec=dec4,sep=sep4)
			mat.raw$rox[i,]<-sscp[,ref1]
			mat.raw$profil[i,]<-sscp[,com1]
		}
		
		####  Initialize matrices and vectors  ####
		
		rownames(mat.raw$profil)<-basename(fil)
		rownames(mat.raw$rox)<-basename(fil)
		mat.raw<<-mat.raw
		mat.analyse<<-mat.raw$profil
		rxref<-0
		rxref<<-rxref
		alig<-vector(length=length(rownames(mat.raw$profil)));		alig<-rownames(mat.raw$profil);																			alig<<-alig
		fact<-matrix(nr=dim(mat.raw$profil)[1],nc=2);						fact[]<-1;						rownames(fact)<-rownames(mat.raw$profil);					fact<<-fact
		param<-matrix(nr=dim(mat.raw$profil)[1],nc=2);					param[]<-1;					rownames(param)<-rownames(mat.raw$profil);				param<<-param
		div<-matrix(nr=dim(mat.raw$profil)[1],nc=2);						div[]<-1;						names(div)<-rownames(mat.raw$profil);							div<<-div 
		mat.align<-matrix(nr=dim(mat.raw$profil)[1],nc=2);				mat.align[]<-1;				rownames(mat.align)<-rownames(mat.raw$profil);			mat.align<<-mat.align
		mat.background<-matrix(nr=dim(mat.raw$profil)[1],nc=2);	mat.background[]<-1;	rownames(mat.background)<-rownames(mat.raw$profil);	mat.background<<-mat.background 
		mat.baseline<-matrix(nr=dim(mat.raw$profil)[1],nc=2);			mat.baseline[]<-1;		rownames(mat.baseline)<-rownames(mat.raw$profil);		mat.baseline<<-mat.baseline 
		mat.range<-matrix(nr=dim(mat.raw$profil)[1],nc=2);			mat.range[]<-1;			rownames(mat.range)<-rownames(mat.raw$profil);			mat.range<<-mat.range
		mat.normalise<-matrix(nr=dim(mat.raw$profil)[1],nc=2);		mat.normalise[]<-1;		rownames(mat.normalise)<-rownames(mat.raw$profil);		mat.normalise<<-mat.normalise  
		mat.rebuilt<-matrix(nr=dim(mat.raw$profil)[1],nc=2);			mat.rebuilt[]<-1;			rownames(mat.rebuilt)<-rownames(mat.raw$profil);			mat.rebuilt<<-mat.rebuilt 
		mat.binary<-matrix(nr=dim(mat.raw$profil)[1],nc=2);			mat.binary[]<-1			;rownames(mat.binary)<-rownames(mat.raw$profil);			mat.binary<<-mat.binary
		print("Successfully loaded!")
		tkdestroy(tt1)
		tkdestroy(tt)
		
	}
	
	####  Main window  ####
	
	tkgrid(tklabel(tt,text="")) 
	t6<-tkframe(tt)
	b1<-tkbutton(t6,text="Import all",command=import.channels)
	b2<-tkbutton(t6,text="Cancel",command=function() tkdestroy(tt))
	tkpack(b1,b2,side="left")
	tkgrid(t6)
	tkfocus(tt)
	tkgrid(tklabel(tt,text="")) 
}

