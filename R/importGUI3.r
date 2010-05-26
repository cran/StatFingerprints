importGUI3 <-
function()
{
	fil=if (interactive()) file.choose()
	
	tt<-tktoplevel()
	tkwm.title(tt,"Import an ecological table")
	tkgrid(tklabel(tt,text="                                                                                            "))
	
	t1<-tkframe(tt)
	text1<-tklabel(t1,text="Each sample is in")
	fa<-c("row","column")
	trans<-tkwidget(t1,"ComboBox",editable=FALSE,values=fa,height=2)
	tkpack(text1,trans,side="left")
	tkgrid(t1)
	
	tkgrid(tklabel(tt,text=" "))
	
	t2<-tkframe(tt)
	text2<-tklabel(t2,text="Do you have column header")
	he <- c("yes","no")
	heac <- tkwidget(t2,"ComboBox",editable=FALSE,values=he,height=2)
	tkpack(text2,heac,side="left")
	tkgrid(t2)
	
	t3<-tkframe(tt)
	text3<-tklabel(t3,text="Do you have row header")
	he <- c("yes","no")
	hear <- tkwidget(t3,"ComboBox",editable=FALSE,values=he,height=2)
	tkpack(text3,hear,side="left")
	tkgrid(t3)
	
	t4<-tkframe(tt)
	text4<-tklabel(t4,text="Choose the field separator")
	sep<-c("; (semi colon)",",  (comma)",".  (dot)")
	sepe<-tkwidget(t4,"ComboBox",editable=FALSE,values=sep,height=3)
	tkpack(text4,sepe,side="left")
	tkgrid(t4)
	
	t5<-tkframe(tt)
	text5<-tklabel(t5,text="Choose decimal symbol")
	dec <- c(", (comma)",". (dot)")
	dece <- tkwidget(t5,"ComboBox",editable=FALSE,values=dec,height=2)
	tkpack(text5,dece,side="left")
	tkgrid(t5)
	
	import.file<-function()
	{
		m1<-0
		m<-0
		trans1<- unlist(as.numeric(tcl(trans,"getvalue"))+1 )
		heac1 <-unlist(as.numeric(tcl(heac,"getvalue"))+1 )
		if(heac1==1) heac<-TRUE
		if(heac1==2) heac<-FALSE
		hear1 <-unlist(as.numeric(tcl(hear,"getvalue"))+1 )
		if(hear1==1) hear<-TRUE
		if(hear1==2) hear<-FALSE  
		sep1 <-unlist(as.numeric(tcl(sepe,"getvalue"))+1 )
		if(sep1==1) sep<-";"
		if(sep1==2) sep<-","
		if(sep1==3) sep<-"."
		dec1 <-unlist(as.numeric(tcl(dece,"getvalue"))+1 )
		if(dec1==1) dec<-","
		if(dec1==2) dec<-"."
		m<-read.table(file=fil,colClasses = "character",dec=dec,sep=sep,h=heac)
		if (hear==TRUE) hear1<-m[[1]]
		if (hear==TRUE) m<-m[,-1]
		
		m1<-matrix(nr=dim(m)[1],nc=dim(m)[2])
		colnames(m1)<-colnames(m)
		for (i in 1:dim(m)[2])
		{ 
			m1[,i]<-as.numeric(unlist(m))[c(dim(m)[1]*i-c(dim(m)[1]-1)):c(dim(m)[1]*i)]
		}
		if(hear==TRUE) 
			rownames(m1)<-hear1
		if (trans1==2)
			m1<-t(m1)
		mat.raw<-list(profil=m1,rox=m1)
		mat.raw<<-mat.raw
		mat.analyse<-m1
		mat.analyse<<-mat.analyse
		rxref<-0
		rxref<<-rxref
		alig<-paste("Align_",rownames(mat.raw$profil))
		alig<<-alig
		fact<-matrix(nr=dim(mat.raw$profil)[1],nc=2);						fact[]<-1;						rownames(fact)<-rownames(mat.raw$profil);					fact<<-fact
		param<-matrix(nr=dim(mat.raw$profil)[1],nc=2);					param[]<-1;					rownames(param)<-rownames(mat.raw$profil);				param<<-param
		div<-matrix(nr=dim(mat.raw$profil)[1],nc=2);						div[]<-1;						names(div)<-rownames(mat.raw$profil);							div<<-div 
		mat.align<-matrix(nr=dim(mat.raw$profil)[1],nc=2);				mat.align[]<-1;				rownames(mat.align)<-rownames(mat.raw$profil);			mat.align<<-mat.analyse
		mat.background<-matrix(nr=dim(mat.raw$profil)[1],nc=2);	mat.background[]<-1;	rownames(mat.background)<-rownames(mat.raw$profil);	mat.background<<-mat.background 
		mat.baseline<-matrix(nr=dim(mat.raw$profil)[1],nc=2);			mat.baseline[]<-1;		rownames(mat.baseline)<-rownames(mat.raw$profil);		mat.baseline<<-mat.baseline 
		mat.range<-matrix(nr=dim(mat.raw$profil)[1],nc=2);			mat.range[]<-1;			rownames(mat.range)<-rownames(mat.raw$profil);			mat.range<<-mat.range
		mat.normalise<-matrix(nr=dim(mat.raw$profil)[1],nc=2);		mat.normalise[]<-1;		rownames(mat.normalise)<-rownames(mat.raw$profil);		mat.normalise<<-mat.normalise  
		mat.rebuilt<-matrix(nr=dim(mat.raw$profil)[1],nc=2);			mat.rebuilt[]<-1;			rownames(mat.rebuilt)<-rownames(mat.raw$profil);			mat.rebuilt<<-mat.rebuilt 
		mat.binary<-matrix(nr=dim(mat.raw$profil)[1],nc=2);			mat.binary[]<-1			;rownames(mat.binary)<-rownames(mat.raw$profil);			mat.binary<<-mat.binary
		print("Successfully loaded!")
		tkdestroy(tt)
	}
	
	tkgrid(tklabel(tt,text=" "))
	t6<-tkframe(tt)
	b1<-tkbutton(t6,text="Import ecological table",command=import.file)
	b2<-tkbutton(t6,text="Cancel",command=function() tkdestroy(tt))
	tkpack(b1,b2,side="left")
	tkgrid(t6)
	tkgrid(tklabel(tt,text=" "))
	tkfocus(tt)
}

