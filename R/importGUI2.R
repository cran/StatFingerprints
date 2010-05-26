importGUI2 <-
function()
{
	fil=if (interactive()) file.choose()
	
	tt<-tktoplevel()
	tkwm.title(tt,"Import variables (ASCII Files)")
	tkgrid(tklabel(tt,text="                                                                                                            "))
	
	t1<-tkframe(tt)
	text1<-tklabel(t1,text="Choose the field separator")
	sep<-c(";  (semicolon)",",  (comma)",".  (dot)")
	sepe<-tkwidget(t1,"ComboBox",editable=FALSE,values=sep,height=3)
	tkpack(text1,sepe,side="left")
	tkgrid(t1)
	
	t2<-tkframe(tt)
	text2<-tklabel(t2,text="Choose decimal symbol")
	dec <- c(",  (comma)",".  (dot)")
	dece <- tkwidget(t2,"ComboBox",editable=FALSE,values=dec,height=2)
	tkpack(text2,dece,side="left")
	tkgrid(t2)
	
	t3<-tkframe(tt)
	text3<-tklabel(t3,text="Do you have header?")
	he <- c("Yes","No")
	hea <- tkwidget(t3,"ComboBox",editable=FALSE,values=he,height=2)
	tkpack(text3,hea,side="left")
	tkgrid(t3)
	
	t4<-tkframe(tt)
	text4<-tklabel(t4,text="File type")
	fa<-c("Qualitative variables","Quantitative variables")
	fac<-tkwidget(t4,"ComboBox",editable=FALSE,values=fa,height=2)
	tkpack(text4,fac,side="left")
	tkgrid(t4)
	
	import.file<-function()
	{
		sep1 <-unlist(as.numeric(tcl(sepe,"getvalue"))+1 )
		if(sep1==1) sep<-";"
		if(sep1==2) sep<-","
		if(sep1==3) sep<-"."
		dec1 <-unlist(as.numeric(tcl(dece,"getvalue"))+1 )
		if(dec1==1) dec<-","
		if(dec1==2) dec<-"."
		he1 <-unlist(as.numeric(tcl(hea,"getvalue"))+1 )
		if(he1==1) he<-TRUE
		if(he1==2) he<-FALSE
		fa1<-unlist(as.numeric(tcl(fac,"getvalue"))+1)
		if(fa1==1) fact<-read.table(fil,header=he,dec=dec,sep=sep)
		if(fa1==1 & dim(fact)[1]!=dim(mat.raw$profil)[1]) stop("Errors: Number of qualitative variables differs of this of fingerprint profiles")
		
		if(fa1==1) rownames(fact)<-rownames(mat.raw$profil)
		if(fa1==2) param<-read.table(fil,header=he,dec=dec,sep=sep)
		if(fa1==2 & dim(param)[1]!=dim(mat.raw$profil)[1]) stop("Errors: Number of quantitative variables differs of this of fingerprint profiles")
		if(fa1==2 & is.factor(param[2,1])==TRUE) param=param[,2:dim(param)[2]]
		if(fa1==2) rownames(param)<-rownames(mat.raw$profil)
		if(fa1==1) 
			for (i in 1:dim(fact)[2])
			{
				fact[i]<-factor(fact[[i]])
			}
		
		if(fa1==1) fact<<-fact
		if(fa1==2) param<<-param
		print("Variables have been successfully loaded!")
		tkdestroy(tt)
		if(fa1==1) fact<<-edit(fact)
		if(fa1==2) param<<-edit(param)
		tkfocus(MainMenu)
	}
	tkgrid(tklabel(tt,text="  "))
	
	t5<-tkframe(tt)
	b1<-tkbutton(t5,text="Import CSV",command=import.file)
	b2<-tkbutton(t5,text="Cancel",command=function() tkdestroy(tt))
	tkpack(b1,b2,side="left")
	tkgrid(t5)
	tkgrid(tklabel(tt,text="   "))
	tkfocus(tt)
}

