Robject2SFGUI <-
function(){
	none<<-0 
	aaa<<-objects(envir=.GlobalEnv)
	
	require(StatFingerprints)
	tclRequire("BWidget")
	
	tt <- tktoplevel()
	tkwm.title(tt,"Transfert RISA profiles to StatFingerprints program")
	tkgrid(tklabel(tt,text="                                                                                           "))
	
	t5<-tkframe(tt)  
	text5<-tklabel(t5,text="Select your profiles")
	sep<-aaa
	sepe<-tkwidget(t5,"ComboBox",editable=FALSE,values=sep,height=length(sep))
	tkpack(text5,sepe,side="left")
	tkgrid(t5)
	t1<-tkframe(tt)  
	text1<-tklabel(t1,text="Select your factors (if no factors, select none)")
	sep1<-aaa
	sepe1<-tkwidget(t1,"ComboBox",editable=FALSE,values=c("none",sep1),height=length(sep1)+1)
	tkpack(text1,sepe1,side="left")
	tkgrid(t1)  
	t2<-tkframe(tt)  
	text2<-tklabel(t2,text="Select your parameters (if no parameters, select none)")
	sep2<-aaa
	sepe2<-tkwidget(t2,"ComboBox",editable=FALSE,values=c("none",sep2),height=length(sep2)+1)
	tkpack(text2,sepe2,side="left")
	tkgrid(t2)   
	
	ess<-function()
	{
		sep11<-unlist(as.numeric(tcl(sepe,"getvalue"))+1 );sep11<-aaa[sep11]
		sep12<-unlist(as.numeric(tcl(sepe1,"getvalue"))+1 );sep12<-aaa[sep12]
		sep13<-unlist(as.numeric(tcl(sepe2,"getvalue"))+1 );sep13<-aaa[sep13]
		if (sep11=="none") tkmessageBox(message="Error, you have to select RISA profiles")
		if (sep11=="none") stop()
		
		
		profil<-as.matrix(get(sep11))
		rox<-as.matrix(get(sep11))
		mat.raw<-list(profil=profil,rox=rox);mat.raw<<-mat.raw
		mat.analyse<-as.matrix(get(sep11));mat.analyse<<-mat.analyse 
		mat.align<-as.matrix(get(sep11));mat.align<<-mat.align
		mat.baseline<-as.matrix(get(sep11));mat.baseline<<-mat.baseline
		mat.normalisation<-as.matrix(get(sep11));mat.normalisation<<-mat.normalisation
		mat.range<-as.matrix(get(sep11)); mat.range<<-mat.range
		mat.rebuilt<-as.matrix(get(sep11)); mat.rebuilt<<-mat.rebuilt
		mat.binary<-as.matrix(get(sep11));mat.binary<<-mat.binary
		mat.background<-as.matrix(get(sep11));mat.background<<-mat.background
		filename<-"Don't forget to save your new project";filename<<-filename
		rxref<-0;rxref<<-rxref
		alig<-vector(length=dim(as.matrix(get(sep11)))[1]) ;alig[]<-1 ;alig<<-alig
		
		div<-matrix(nr=5,nc=2) ; div[]<-1;div<<-div
		if (sep12=="none") 
		{
			fact<-matrix(nr=5,nc=2)
			fact[]<-1
		}
		
		if (sep12!="none") 
			fact<-get(sep12)
		fact<<-fact
		
		if (sep13=="none") 
		{
			param<-matrix(nr=5,nc=2)
			param[]<-1
		}
		
		if (sep13!="none") 
			param<-as.matrix(as.matrix(get(sep13)))
		param<<-param
		
		tkdestroy(tt)
		
	}
	tkgrid(tklabel(tt,text="                                                                                           "))
	tkgrid(tkbutton(tt,text="Go to StatFingerprints",command=ess))
	tkgrid(tklabel(tt,text="                                                                                           "))
	
}

