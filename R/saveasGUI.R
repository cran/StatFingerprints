saveasGUI <-
function()
{
	fileName<-tclvalue(tkgetSaveFile())
	filename<-paste(fileName,".Rdata",sep="")
	if (filename == "")
		return()
	
	tt1 <- tktoplevel()
	tkwm.title(tt1,"Loading")
	tkgrid(tklabel(tt1,font="arial 12",text="Please wait...\n This operation may take several minutes                           "))
	tkfocus(tt1)
	tkconfigure(tt1)
	
	filename<<-filename
	
	save(list=ls(envir=.GlobalEnv), file=filename)
#	save(mat.raw,fact,param,div,alig,filename,rxref,mat.align,mat.background,mat.baseline,mat.range,mat.analyse,mat.normalise,mat.rebuilt,mat.binary,file=filename)
	
	tkdestroy(tt1)
	tkfocus(MainMenu)
}

