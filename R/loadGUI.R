loadGUI <-
function()
{
	newrdata()
	fil=if (interactive()) file.choose()
	
	tt1 <- tktoplevel()
	tkwm.title(tt1,"Loading")
	tkgrid(tklabel(tt1,font="arial 12",text="Please wait...\n This operation may take several minutes                           "))
	tkfocus(tt1)
	tkconfigure(tt1)
	
	load(file=fil)
	filename<<-filename
	if(exists("mat")==TRUE)
		mat.raw<<-mat else mat.raw<<-mat.raw
	if(exists("mat1")==TRUE)
		mat.align<<-mat1 else mat.align<<-mat.align
	rxref<<-rxref
	#if (exists(fact))
		fact<<-fact
	#if (exists(param))
		param<<-param
	if(is.vector(div)==TRUE) 
	{
		div.names<-names(div)
		div.num<-as.numeric(div)
		div<-as.matrix(div)
		rownames(div)<-div.names
	}
	else 
		div<<-div
	alig<<-alig
	if(exists("mat2")==TRUE) mat.background	<<-mat2 else mat.background<<-mat.background
	if(exists("mat3")==TRUE) mat.baseline		<<-mat3 else mat.baseline<<-mat.baseline
	if(exists("mat5")==TRUE) mat.range			<<-mat5 else mat.range<<-mat.range
	if(exists("mat6")==TRUE) mat.analyse		<<-mat6 else mat.analyse<<-mat.analyse
	if(exists("mat7")==TRUE) mat.normalise		<<-mat7 else mat.normalise<<-mat.normalise
	if(exists("mat8")==TRUE) mat.rebuilt			<<-mat8 else mat.rebuilt<<-mat.rebuilt
	if(exists("mat9")==TRUE) mat.binary			<<-mat9 else mat.binary<<-mat.binary  
	print("Your project has been successfully loaded")
	
	tkdestroy(tt1)
}

