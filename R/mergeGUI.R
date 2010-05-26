mergeGUI <-
function()
{ 
	tt<-tktoplevel()
	tkwm.title(tt,"Merge two files")

	####  Load the 2 projects  ####
	
	load.project<-function()
	{ 
		####  First project  ####
		loadGUI()
		mat.raw.1<<-mat.raw
		mat.align.1<<-mat.align
		rxref1<<-rxref
		fact1<<-fact
		param1<<-param
		div1<<-div
		alig1<<-alig
		
		####  Second project  ####
		loadGUI()
		mat.raw.2<<-mat.raw
		mat.align.2<<-mat.align
		rxref2<<-rxref
		fact2<<-fact
		param2<<-param
		div2<<-div
		alig2<<-alig
		print("Your two projects have been successfully loaded")
	}

	####  Merge the 2 projects  ####
	
	merge<-function()
	{
		a01<-0
		for (i in 1:length(rxref2))
		{
			if (rxref2[i]!=rxref1[i]) a01<-1
		}
		
		####  WARNINGS  ####
		if (a01==1)
		{
			tkmessageBox(message="Peaks of your both reference standard are not the same and thus fingerprint profiles between the two projects could not be adequatly compared",icon="warning",type="ok")
			stop("")
		}
			
		if (dim(fact1)[2]!= dim(fact2)[2])
		{
			tkmessageBox(message="Merge can not be done. The factor files have not the same dimensions" ,icon="warning",type="ok")
			stop("")
		}
		if (dim(param1)[2]!= dim(param2)[2])
		{
			tkmessageBox(message="Merge can not be done. The parameter files have not the same dimensions" ,icon="warning",type="ok")
			stop("")
		}
		if (dim(mat.raw.1$profil)[2]!=dim(mat.raw.2$profil)[2])
		{
			tkmessageBox(message="Merge can not be done. Problem occured as fingerprint profiles in the two projects have not the same length" ,icon="warning",type="ok")
			stop("")
		}
		if (dim(mat.align.1)[2]!=dim(mat.align.2)[2])
		{
			tkmessageBox(message="Merge can not be done. Problem occured as fingerprint ALIGNED profiles in the two projects have not the same length" ,icon="warning",type="ok")
			stop("")
		}
		
		####  Merge matrices  ####
		mat.raw$profil<<-rbind(mat.raw.1$profil,mat.raw.2$profil)
		mat.raw$rox<<-rbind(mat.raw.1$rox,mat.raw.2$rox)
		mat.align<<-rbind(mat.align.1,mat.align.2)
		rxref<<-rxref1
		fact<<-rbind(fact1,fact2)
		
		
		#### Merge factors   ####
		relevels<-function(ff)
		{
			for (i in 1:dim(ff)[2])
			{
				f<-ff[,i]
				a=vector(length=length(levels(f)))
				for (j in 1:length(levels(f)))
				{
					a[j]<-length(which(f==levels(f)[j]))
				}
				a=which(a==0)
				j<-vector(length=length(f))
				j<-factor(j)
				if (length(a)!=0)
					levels(j)<-levels(f)[-a]
				if (length(a)==0)
					levels(j)<-levels(f)
				j[]<-f
				ff[,i]<-j
			}
			return(ff)
		} 
		if(is.factor(fact1)==TRUE & is.factor(fact2)==TRUE) 
			fact<<-relevels(fact)
		
		####  Initialize matrices and vectors  ####
		##  baseline, range and normalisation must be done again on the resulting matrix  ##
		
		alig<<-c(alig1,alig2)
		param<<-rbind(param1,param2)
		div<<- rbind(div1,div2)
		mat.baseline<-matrix(nr=dim(mat.align)[1],nc=2);		mat.baseline[]<-1;	rownames(mat.baseline)<-rownames(mat.align);	mat.baseline<<-mat.baseline
		mat.range<-matrix(nr=dim(mat.align)[1],nc=2);		mat.range[]<-1;		rownames(mat.range)<-rownames(mat.align);		mat.range<<-mat.range
		mat.analyse<<-mat.raw$profil
		mat.normalise<-matrix(nr=dim(mat.align)[1],nc=2);	mat.normalise[]<-1;	rownames(mat.normalise)<-rownames(mat.align);	mat.normalise<<-mat.normalise
		mat.background<-matrix(nr=dim(mat.align)[1],nc=2);	mat.background[]<-1;	rownames(mat.background)<-rownames(mat.align);	mat.background<<-mat.background
		mat.rebuilt<-matrix(nr=dim(mat.align)[1],nc=2);		mat.rebuilt[]<-1;		rownames(mat.rebuilt)<-rownames(mat.align);		mat.rebuilt<<-mat.rebuilt
		mat.binary<-matrix(nr=dim(mat.align)[1],nc=2);		mat.binary[]<-1;		rownames(mat.binary)<-rownames(mat.align);		mat.binary<<-mat.binary
		print("Your two projects have been successfully merged")
	}

	####  Main window  ####
	
	tkgrid(tkbutton(tt,font="arial 11",text="1 Step : load two projects",command=load.project))
	tkgrid(tklabel(tt,text=""))
	tkgrid(tklabel(tt,font="arial 10",wraplength="4i",justify="left",text="In the two projects, profiles must have the same number of scans, and if they are already aligned the reference standard used must be the same (same number of peaks define has reference) for both.  "))
	tkgrid(tklabel(tt,text=""))
	tkgrid(tkbutton(tt,font="arial 11",text="2 Step : merge the projects",command=merge))
	tkgrid(tklabel(tt,text=""))
	tkgrid(tklabel(tt,font="arial 10",wraplength="4i",justify="left",text="After merging the two projects and save into a new project, don't forget to process profiles from step 3 to 5 before statistical analyses"))
		
	tkfocus(tt)
}

