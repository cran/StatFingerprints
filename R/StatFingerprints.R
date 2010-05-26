StatFingerprints <-
function()
{
### If there is alternartives sources files in the directory 'addon', use theese files instead of the original ones
  for (i in c(1:length(.libPaths()))) 
  {
    if (file.exists(paste(.libPaths()[i],"/StatFingerprints/addon", sep="")))
    {
      addonfiles=dir(paste(.libPaths()[i],"/StatFingerprints/addon", sep="")) 
      if ((length(addonfiles))!=0)
      {
        for (j in c(1:length(addonfiles))) 
        {
          source(paste(.libPaths()[i],"/StatFingerprints/addon/",addonfiles[j],sep=""))
        }
      }
    }
  }
  
  ## Si filename et mat.raw n'existent pas, lance la fonction newrdata qui initialise tous les objets
	if(exists("filename")==FALSE & exists("mat.raw")==FALSE) newrdata()
	if(exists("filename")==TRUE & exists("mat.raw")==FALSE)
	{
		filename<<-filename
		if(exists("mat")==TRUE) mat.raw<<-mat else mat.raw<<-mat.raw
		if(exists("mat1")==TRUE) mat.align<<-mat1 else mat.align<<-mat.align
		rxref<<-rxref
		fact<<-fact
		param<<-param
	    div<<-as.matrix(div)
		alig<<-alig
		if(exists("mat2")==TRUE) mat.background	<<-mat2  else mat.background<<-mat.background
		if(exists("mat3")==TRUE) mat.baseline		<<-mat3  else mat.baseline<<-mat.baseline
		if(exists("mat5")==TRUE) mat.range			<<-mat5  else mat.range<<-mat.range
		if(exists("mat6")==TRUE) mat.analyse		<<-mat6  else mat.analyse<<-mat.analyse
		if(exists("mat7")==TRUE) mat.normalise		<<-mat7  else mat.normalise<<-mat.normalise
		if(exists("mat8")==TRUE) mat.rebuilt			<<-mat8  else mat.rebuilt<<-mat.rebuilt
		if(exists("mat9")==TRUE) mat.binary			<<-mat9  else mat.binary<<-mat.binary 
	}
	####  Requires
	tclRequire("BWidget")                                   ## load BWidget package (R 2.7)
	
	####  Main window
	MainMenu <- tktoplevel()
	
	closeAllConnections()  
	
	tktitle(MainMenu) <- "StatFingerprints: Processing and statistical analysis of molecular fingerprint profiles"
	FrameMainMenu <- tkframe(MainMenu, relief="groove", borderwidth=5, background="black")
	
	####  Files menu
	topMenuFile <- tk2menubutton(FrameMainMenu, text="File")
	FileMenu <- tk2menu(topMenuFile, tearoff=FALSE)
	tkconfigure(topMenuFile, menu=FileMenu)  
	
	####  Sub-menu Files Import
	opensubmenu <- tk2menu(FileMenu, tearoff=FALSE)
	opensubmenu3 <- tk2menu(opensubmenu, tearoff=FALSE)    
	
	tkadd(opensubmenu3,"command",label="Import ASCII files",                              command=function() importGUI1())
	tkadd(opensubmenu3,"command",label="Import an ecological table (ASCII)",              command=function() importGUI3())
	tkadd(opensubmenu3,"command",label="Convert FSA files and import",                    command=function() convertGUI())
	tkadd(opensubmenu3,"command",label="Import R objects",                    command=function() Robject2SFGUI())
	
	tkadd(FileMenu,"cascade",label="New project...", menu=opensubmenu)
	tkadd(opensubmenu,"cascade",label="Import fingerprint profiles ...", menu=opensubmenu3)
	
	tkadd(FileMenu,"command",label="Load project",                                          command=function()  loadGUI())
	tkadd(FileMenu,"command",label="Save project as...",                                    command=function()  saveasGUI())
	tkadd(FileMenu,"command",label="Quit StatFingerprints",                                 command=function()  askquitGUI())
	tkadd(opensubmenu,"command",label="Import variables (quantitatives or qualitatives)",   command=function()  importGUI2())
	
	####  Edit menu
	topMenuEdit <- tk2menubutton(FrameMainMenu, text="Edit")
	EditMenu <- tk2menu(topMenuEdit, tearoff=FALSE)
	tkconfigure(topMenuEdit, menu=EditMenu)
	tkadd(EditMenu,"command",label="  Change names of profiles",command=function() change.nameGUI())  
	tkadd(EditMenu,"command",label="  Merge two projects",command=function() mergeGUI())
	tkadd(EditMenu,"command",label="  Delete profiles within the project",command=function() deleteGUI())       
	tkadd(EditMenu,"command",label="  Select profiles using levels of factor",command=function() deletefactGUI())       
	
	####  profiles processing menu
	topMenuSignal <- tk2menubutton(FrameMainMenu, text="Profile processing")
	SignalMenu <- tk2menu(topMenuSignal, tearoff=FALSE)
	tkconfigure(topMenuSignal, menu=SignalMenu)
	
	####  Sub-menu Data Transformation Rox
	opensubmenu <- tk2menu(SignalMenu, tearoff=FALSE)
	tkadd(opensubmenu,"command",label="Define peaks using your own reference standard", command=function()  roxnewGUI())
	tkadd(opensubmenu,"command",label="Use peaks of ROX defined in the file Rox.ref", command=function()  roxdefault())
	tkadd(SignalMenu,"cascade",label="  1 Step : define standard",menu=opensubmenu)
	
	opensubm <- tk2menu(SignalMenu, tearoff=FALSE)
	tkadd(opensubm,"command",label="Align profiles one by one", command=function()  alignGUI())
	tkadd(opensubm,"command",label="(Option : check quality of alignement)", command=function()  plot3dimGUI())
	tkadd(SignalMenu,"cascade",label="  2 Step : align profiles to the standard", menu=opensubm)
	
	opensub <- tk2menu(SignalMenu, tearoff=FALSE)	
	tkadd(opensub,"command",label="Define a common baseline for all profiles", command=function()  baselineGUI())
	tkadd(SignalMenu,"cascade",label="  3 Step : define a common baseline for all profiles", menu=opensub)
	
	tkadd(SignalMenu,"command",label="  4 Step : define the range of the profiles", command=function()  trunckGUI())
	tkadd(SignalMenu,"command",label="     (Option : rebuild peaks of profiles with defects)", command=function() make.peakGUI())
	
	tkadd(SignalMenu,"command",label="  5 Step : normalise area under profiles", command=function()  normalisationGUI())
	tkadd(SignalMenu,"command",label="     (Option : Delete background under profiles)", command=function()  delete.backgroundGUI())
	tkadd(SignalMenu,"command",label="     (Option : transform profiles into presence/absence profiles)", command=function()  binaryGUI())
	
	
	####  Plot menu  
	topMenuPlot <- tk2menubutton(FrameMainMenu, text="Plot")
	PlotMenu <- tk2menu(topMenuPlot,tearoff=FALSE)
	tkconfigure(topMenuPlot,menu=PlotMenu)
	opensubmenu12 <- tk2menu(PlotMenu, tearoff=FALSE)
	tkadd(opensubmenu12,"command",label="In 2 dimensions",                command=function()  plot2dGUI ())
	tkadd(opensubmenu12,"command",label="In 3 dimensions",                command=function()  plot3dimGUI ())
	tkadd(PlotMenu,"cascade",label="Plot profiles",menu=opensubmenu12) 
	opensubmenu11 <- tk2menu(PlotMenu, tearoff=FALSE)
	tkadd(opensubmenu11,"command",label="In 2 dimensions",                command=function()  plotordGUI ())
	tkadd(opensubmenu11,"command",label="In 3 dimensions",                command=function()  plotord3dGUI ())
	tkadd(PlotMenu,"cascade",label="Plot saved nMDS vs PCA: advanced tools",menu=opensubmenu11)
	tkadd(PlotMenu,"command",label="Two-way factor plot",command=function() graphfactGUI())
	
	####  Univariate statistics menu
	topMenuUniStat <- tk2menubutton(FrameMainMenu, text="Univariate statistics: diversity index")
	UniStatMenu <- tk2menu(topMenuUniStat,tearoff=FALSE)
	tkconfigure(topMenuUniStat,menu=UniStatMenu)
	tkadd(UniStatMenu,"command",label="Compute diversity index",	         command=function()  diversitiesGUI())
	tkadd(UniStatMenu,"command",label="Descriptive statistics",              command=function()  des.univGUI())
	tkadd(UniStatMenu,"command",label="Multifactor ANOVA",                   command=function()  anovaGUI())
	tkadd(UniStatMenu,"command",label="Simple correlation",                  command=function()  correlationGUI())
	
	####  Multivariate statistics menu
	topMenuMultiStat <- tk2menubutton(FrameMainMenu, text="Multivariate statistics: structure")
	MultiStatMenu <- tk2menu(topMenuMultiStat,tearoff=FALSE)
	tkconfigure(topMenuMultiStat,menu=MultiStatMenu)
	tkadd(MultiStatMenu,"command",label="Explorative statistic: ordination methods")
	tkadd(MultiStatMenu,"command",label="  Non-Metric Multidimensional Scaling (nMDS)",    command=function()  best.nmdsGUI ())
	tkadd(MultiStatMenu,"command",label="  Principal Components Analysis (PCA)",           command=function()  pcaGUI())
	tkadd(MultiStatMenu,"command",label="  Comparison of PCA/nMDS",                        command=function()  best.ordinationGUI ())
	tkadd(MultiStatMenu,"command",label="")
	tkadd(MultiStatMenu,"command",label="Explorative statistic: dendrogram methods")
	tkadd(MultiStatMenu,"command",label="  Hierarchical clustering",                       command=function()  dendroGUI())
	tkadd(MultiStatMenu,"command",label="  Heatmap",                                       command=function()  heat.mapGUI())
	tkadd(MultiStatMenu,"command",label="")
	tkadd(MultiStatMenu,"command",label="Statistical test with factor")
	tkadd(MultiStatMenu,"command",label="  Multivariate ANOVA (50-50 F-test & rotation)",  command=function()  permanovaGUI())  
	opensubmenu10 <- tk2menu(topMenuMultiStat, tearoff=FALSE)
	tkadd(opensubmenu10,"command",label="  Global ANOSIM: test effect of a qualitative variable",                 command=function()   globalanosimGUI())
	tkadd(opensubmenu10,"command",label="  Pairwise ANOSIM: test effect of levels within a qualitative variable", command=function()   pwanosimGUI())
	tkadd(MultiStatMenu,"cascade",label="  ANalysis Of SIMilarity (ANOSIM)",menu=opensubmenu10)
	tkadd(MultiStatMenu,"command",label="  Within-group variability",                                   command=function()  withingroupvariabilityGUI())
	tkadd(MultiStatMenu,"command",label="")
	tkadd(MultiStatMenu,"command",label="Define area of profile which differed between two groups")
	tkadd(MultiStatMenu,"command",label="  SIMilarity PERcentages procedure (SIMPER)",                  command=function()  simperGUI())
	tkadd(MultiStatMenu,"command",label="  Iterative tests (t test/Mann-Whitney/Fisher's exact)",       command=function()  iterative.testGUI())
	tkadd(MultiStatMenu,"command",label="")
	tkadd(MultiStatMenu,"command",label="Statistical test with parameter")
	tkadd(MultiStatMenu,"command",label="  Multivariate correlation (50-50 F-test & rotation)",         command=function()  ffcorGUI())
	tkadd(MultiStatMenu,"command",label="  Redundancy analysis (rda)",         command=function()  rdaGUI())
	tkadd(MultiStatMenu,"command",label="  Constrained correspondence analysis (cca)",         command=function()  ccaGUI())
	tkadd(MultiStatMenu,"command",label="  Constrained analysis of proximities (cap)",         command=function()  capscaleGUI())
	
  tkadd(MultiStatMenu,"command",label="")
	tkadd(MultiStatMenu,"command",label="Export proximity matrix", command=function() export.distGUI())
	
	####  Help
	topMenuHelp <- tk2menubutton(FrameMainMenu, text="Help")
	HelpMenu <- tk2menu(topMenuHelp,tearoff=FALSE)
	tkconfigure(topMenuHelp,menu=HelpMenu)
	tkadd(HelpMenu,"command",label="User manual",               command=function()  oppdf())
	tkadd(HelpMenu,"command",label="About StatFingerprints",    command=function()  hsscp())
	tkadd(HelpMenu,"command",label="How to cite in publications",    command=function()  cit())
  tkadd(HelpMenu,"command",label="Bug report",                command=function()  bug())
	
	#####  Sub-fonction menu Help
	hsscp <- function(){print(help(StatFingerprints))}
	bug <- function()  {tkmessageBox(message="Please send an email at StatFingerprints@gmail.com with details about the error")}
	cit<-function() {tkmessageBox(message="Please cite StatFingerprints in your publication. Michelland RJ, Dejean S, Combes S, Lamothe L & Cauquil L (2009) Statfingerprints: a friendly graphical interface program for microbial fingerprint profiles processing and analysis. Molecular Ecology Resources 9: 1359-1363")
                  oppdfcit()}
	####  tkpack
	tkpack(topMenuFile,topMenuEdit,topMenuSignal,topMenuPlot,topMenuUniStat,topMenuMultiStat,topMenuHelp,side="left")
	
	FrameMain <- tkframe(MainMenu)
	
	#### Logos
	logos <- tkframe(MainMenu)  
	path.logo.R <- file.path(paste(.libPaths()[1], "/StatFingerprints/Rlogo.GIF",sep=""))
	image.logo.R <- tkimage.create("photo", file = path.logo.R)
	Rlabel <- tklabel(logos, image = image.logo.R)
	path.logo.tcltk <- file.path(paste(.libPaths()[1], "/StatFingerprints/tcltk.GIF",sep=""))
	image.logo.tcltk <- tkimage.create("photo", file = path.logo.tcltk)
	tcltklab <- tklabel(logos, image = image.logo.tcltk)
	kk<-tklabel(logos,text="")
	tkpack(Rlabel,tcltklab,kk,side="left")
	
	change.current.dir <- function()
	{
		locate.new.dir <-if (interactive()) choose.dir(getwd(), "Select a new current directory")
		setwd(locate.new.dir)
	}
	
	##  Change current directory
	etat.current.dir <- tkframe(MainMenu)
	current.dir.txt <- tktext(etat.current.dir,bg="#d8d8d8", width=40,height=1,fg="dark green")
	current.dir.lab <- tklabel(etat.current.dir,text="          Current directory: ")
	current.dir.but<-tkbutton(etat.current.dir,text="Change current directory",command=change.current.dir)
	tkpack(current.dir.lab,current.dir.txt,current.dir.but,side="left")
	
	##  Filename   
	filename.txt <- tktext(FrameMain,bg="#d8d8d8", width=81,height=1,fg="dark green")
	filename.lab <- tklabel(FrameMain,text="          Project: ")
	tkpack(filename.lab,filename.txt,side="left")
	
	##  Aligned profiles
	etat.aligned <- tkframe(MainMenu) 
	aligned.profil.txt <- tktext(etat.aligned,bg="#d8d8d8", width=10,height=1,fg="dark green")
	aligned.profil.lab <-tklabel(etat.aligned,text="          Aligned profiles: ")
	tkpack(aligned.profil.lab,aligned.profil.txt,side="left")
	
	export.matrix<-function()
	{
		if(sum(mat.raw$profil[1,])==length(mat.raw$profil[1,]))  tkmessageBox(message="No profiles to export")
		else
		{
			tt <- tktoplevel()
			tkwm.title(tt,"Choose matrix to import")
			rb1 <- tkradiobutton(tt)
			rb2 <- tkradiobutton(tt)
			rb3 <- tkradiobutton(tt)
			rb4 <- tkradiobutton(tt)
			rb5 <- tkradiobutton(tt)
			rb6 <- tkradiobutton(tt)
			rb7 <- tkradiobutton(tt)
			rbValue <- tclVar("mat.normalise")
			tkconfigure(rb1,variable=rbValue,value="mat.raw")
			tkconfigure(rb2,variable=rbValue,value="mat.align")
			tkconfigure(rb3,variable=rbValue,value="mat.background")
			tkconfigure(rb4,variable=rbValue,value="mat.baseline")
			tkconfigure(rb5,variable=rbValue,value="mat.range")
			tkconfigure(rb6,variable=rbValue,value="mat.normalise")
			tkconfigure(rb7,variable=rbValue,value="mat.binary")
			tkgrid(tklabel(tt,text="      Which matrix to export ?           "))
			tkgrid(tklabel(tt,text="mat.raw "),rb1)
			tkgrid(tklabel(tt,text="mat.align "),rb2)
			tkgrid(tklabel(tt,text="mat.background "),rb3)
			tkgrid(tklabel(tt,text="mat.baseline"),rb4)
			tkgrid(tklabel(tt,text="mat.range "),rb5)
			tkgrid(tklabel(tt,text="mat.normalise"),rb6)
			tkgrid(tklabel(tt,text="mat.binary "),rb7)
		
			OnOK <- function()
			{
				rbVal <- as.character(tclvalue(rbValue))
				tkdestroy(tt)
				if (rbVal=="mat.raw")
				{
					if(sum(mat.raw$profil[1,])==length(mat.raw$profil[1,]))  tkmessageBox(message="No profiles imported")
					else
					{
						fileName<-tclvalue(tkgetSaveFile())
						filename<-paste(fileName,".csv",sep="")
						nomscol=c("Sample",colnames(div))
						write.table(t(nomscol),file=filename,sep=";",dec=".",row.names=F,col.names=F)
						write.table(t(mat.raw$profil[1,]),file=filename,sep=";",dec=".", col.names=F,append=T)
					}
				}
			
				if (rbVal=="mat.align")
				{
					if(alig[1]==1) tkmessageBox(message="No aligned profile to import")
					else
					{
						fileName<-tclvalue(tkgetSaveFile())
						filename<-paste(fileName,".csv",sep="")
						write.table(t(mat.align),file=filename,sep=";",dec=".")
						if(alig[1]!=1 & length(which(c(alig==rownames(mat.raw$profil))))!=c(dim(mat.raw$profil)[1]) & length(which(c(alig==rownames(mat.raw$profil))))!=0) tkmessageBox(message="Be careful some profiles are not aligned")
					}
				}
			
				if (rbVal=="mat.background")
				{
					if(exists("mat.background")==TRUE & mat.background[1,1]==1) tkmessageBox(message="Background has not been deleted")
					else
					{
						fileName<-tclvalue(tkgetSaveFile())
						filename<-paste(fileName,".csv",sep="")
						write.table(t(mat.background),file=filename,sep=";",dec=".") 	
					}
				}
			
				if (rbVal=="mat.baseline")
				{
					if(exists("mat.baseline")==TRUE & mat.baseline[1,1]==1) tkmessageBox(message="Baseline has not been proceed")
					else
					{
						fileName<-tclvalue(tkgetSaveFile())
						filename<-paste(fileName,".csv",sep="")
						write.table(t(mat.baseline),file=filename,sep=";",dec=".") 	
					}
				}
			
				if (rbVal=="mat.range")
				{
					if(exists("mat.range")==TRUE & mat.range[1,1]==1) tkmessageBox(message="Range treatment has not been proceed")
					else
					{
						fileName<-tclvalue(tkgetSaveFile())
						filename<-paste(fileName,".csv",sep="")
						write.table(t(mat.range),file=filename,sep=";",dec=".") 	
					}
				}	
			
				if (rbVal=="mat.normalise")
				{
					if(exists("mat.normalise")==TRUE & mat.normalise[1,1]==1) tkmessageBox(message="Normalisation treatment has not been proceed")
					else
					{
						fileName<-tclvalue(tkgetSaveFile())
						filename<-paste(fileName,".csv",sep="")
						write.table(t(mat.normalise),file=filename,sep=";",dec=".") 	
					}
				}
			
				if (rbVal=="mat.binary")
				{
					if(exists("mat.binary")==TRUE & mat.binary[1,1]==1) tkmessageBox(message="Binary matrix does not exist")
					else
					{
						fileName<-tclvalue(tkgetSaveFile())
						filename<-paste(fileName,".csv",sep="")
						write.table(t(mat.binary),file=filename,sep=";",dec=".") 	
					}
				}
			
			}
			OK.but <- tkbutton(tt,text="Export matrix",command=OnOK)
			tkgrid(OK.but)
			tkfocus(tt)
		}
	}
	
	## Profiles imported
	etat.profil <- tkframe(MainMenu)
	mat.raw.txt <- tktext(etat.profil,bg="#d8d8d8", width=10,height=1,fg="dark green")
	mat.raw.lab <- tklabel(etat.profil,text="          Imported profiles: ")
	mat.raw.but <- tkbutton(etat.profil,text="Edit", command=change.nameGUI)
	mat.raw.but2 <- tkbutton(etat.profil,text="Export matrix", command=export.matrix)
	tkpack(mat.raw.lab,mat.raw.but,mat.raw.txt,mat.raw.but2,side="left")
	
	##  Diversity
	edit.div<-function()
	{
		div<<-edit(div)
	}
	
	edit.summary.div<-function()
	{
		if(sum(div[1,],na.rm=TRUE)!=length(div[1,]))  print(summary(div))
		if(sum(div[1,],na.rm=TRUE)==length(div[1,])) tkmessageBox(message="No diversity index calculated")
	}  
	
	etat.div<-tkframe(MainMenu) 
	div.txt <- tktext(etat.div,bg="#d8d8d8", width=10,height=1,fg="dark green")
	div.lab<-tklabel(etat.div,text="          Diversity index: ")
	div.but<-tkbutton(etat.div,text="Edit",command=edit.div)
	div.but2<-tkbutton(etat.div,text="  Summary of diversity indexes",command=edit.summary.div)
	tkpack(div.lab,div.but,div.txt,div.but2,side="left")
	
	##  Factors
	edit.fact<-function()
	{
		fact<<-edit(fact)
	}
	edit.summary.fact<-function()
	{
		if (is.factor(fact[1,1])==TRUE) print(summary(fact))
		if (is.factor(fact[1,1])==FALSE) tkmessageBox(message="No qualitative variable is imported")
	}
	etat.fact<-tkframe(MainMenu) 
	fact.txt <- tktext(etat.fact,bg="#d8d8d8", width=10,height=1,fg="dark green")
	fact.lab<-tklabel(etat.fact,text="          Imported qualitative variables: ")
	fact.but<-tkbutton(etat.fact,text="Edit",command=edit.fact)
	fact.but2<-tkbutton(etat.fact,text="  Number of samples per level of qualitative variable",command=edit.summary.fact)
	tkpack(fact.lab,fact.but,fact.txt,fact.but2,side="left")
	
	##  Parameter
	edit.param<-function()
	{
		param<<-edit(param)
	}
	edit.summary.param<-function()
	{
		if(sum(param[1,],na.rm=TRUE)!=length(param[1,]))	print(summary(param))
		if(sum(param[1,],na.rm=TRUE)==length(param[1,])) tkmessageBox(message="No qualitative variable is imported")
	}
	
	etat.param<-tkframe(MainMenu) 
	param.txt <- tktext(etat.param,bg="#d8d8d8", width=10,height=1,fg="dark green")
	param.lab<-tklabel(etat.param,text="          Imported quantitative variables: ")
	param.but<-tkbutton(etat.param,text="Edit",command=edit.param)
	param.but2<-tkbutton(etat.param,text="  Summary of quantitative variable",command=edit.summary.param)
	tkpack(param.lab,param.but,param.txt,param.but2,side="left")
	
	##  Reference standard
	etat.rox<-tkframe(MainMenu) 
	rox.txt <- tktext(etat.rox,bg="#d8d8d8", width=5,height=1,fg="dark green")
	rox.lab<-tklabel(etat.rox,text="          Reference standard: ")
	tkpack(rox.lab,rox.txt,side="left")
	
	##  Background
	etat.background<-tkframe(MainMenu) 
	mat.background.txt <- tktext(etat.background,bg="#d8d8d8", width=5,height=1,fg="dark green")
	mat.background.lab<-tklabel(etat.background,text="          Background deleted: ")
	tkpack(mat.background.lab,mat.background.txt,side="left")
	
	##  Baseline
	etat.baseline<-tkframe(MainMenu) 
	mat.baseline.txt <- tktext(etat.baseline,bg="#d8d8d8", width=5,height=1,fg="dark green")
	mat.baseline.lab<-tklabel(etat.baseline,text="          Common baseline: ")
	tkpack(mat.baseline.lab,mat.baseline.txt,side="left")
	
	##  Range
	etat.range<-tkframe(MainMenu)
	mat.range.txt <- tktext(etat.range,bg="#d8d8d8", width=5,height=1,fg="dark green")
	mat.range.lab<-tklabel(etat.range,text="          Range defined: ")
	tkpack(mat.range.lab,mat.range.txt,side="left")
	
	##  Normalize
	etat.normalise<-tkframe(MainMenu)
	mat.normalise.txt <- tktext(etat.normalise,bg="#d8d8d8", width=5,height=1,fg="dark green")
	mat.normalise.lab<-tklabel(etat.normalise,text="          Normalized: ")
	tkpack(mat.normalise.lab,mat.normalise.txt,side="left")
	
	##  Binary
	etat.binary<-tkframe(MainMenu)
	mat.binary.txt <- tktext(etat.binary,bg="#d8d8d8", width=5,height=1,fg="dark green")
	mat.binary.lab<-tklabel(etat.binary,text="          Binary profiles: ")
	tkpack(mat.binary.lab,mat.binary.txt,side="left")
	
	##  Display windows
	etat1<-tklabel(MainMenu,text=" ")
	etat2<-tkframe(MainMenu)
	er1<-tklabel(etat2,text="")
	er2<-tklabel(etat2,text="")
	tkpack(er1,er2,side="left")
	etat3<-tkframe(MainMenu)
	er11<-tklabel(etat3,text="")
	er21<-tklabel(etat3,text="")
	tkpack(er11,er21,side="left")
	tkgrid(FrameMainMenu)
	tkgrid(etat1)
	tkgrid(logos)
	tkgrid(etat2)
	tkgrid( FrameMain,sticky="w")
	tkgrid(tklabel(MainMenu,text=""))
	tkgrid(etat.current.dir,sticky="w" )
	tkgrid(tklabel(MainMenu,text=""))
	tkgrid(etat.profil,sticky="w" )
	tkgrid( etat.fact,sticky="w")
	tkgrid(etat.param,sticky="w" )
	tkgrid(tklabel(MainMenu,text=""))
	tkgrid(etat.rox,sticky="w"  )
	tkgrid(etat.aligned,sticky="w" )
	tkgrid( etat.baseline,sticky="w")
	tkgrid( etat.range,sticky="w") 
	tkgrid( etat.normalise,sticky="w")
	tkgrid(etat.background,sticky="w")
	tkgrid( etat.binary,sticky="w")
	tkgrid(tklabel(MainMenu,text=""))
	tkgrid( etat.div,sticky="w")
	tkgrid(etat3)
	
	.write<-function()
	{
		tkconfigure(current.dir.txt, state="normal")
		tkdelete(current.dir.txt,"0.0","100000.0")
		tkinsert(current.dir.txt, "end", getwd())
		tkconfigure(current.dir.txt, state="disabled")
		
		tkconfigure(filename.txt, state="normal")
		tkdelete(filename.txt,"0.0","100000.0")
		tkinsert(filename.txt, "end", filename)
		tkconfigure(filename.txt, state="disabled")
		
		tkconfigure(aligned.profil.txt, state="normal")
		tkdelete(aligned.profil.txt,"0.0","100000.0")
		if(alig[1]==1) tkinsert(aligned.profil.txt, "end", "no")
		if(alig[1]!=1 & length(which(c(alig==rownames(mat.raw$profil))==FALSE))==dim(mat.raw$profil)[1]) tkinsert(aligned.profil.txt, "end", "complete")
		if(alig[1]!=1 & length(which(c(alig==rownames(mat.raw$profil))))!=c(dim(mat.raw$profil)[1]) & length(which(c(alig==rownames(mat.raw$profil))))!=0)  tkinsert(aligned.profil.txt, "end", paste(length(which((alig!= rownames(mat.raw$profil))))," to ",length(alig),sep=""))
		if(alig[1]!=1 & length(which(c(alig==rownames(mat.raw$profil))))==c(dim(mat.raw$profil)[1]))  tkinsert(aligned.profil.txt, "end", "no")
		tkconfigure(aligned.profil.txt, state="disabled") 
		
		tkconfigure(mat.raw.txt, state="normal")
		tkdelete(mat.raw.txt,"0.0","100000.0")
		if(sum(mat.raw$profil[1,])==length(mat.raw$profil[1,])) tkinsert(mat.raw.txt, "end", "no")
		if(sum(mat.raw$profil[1,])!=length(mat.raw$profil[1,])) tkinsert(mat.raw.txt, "end", dim(mat.raw$profil)[1])
		tkconfigure(mat.raw.txt, state="disabled")  
		
		tkconfigure(div.txt, state="normal")
		tkdelete(div.txt,"0.0","100000.0")
		if(sum(div[1,],na.rm=TRUE)!=length(div[1,]))  tkinsert(div.txt, "end", dim(div)[2]) 
		if(sum(div[1,],na.rm=TRUE)==length(div[1,]))  tkinsert(div.txt, "end", "no")  
		tkconfigure(div.txt, state="disabled")    
		
		tkconfigure(fact.txt, state="normal")
		tkdelete(fact.txt,"0.0","100000.0")
		if(is.factor(fact[1,1])==TRUE) tkinsert(fact.txt, "end", dim(fact)[2])
		if(is.factor(fact[1,1])==FALSE) tkinsert(fact.txt, "end", "no")
		tkconfigure(fact.txt, state="disabled")  
		
		tkconfigure(param.txt, state="normal")
		tkdelete(param.txt,"0.0","100000.0")
		if(sum(param[1,],na.rm=TRUE)!=length(param[1,]))  tkinsert(param.txt, "end", dim(param)[2])
		if(sum(param[1,],na.rm=TRUE)==length(param[1,])) tkinsert(param.txt, "end", "no")
		tkconfigure(param.txt, state="disabled")  
		
		tkconfigure(rox.txt, state="normal")
		tkdelete(rox.txt,"0.0","100000.0")
		if(rxref[1]!=0) tkinsert(rox.txt, "end", "yes")
		if(rxref[1]==0) tkinsert(rox.txt, "end", "no")
		tkconfigure(rox.txt, state="disabled") 
		
		tkconfigure(mat.background.txt, state="normal")
		tkdelete(mat.background.txt,"0.0","100000.0")
		if(exists("mat.background")==TRUE & mat.background[1,1]!=1) tkinsert(mat.background.txt, "end", "yes")
		if(exists("mat.background")==TRUE & mat.background[1,1]==1) tkinsert(mat.background.txt, "end", "no")
		if(exists("mat.background")==FALSE) tkinsert(rox.txt, "end", "no")
		tkconfigure(mat.background.txt, state="disabled") 
		
		tkconfigure(mat.baseline.txt, state="normal")
		tkdelete(mat.baseline.txt,"0.0","100000.0")
		if(exists("mat.baseline")==TRUE & mat.baseline[1,1]!=1) tkinsert(mat.baseline.txt, "end", "yes")
		if(exists("mat.baseline")==TRUE & mat.baseline[1,1]==1) tkinsert(mat.baseline.txt, "end", "no")
		if(exists("mat.baseline")==FALSE) tkinsert(mat.baseline.txt, "end", "no")
		tkconfigure(mat.baseline.txt, state="disabled")
		
		tkconfigure(mat.range.txt, state="normal")
		tkdelete(mat.range.txt,"0.0","100000.0")
		if(exists("mat.range")==TRUE & mat.range[1,1]!=1) tkinsert(mat.range.txt, "end", "yes")
		if(exists("mat.range")==TRUE & mat.range[1,1]==1) tkinsert(mat.range.txt, "end", "no")
		if(exists("mat.range")==FALSE) tkinsert(mat.range.txt, "end", "no")
		tkconfigure(mat.range.txt, state="disabled")  
		
		tkconfigure(mat.normalise.txt, state="normal")
		tkdelete(mat.normalise.txt,"0.0","100000.0")
		if(exists("mat.normalise")==TRUE & mat.normalise[1,1]!=1) tkinsert(mat.normalise.txt, "end", "yes")
		if(exists("mat.normalise")==TRUE & mat.normalise[1,1]==1) tkinsert(mat.normalise.txt, "end", "no")
		if(exists("mat.normalise")==FALSE) tkinsert(mat.normalise.txt, "end", "no")
		tkconfigure(mat.normalise.txt, state="disabled")  
		
		tkconfigure(mat.binary.txt, state="normal")
		tkdelete(mat.binary.txt,"0.0","100000.0")
		if(exists("mat.binary")==TRUE & mat.binary[1,1]!=1) tkinsert(mat.binary.txt, "end", "yes")
		if(exists("mat.binary")==TRUE & mat.binary[1,1]==1) tkinsert(mat.binary.txt, "end", "no")
		if(exists("mat.binary")==FALSE) tkinsert(mat.binary.txt, "end", "no")
		tkconfigure(mat.binary.txt, state="disabled")     
	}
	
	run <- function()
	{
		code <- tclvalue(tkget(txt,"0.0","end"))
		e <- try(parse(text=code))
		if (inherits(e, "try-error"))
		{
			tkmessageBox(message="Syntax error",icon="error")
			return()
		}

		.write()
		print(eval(e))
		.write()
	}
	
	tkfocus(MainMenu)
	tkbind(filename.txt, "<Motion>",.write)
	tkbind(filename.txt, "<Control-Return>",run)
	tkbind(mat.raw.txt, "<Motion>",.write)
	tkbind(mat.raw.txt, "<Control-Return>",run)
	tkbind(div.txt, "<Motion>",.write)
	tkbind(div.txt, "<Control-Return>",run)
	tkbind(fact.txt, "<Motion>",.write)
	tkbind(fact.txt, "<Control-Return>",run)
	tkbind(param.txt, "<Motion>",.write)
	tkbind(param.txt, "<Control-Return>",run)
	tkbind(rox.txt, "<Motion>",.write)
	tkbind(rox.txt, "<Control-Return>",run)
	tkbind(mat.background.txt, "<Motion>",.write)
	tkbind(mat.background.txt, "<Control-Return>",run)
	tkbind(mat.baseline.txt, "<Motion>",.write)
	tkbind(mat.baseline.txt, "<Control-Return>",run)
	tkbind(mat.range.txt, "<Motion>",.write)
	tkbind(mat.range.txt, "<Control-Return>",run)
	tkbind(mat.normalise.txt, "<Motion>",.write)
	tkbind(mat.normalise.txt, "<Control-Return>",run)
	tkbind(mat.binary.txt, "<Motion>",.write)
	tkbind(mat.binary.txt, "<Control-Return>",run)
	tkbind(aligned.profil.txt, "<Motion>",.write)
	tkbind(aligned.profil.txt, "<Control-Return>",run)   
	MainMenu<<-MainMenu
}

