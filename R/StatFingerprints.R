#######################################################
#    Function to start the program StatFingerprints   #
#######################################################

"StatFingerprints"<-function()
{
  if(exists("filename")==FALSE & exists("mat")==FALSE) newrdata()
####  Requires
  require(tcltk) || stop("tcl/tk library not available")  ## load tcltk package
  require(tcltk2) || stop("tcl/tk library not available") ## load tcltk package
  tclRequire("BWidget")                                   ## load BWidget package (R 2.7)

####  Main window
  MainMenu <- tktoplevel()

  closeAllConnections()  
       
  .write<-function()
  {
    tkconfigure(filenametxt, state="normal")
    tkdelete(filenametxt,"0.0","100000.0")
    tkinsert(filenametxt, "end", filename)
    tkconfigure(filenametxt, state="disabled")

    tkconfigure(alitxt, state="normal")
    tkdelete(alitxt,"0.0","100000.0")
    if(alig[1]==1) tkinsert(alitxt, "end", "no")
    if(alig[1]!=1 & length(which(c(alig==rownames(mat$profil))==FALSE))==dim(mat$profil)[1]) tkinsert(alitxt, "end", "complete")
    if(alig[1]!=1 & length(which(c(alig==rownames(mat$profil))))!=c(dim(mat$profil)[1]) & length(which(c(alig==rownames(mat$profil))))!=0)  tkinsert(alitxt, "end", paste(length(which((alig!= rownames(mat$profil))))," to ",length(alig),sep=""))
    if(alig[1]!=1 & length(which(c(alig==rownames(mat$profil))))==c(dim(mat$profil)[1]))  tkinsert(alitxt, "end", "no")
    tkconfigure(alitxt, state="disabled") 
  
    tkconfigure(mattxt, state="normal")
    tkdelete(mattxt,"0.0","100000.0")
    if(sum(mat$profil[1,])==length(mat$profil[1,])) tkinsert(mattxt, "end", "no")
    if(sum(mat$profil[1,])!=length(mat$profil[1,])) tkinsert(mattxt, "end", dim(mat$profil)[1])
    tkconfigure(mattxt, state="disabled")  
  
    tkconfigure(divtxt, state="normal")
    tkdelete(divtxt,"0.0","100000.0")
    if(div[1]!=1)  tkinsert(divtxt, "end", length(div)) 
    if(div[1]==1)  tkinsert(divtxt, "end", "no")  
    tkconfigure(divtxt, state="disabled")    
  
    tkconfigure(facttxt, state="normal")
    tkdelete(facttxt,"0.0","100000.0")
    if(is.factor(fact[1,1])==TRUE) tkinsert(facttxt, "end", dim(fact)[2])
    if(is.factor(fact[1,1])==FALSE) tkinsert(facttxt, "end", "no")
    tkconfigure(facttxt, state="disabled")  

    tkconfigure(paramtxt, state="normal")
    tkdelete(paramtxt,"0.0","100000.0")
    if(param[1,1]!=1) tkinsert(paramtxt, "end", dim(param)[2])
    if(param[1,1]==1) tkinsert(paramtxt, "end", "no")
    tkconfigure(paramtxt, state="disabled")  

    tkconfigure(roxtxt, state="normal")
    tkdelete(roxtxt,"0.0","100000.0")
    if(rxref[1]!=0) tkinsert(roxtxt, "end", "yes")
    if(rxref[1]==0) tkinsert(roxtxt, "end", "no")
    tkconfigure(roxtxt, state="disabled") 
    
    tkconfigure(mat2txt, state="normal")
    tkdelete(mat2txt,"0.0","100000.0")
    if(exists("mat2")==TRUE & mat2[1,1]!=1) tkinsert(mat2txt, "end", "yes")
    if(exists("mat2")==TRUE & mat2[1,1]==1) tkinsert(mat2txt, "end", "no")
    if(exists("mat2")==FALSE) tkinsert(roxtxt, "end", "no")
    tkconfigure(mat2txt, state="disabled") 
    
    tkconfigure(mat3txt, state="normal")
    tkdelete(mat3txt,"0.0","100000.0")
    if(exists("mat3")==TRUE & mat3[1,1]!=1) tkinsert(mat3txt, "end", "yes")
    if(exists("mat3")==TRUE & mat3[1,1]==1) tkinsert(mat3txt, "end", "no")
    if(exists("mat3")==FALSE) tkinsert(mat3txt, "end", "no")
    tkconfigure(mat3txt, state="disabled")

    tkconfigure(mat4txt, state="normal")
    tkdelete(mat4txt,"0.0","100000.0")
    if(exists("mat5")==TRUE & mat5[1,1]!=1) tkinsert(mat4txt, "end", "yes")
    if(exists("mat5")==TRUE & mat5[1,1]==1) tkinsert(mat4txt, "end", "no")
    if(exists("mat5")==FALSE) tkinsert(mat4txt, "end", "no")
    tkconfigure(mat4txt, state="disabled")  
    
    tkconfigure(mat5txt, state="normal")
    tkdelete(mat5txt,"0.0","100000.0")
    if(exists("mat7")==TRUE & mat7[1,1]!=1) tkinsert(mat5txt, "end", "yes")
    if(exists("mat7")==TRUE & mat7[1,1]==1) tkinsert(mat5txt, "end", "no")
    if(exists("mat7")==FALSE) tkinsert(mat5txt, "end", "no")
    tkconfigure(mat5txt, state="disabled")  
    
    tkconfigure(mat6txt, state="normal")
    tkdelete(mat6txt,"0.0","100000.0")
    if(exists("mat9")==TRUE & mat9[1,1]!=1) tkinsert(mat6txt, "end", "yes")
    if(exists("mat9")==TRUE & mat9[1,1]==1) tkinsert(mat6txt, "end", "no")
    if(exists("mat9")==FALSE) tkinsert(mat6txt, "end", "no")
    tkconfigure(mat6txt, state="disabled")     
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
   #cat("Executing from script window:",
   #   "-----", code, "result:", sep="\n")
   .write()
   print(eval(e))
   .write()
  }
       
  tktitle(MainMenu)<-"StatFingerprints: processing and statistical analysis of molecular fingerprint profiles"
  FrameMainMenu <- tkframe(MainMenu, relief="groove", borderwidth=5, background="black")

####  Files menu
  topMenuFile <- tk2menubutton(FrameMainMenu, text="File")
  FileMenu <- tk2menu(topMenuFile, tearoff=FALSE)
  tkconfigure(topMenuFile, menu=FileMenu)  

####  Sub-menu Files Import
  opensubmenu <- tk2menu(topMenuFile, tearoff=FALSE)
    opensubmenu3 <- tk2menu(topMenuFile, tearoff=FALSE)
      
  
    tkadd(opensubmenu3,"command",label="Import ASCII files",                          command=function() importGUI1())
    tkadd(opensubmenu3,"command",label="Import an ecological table (ASCII)",             command=function() importGUI3())
    tkadd(opensubmenu3,"cascade",label="Convert FSA files and import",              command=function() convertGUI())
  
  tkadd(FileMenu,"cascade",label="New project...", menu=opensubmenu)
  tkadd(opensubmenu,"cascade",label="Import fingerprint profiles ...", menu=opensubmenu3)

  tkadd(FileMenu,"command",label="Load project",                                          command=function()  loadGUI())
  tkadd(FileMenu,"command",label="Save project",                                          command=function()  saveGUI())
  tkadd(FileMenu,"command",label="Save project as...",                                    command=function()  saveasGUI())
  tkadd(FileMenu,"command",label="Quit StatFingerprints",                                                command=function()  askquitGUI())
  tkadd(opensubmenu,"command",label="Import variables (quantitatives or qualitatives)",       command=function() importGUI2())

####  Edit menu
  topMenuEdit <- tk2menubutton(FrameMainMenu, text="Edit")
  EditMenu <- tk2menu(topMenuEdit, tearoff=FALSE)
  tkconfigure(topMenuEdit, menu=EditMenu)
  tkadd(EditMenu,"command",label="  Change names of profiles",                          command=function() change.nameGUI())  
  tkadd(EditMenu,"command",label="  Add profiles to the project",                       command=function() addGUI())
  tkadd(EditMenu,"command",label="  Delete profiles within the project",                    command=function() deleteGUI())       
  tkadd(EditMenu,"command",label="  Select profiles using levels of factor",                    command=function() deletefactGUI())       

####  profiles processing menu
  topMenuSignal <- tk2menubutton(FrameMainMenu, text="Profile processing")
  SignalMenu <- tk2menu(topMenuSignal, tearoff=FALSE)
  tkconfigure(topMenuSignal, menu=SignalMenu)

####  Sub-menu Data Transformation Rox
  opensubmenu <- tk2menu(topMenuSignal, tearoff=FALSE)
  tkadd(opensubmenu,"command",label="Define peaks using your own reference standard",                  command=function()  roxnewGUI())
  tkadd(opensubmenu,"command",label="Use peaks of ROX HD400 - Applied Biosystems -",       command=function()  roxdefault())
  tkadd(SignalMenu,"cascade",label="  1 Step : define standard",menu=opensubmenu)
  opensubm <- tk2menu(topMenuSignal, tearoff=FALSE)
  tkadd(opensubm,"command",label="Align profiles one by one",                             command=function()  alignGUI())        
  tkadd(opensubm,"command",label="(Option : check quality of alignement)",                command=function()  plot3dimGUI())
  tkadd(SignalMenu,"cascade",label="  2 Step : align profiles to the standard", menu=opensubm)
  opensub <- tk2menu(topMenuSignal, tearoff=FALSE)
  tkadd(opensub,"command",label="(Option : Delete background under profiles)",            command=function()  delete.backgroundGUI())
  tkadd(opensub,"command",label="Define a common baseline for all profiles",              command=function()  baselineGUI())         
  tkadd(SignalMenu,"cascade",label="  3 Step : define a common baseline for all profiles", menu=opensub)
  tkadd(SignalMenu,"command",label="  4 Step : define the range of the profiles",                     command=function()  trunckGUI())
  tkadd(SignalMenu,"command",label="     (Option : rebuild peaks of profiles with defects)",    command=function()  make.peakGUI())
  tkadd(SignalMenu,"command",label="  5 Step : normalise area under profiles",                        command=function()  normalisationGUI())
  tkadd(SignalMenu,"command",label="     (Option : transform profiles into presence/absence profiles)", command=function()  binaryGUI())

####  Plot menu  
  topMenuPlot<-tk2menubutton(FrameMainMenu, text="Plot")
  PlotMenu<-tk2menu(topMenuPlot,tearoff=FALSE)
  tkconfigure(topMenuPlot,menu=PlotMenu)
  opensubmenu12 <- tk2menu(PlotMenu, tearoff=FALSE)
  tkadd(opensubmenu12,"command",label="In 2 dimensions",                command=function()  plot2dGUI ())
  tkadd(opensubmenu12,"command",label="In 3 dimensions",                command=function()  plot3dimGUI ())
  tkadd(PlotMenu,"cascade",label="Plot profiles",menu=opensubmenu12) 
  opensubmenu11 <- tk2menu(PlotMenu, tearoff=FALSE)
  tkadd(opensubmenu11,"command",label="In 2 dimensions",                command=function()  plotordGUI ())
  tkadd(opensubmenu11,"command",label="In 3 dimensions",                command=function()  plotord3dGUI ())
  tkadd(PlotMenu,"cascade",label="Plot saved nMDS vs PCA: advanced tools",menu=opensubmenu11)

####  Univariate statistics menu
  topMenuUniStat <- tk2menubutton(FrameMainMenu, text="Univariate statistics: diversity index")
  UniStatMenu<-tk2menu(topMenuUniStat,tearoff=FALSE)
  tkconfigure(topMenuUniStat,menu=UniStatMenu)
  tkadd(UniStatMenu,"command",label="Compute diversity index",	            command=function()  diversitiesGUI())
  tkadd(UniStatMenu,"command",label="Descriptive statistics",               command=function()  des.univGUI())
  tkadd(UniStatMenu,"command",label="Multifactor ANOVA",                   command=function()  anovaGUI())
  tkadd(UniStatMenu,"command",label="Simple correlation",                  command=function()  correlationGUI())

####  Multivariate statistics menu
  topMenuMultiStat<-tk2menubutton(FrameMainMenu, text="Multivariate statistics: structure")
  MultiStatMenu<-tk2menu(topMenuMultiStat,tearoff=FALSE)
  tkconfigure(topMenuMultiStat,menu=MultiStatMenu)
  tkadd(MultiStatMenu,"command",label="Explorative statistic: ordination methods")
  tkadd(MultiStatMenu,"command",label="  Non-Metric Multidimensional Scaling (nMDS)",    command=function()  best.nmdsGUI ())
  tkadd(MultiStatMenu,"command",label="  Principal Components Analysis (PCA)",           command=function()  pcaGUI())
  tkadd(MultiStatMenu,"command",label="  Comparison of PCA/nMDS",           command=function()  best.ordinationGUI ())
  tkadd(MultiStatMenu,"command",label="")
  tkadd(MultiStatMenu,"command",label="Explorative statistic: dendrogram methods")
  tkadd(MultiStatMenu,"command",label="  Hierarchical clustering",                       command=function()  dendoGUI())
  tkadd(MultiStatMenu,"command",label="  Heatmap",                                       command=function()  heat.mapGUI())
  tkadd(MultiStatMenu,"command",label="")
  tkadd(MultiStatMenu,"command",label="Statistical test with factor")
  tkadd(MultiStatMenu,"command",label="  Multivariate ANOVA (50-50 F-test & rotation)",  command=function()  permanovaGUI())  
  opensubmenu10 <- tk2menu(topMenuMultiStat, tearoff=FALSE)
  tkadd(opensubmenu10,"command",label="  Global ANOSIM: test effect of a qualitative variable",                 command=function()   globalanosimGUI())
  tkadd(opensubmenu10,"command",label="  Pairwise ANOSIM: test effect of levels within a qualitative variable", command=function()   pwanosimGUI())
  tkadd(MultiStatMenu,"cascade",label="  ANalysis Of SIMilarity (ANOSIM)",menu=opensubmenu10)
  tkadd(MultiStatMenu,"command",label="  Within-group variability",                                 command=function()  withingroupvariabilityGUI())
  tkadd(MultiStatMenu,"command",label="")
  tkadd(MultiStatMenu,"command",label="Define area of profile which differed between two groups")
  tkadd(MultiStatMenu,"command",label="  SIMilarity PERcentages procedure (SIMPER)",                                                    command=function()  simperGUI())
  tkadd(MultiStatMenu,"command",label="  Iterative tests (t test/Mann-Whitney/Fisher's exact)",                      command=function()  iterative.testGUI())
  tkadd(MultiStatMenu,"command",label="")
  tkadd(MultiStatMenu,"command",label="Statistical test with parameter")
  tkadd(MultiStatMenu,"command",label="  Multivariate correlation (50-50 F-test & rotation)",         command=function()  ffcorGUI())
####  Help
  topMenuHelp<-tk2menubutton(FrameMainMenu, text="Help")
  HelpMenu<-tk2menu(topMenuHelp,tearoff=FALSE)
  tkconfigure(topMenuHelp,menu=HelpMenu)
  tkadd(HelpMenu,"command",label="User manual",   command=function()  oppdf())
  tkadd(HelpMenu,"command",label="About StatFingerprints",    command=function()  hsscp())
  tkadd(HelpMenu,"command",label="Bug report",    command=function()  bug())

#####Sub-fonction
  hsscp<-function(){print(help(StatFingerprints))}
  bug<-function(){  tkmessageBox(message="Please send an email at StatFingerprints@gmail.com with details about the error") }

####  tkpack
  tkpack(topMenuFile,topMenuEdit,topMenuSignal,topMenuPlot,topMenuUniStat,topMenuMultiStat,topMenuHelp,side="left")

  FrameMain <- tkframe(MainMenu)

##Filename   
  filenametxt <- tktext(FrameMain,bg="#d8d8d8", width=81,height=1,fg="dark green")
  filenamelab<-tklabel(FrameMain,text="          Project: ")
  tkpack(filenamelab,filenametxt,side="left")

#### Logos
  im1<-tkframe(MainMenu)  
  zz<-file.path(paste(.libPaths(), "/StatFingerprints/Rlogo.GIF",sep=""))
  icn<-tkimage.create("photo", file = zz)
  Rlabel <- tklabel(im1, image = icn)
  zzz<-file.path(paste(.libPaths(), "/StatFingerprints/tcltk.GIF",sep=""))
  icnn<-tkimage.create("photo", file = zzz)
  tcltklab <- tklabel(im1, image = icnn)
  kk<-tklabel(im1,text="")
  tkpack(Rlabel,tcltklab,kk,side="left")

##align
  etat00<-tkframe(MainMenu) 
  alitxt<- tktext(etat00,bg="#d8d8d8", width=10,height=1,fg="dark green")
  alilab<-tklabel(etat00,text="          Aligned profiles: ")
  tkpack(alilab,alitxt,side="left")

##nb profil
  etat0<-tkframe(MainMenu)
  mattxt <- tktext(etat0,bg="#d8d8d8", width=10,height=1,fg="dark green")
  matlab<-tklabel(etat0,text="          Imported profiles: ")
  matbut<-tkbutton(etat0,text="Edit",command=change.nameGUI)
  tkpack(matlab,matbut,mattxt,side="left")

#diversity
  e3<-function()
  {
    vec<-matrix(nc=2,nr=length(div))
    vec[,1]<-names(div)
    vec[,2]<-div
    colnames(vec)=c("Name of the profile","Diversity index")
    vec<-edit(vec)
    vec1<-vector(length=dim(vec)[1])
    vec1<-as.numeric(vec[,2])
    names(vec1)<-vec[,1]
    div<<-vec1
  }

  etat111<-tkframe(MainMenu) 
  divtxt <- tktext(etat111,bg="#d8d8d8", width=10,height=1,fg="dark green")
  divlab<-tklabel(etat111,text="          Diversity index: ")
  divbut<-tkbutton(etat111,text="Edit",command=e3)
  tkpack(divlab,divbut,divtxt,side="left")

##factors
  e1<-function()
  {
    fact<<-edit(fact)
  }
  e2<-function()
  {
    if (is.factor(fact[1,1])==TRUE) edit(summary(fact))
    if (is.factor(fact[1,1])==FALSE) tkmessageBox(message="No qualitative variables is imported")
    if (is.factor(fact[1,1])==FALSE) stop("No qualitative variables is imported")
  }
  etat01<-tkframe(MainMenu) 
  facttxt <- tktext(etat01,bg="#d8d8d8", width=10,height=1,fg="dark green")
  factlab<-tklabel(etat01,text="          Imported qualitative variables: ")
  factbut<-tkbutton(etat01,text="Edit",command=e1)
  factbut2<-tkbutton(etat01,text="Number sample per level of qualitative variable",command=e2)
  tkpack(factlab,factbut,facttxt,factbut2,side="left")

#parametre
  e2<-function()
  {
    param<<-edit(param)
  }
  etat022<-tkframe(MainMenu) 
  paramtxt <- tktext(etat022,bg="#d8d8d8", width=10,height=1,fg="dark green")
  paramlab<-tklabel(etat022,text="          Imported quantitative variables: ")
  parambut<-tkbutton(etat022,text="Edit",command=e2)
  tkpack(paramlab,parambut,paramtxt,side="left")

##reference standard
  etat1<-tkframe(MainMenu) 
  roxtxt <- tktext(etat1,bg="#d8d8d8", width=5,height=1,fg="dark green")
  roxlab<-tklabel(etat1,text="          Reference standard: ")
  tkpack(roxlab,roxtxt,side="left")

##delete background
  etat2<-tkframe(MainMenu) 
  mat2txt <- tktext(etat2,bg="#d8d8d8", width=5,height=1,fg="dark green")
  mat2lab<-tklabel(etat2,text="          Background deleted: ")
  tkpack(mat2lab,mat2txt,side="left")

##common baseline
  etat3<-tkframe(MainMenu) 
  mat3txt <- tktext(etat3,bg="#d8d8d8", width=5,height=1,fg="dark green")
  mat3lab<-tklabel(etat3,text="          Common baseline: ")
  tkpack(mat3lab,mat3txt,side="left")

#range defined
  etat4<-tkframe(MainMenu)
  mat4txt <- tktext(etat4,bg="#d8d8d8", width=5,height=1,fg="dark green")
  mat4lab<-tklabel(etat4,text="          Range defined: ")
  tkpack(mat4lab,mat4txt,side="left")

#normalize
  etat5<-tkframe(MainMenu)
  mat5txt <- tktext(etat5,bg="#d8d8d8", width=5,height=1,fg="dark green")
  mat5lab<-tklabel(etat5,text="          Normalized: ")
  tkpack(mat5lab,mat5txt,side="left")

#binary
  etat6<-tkframe(MainMenu)
  mat6txt <- tktext(etat6,bg="#d8d8d8", width=5,height=1,fg="dark green")
  mat6lab<-tklabel(etat6,text="          Binary profiles: ")
  tkpack(mat6lab,mat6txt,side="left")

  etat7<-tklabel(MainMenu,text=" ")
  etat8<-tkframe(MainMenu)
  er1<-tklabel(etat8,text="")
  er2<-tklabel(etat8,text="")
  tkpack(er1,er2,side="left")
  etat9<-tkframe(MainMenu)
  er11<-tklabel(etat9,text="")
  er21<-tklabel(etat9,text="")
  tkpack(er11,er21,side="left")

  tkgrid(FrameMainMenu)
  tkgrid(etat7)
  tkgrid(im1)
  tkgrid(etat8)
  tkgrid( FrameMain,sticky="w")
  tkgrid(etat0,sticky="w" )
  tkgrid( etat01,sticky="w")
  tkgrid(etat022,sticky="w" )
  tkgrid(etat1,sticky="w"  )
  tkgrid(etat00,sticky="w" )
  tkgrid(etat2,sticky="w")
  tkgrid( etat3,sticky="w")
  tkgrid( etat4,sticky="w") 
  tkgrid( etat5,sticky="w")
  tkgrid( etat6,sticky="w")
  tkgrid( etat7,sticky="w")
  tkgrid( etat111,sticky="w")
  tkgrid(etat9)

  tkfocus(MainMenu)
  tkbind(filenametxt, "<Motion>",.write)
  tkbind(filenametxt, "<Control-Return>",run)
  tkbind(mattxt, "<Motion>",.write)
  tkbind(mattxt, "<Control-Return>",run)
  tkbind(divtxt, "<Motion>",.write)
  tkbind(divtxt, "<Control-Return>",run)
  tkbind(facttxt, "<Motion>",.write)
  tkbind(facttxt, "<Control-Return>",run)
  tkbind(paramtxt, "<Motion>",.write)
  tkbind(paramtxt, "<Control-Return>",run)
  tkbind(roxtxt, "<Motion>",.write)
  tkbind(roxtxt, "<Control-Return>",run)
  tkbind(mat2txt, "<Motion>",.write)
  tkbind(mat2txt, "<Control-Return>",run)
  tkbind(mat3txt, "<Motion>",.write)
  tkbind(mat3txt, "<Control-Return>",run)
  tkbind(mat4txt, "<Motion>",.write)
  tkbind(mat4txt, "<Control-Return>",run)
  tkbind(mat5txt, "<Motion>",.write)
  tkbind(mat5txt, "<Control-Return>",run)
  tkbind(mat6txt, "<Motion>",.write)
  tkbind(mat6txt, "<Control-Return>",run)
  tkbind(alitxt, "<Motion>",.write)
  tkbind(alitxt, "<Control-Return>",run)   
  MainMenu<<-MainMenu
}
  
"statfingerprints"  <-function(){StatFingerprints()}
"statfingerprint"   <-function(){StatFingerprints()}                                     
"StatFingerprint"   <-function(){StatFingerprints()}
"SF"                <-function(){StatFingerprints()}
"sf"                <-function(){StatFingerprints()}
