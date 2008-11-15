########################################################
#    GUI for the "Compute diversity index" function    #
########################################################
                                                        
"diversitiesGUI"<-function()
{
  tt <- tktoplevel()
  tkwm.title(tt,"Diversity index estimation")
  tkgrid(tklabel(tt,text="                                                                                           "))
  tkgrid(tklabel(tt,text="Characteristics of the detection of the peaks"))
  paramm<-function()
  {
    trr <- tktoplevel()
    tkwm.title(trr,"What is the radius of the rollball")
    tkgrid(tklabel(trr,text=""))
    zzz<-file.path(paste(.libPaths(), "/StatFingerprints/radius.GIF",sep=""))
    icnn<-tkimage.create("photo", file = zzz)
    tcltklab <- tklabel(trr, image = icnn)
    tkgrid(tcltklab)
    tkgrid(tklabel(trr,text=""))
  
    close<-function()
    {
      tkdestroy(trr)
    }
 
    tkgrid(tkbutton(trr,text="Cancel",command=close))
    tkgrid(tklabel(trr,text=""))
  
    tr <- tktoplevel()
    tkgrid(tklabel(tr,text="                                                                                                                                   "))
    tkwm.title(tr,"Help to define characteristics of peak detection")
    tkgrid(tklabel(tr,text="Characteristics of the detection of the peaks"))
         
    t1<-tkframe(tr)
    text1<-tklabel(t1,text="Radius of the rollball?")
    s1 <- tclVar("10")
    slider1 <- tkentry(t1,width=8,textvariable=s1)
    tkpack(text1,slider1,side="left")
    tkgrid(t1)
           
    t2<-tkframe(tr)
    text2<-tklabel(t2,text="Delete peaks with height under (per mille)")
    s2 <- tclVar("5")
    slider2 <- tkentry(t2,width=8,textvariable=s2)
    tkpack(text2,slider2,side="left")
    tkgrid(t2)
                  
    t3<-tkframe(tr)
    text3<-tklabel(t3,text="Wide peak area")
    s3 <- tclVar("8")
    slider3 <- tkentry(t3,width=8,textvariable=s3)
    tkpack(text3,slider3,side="left")
    tkgrid(t3)
                 
    t4<-tkframe(tr)
    text4<-tklabel(t4,text="Interval size (in scans)")
    s4 <- tclVar("5")
    slider4 <- tkentry(t4,width=8,textvariable=s4)
    tkpack(text4,slider4,side="left")
    tkgrid(t4)
                
    tkgrid(tklabel(tr,text="   "))
    scr <- tkscrollbar(tr, repeatinterval=5,command=function(...)tkyview(tl,...))
    tl<-tklistbox(tr,height=10,width =50 ,selectmode="single",yscrollcommand=function(...)tkset(scr,...),background="white")
    tkgrid(tklabel(tr,text="Which profile do you want to plot?"))
    tkgrid(tklabel(tr,text=""))
    tkgrid(tl,scr)
    tkgrid.configure(scr,rowspan=10,sticky="nsw")
    prof <- rownames(mat$profil)
    for (i in 1:length(prof)) 
    {
      tkinsert(tl,"end",prof[i])
    }
    tkselection.set(tl,0)
          
    paramm1<-function()
    {
      radius=as.numeric(tclvalue(s1))
      lim=as.numeric(tclvalue(s2))
      digit=as.numeric(tclvalue(s3))
      int=as.numeric(tclvalue(s4))
      sel<- as.numeric(tkcurselection(tl))+1 
      peakparameters(prof=mat6[sel,],radius=radius,int=int,lim=lim,digit=digit)
    }
    
    nwpl<-function()
    {
      x11()
    }
    
    tkgrid(tklabel(tr,text=""))
    tr1<-tkframe(tr)
    b1<-tkbutton(tr1,text="plot",command=paramm1)
    b2<-tkbutton(tr1,text="new windows",command=nwpl)    
 
    close<-function()
    {
      tkdestroy(tr)
    }
    
    b3<-tkbutton(tr1,text="Cancel",command=close)
    tkpack(b1,b2,b3,side="left")
    tkgrid(tr1)
    tkgrid(tklabel(tr,text=""))
    tkfocus(tr)
  }

  tkgrid(tkbutton(tt,text="Help to define characteristics of peak detection",command=paramm))

  t1<-tkframe(tt)
  text1<-tklabel(t1,text="Radius of the rollball?")
  s1 <- tclVar("10")
  slider1 <- tkentry(t1,width=8,textvariable=s1)
  tkpack(text1,slider1,side="left")
  tkgrid(t1)
  
  t2<-tkframe(tt)
  text2<-tklabel(t2,text="Delete peaks with height under (per mille)")
  s2 <- tclVar("5")
  slider2 <- tkentry(t2,width=8,textvariable=s2)
  tkpack(text2,slider2,side="left")
  tkgrid(t2)
    
  t3<-tkframe(tt)
  text3<-tklabel(t3,text="Wide peak area")
  s3 <- tclVar("8")
  slider3 <- tkentry(t3,width=8,textvariable=s3)
  tkpack(text3,slider3,side="left")
  tkgrid(t3)
  
  t4<-tkframe(tt)
  text4<-tklabel(t4,text="Interval size (in scans)")
  s4 <- tclVar("5")
  slider4 <- tkentry(t4,width=8,textvariable=s4)
  tkpack(text4,slider4,side="left")
  tkgrid(t4)
  
  t5<-tkframe(tt)  
  text5<-tklabel(t5,text="Method of calculation")
  sep<-c("Use height of peaks","use area under peaks")
  sepe<-tkwidget(t5,"ComboBox",editable=FALSE,values=sep,height=2)
  tkpack(text5,sepe,side="left")
  tkgrid(t5)
   
  tkgrid(tklabel(tt,text=""))
    
  t7<-tkframe(tt)
  text7<-tklabel(t7,text="Choose the index of diversity")
  ind<- c("Peak_number", "-log Simpson","1-simpson", "Shannon", "Buzas & Gibson's evenness","Equitability") 
  tl<-tkwidget(t7,"ComboBox",editable=FALSE,values=ind,height=length(ind))
  tkpack(text7,tl,side="left")
  tkgrid(t7)

  calc<-function()
  {
    radius=as.numeric(tclvalue(s1))
    lim=as.numeric(tclvalue(s2))
    digit=as.numeric(tclvalue(s3))
    int=as.numeric(tclvalue(s4))
   
    sep1<-unlist(as.numeric(tcl(sepe,"getvalue"))+1 )
      if(sep1==1) method<-"high of peaks"
      if(sep1==2) method<-"area"
    
    index<- unlist(as.numeric(tcl(tl,"getvalue"))+1 )
      if (mat9[1,1]!=1 & index!=1) tkmessageBox(message="You can only compute peak number index as you have presence/absence data")
      if (mat9[1,1]!=1 & index!=1) stop("You can only compute peak number index as you have presence/absence data")
    
    div<-diversities(mat=mat6,index=index,radius=radius,int=int,lim=lim,digit=digit,method=method)
    print(paste("Calculated diversity was:",div))
    div<<-div
    vec<-matrix(nc=2,nr=length(div))
    vec[,1]<-names(div)
    vec[,2]<-div
    colnames(vec)=c("Name of the profile","Diversity index")
    vec<-edit(vec)
    vec1<-vector(length=dim(vec)[1])
    vec1<-as.numeric(vec[,2])
    names(vec1)<-vec[,1]
    div<<-vec1 
    print("Diversity index successfully estimated") 
  }

#### Export diversities index
  exportfic<-function()
  {
    fileName<-tclvalue(tkgetSaveFile())
    filename<-paste(fileName,".csv",sep="")
    write.table(div,file=filename,sep=";",dec=".")
    tkmessageBox(message="Diversity successfully saved")
  }
  
  tkgrid(tklabel(tt,text=""))
  tkgrid(tkbutton(tt,text="Calculate diversity index",command=calc))
  tkgrid(tklabel(tt,text=""))
  tkgrid(tkbutton(tt,text="Export the last calculated diversity index",command=exportfic))

  close<-function()
  {
    tkdestroy(tt)
  }
  
  tkgrid(tklabel(tt,text=""))
  tkgrid(tkbutton(tt,text="Cancel",command=close))
  tkgrid(tklabel(tt,text="")) 
  tkfocus(tt)
}