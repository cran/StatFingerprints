##################################################################################
#    GUI for the "transform profiles into presence/absence profiles" function    #
##################################################################################

"binaryGUI"<-function()
{
  tt <- tktoplevel()
  tkwm.title(tt,"Transform into presence/absence fingerpint profiles")
  tkgrid(tklabel(tt,text="                                                                                                                                                                       "))
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
      peakparameters(prof=mat6[sel,],radius=radius,int=int,lim=lim,digit=digit)}
      nwpl<-function(){x11()}
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
  slider1<- tkentry(t1,width=8,textvariable=s1)
  tkpack(text1,slider1,side="left")
  tkgrid(t1)
  
  t2<-tkframe(tt)
  text2<-tklabel(t2,text="Delete peaks with height under (per mille)")
  s2 <- tclVar("5")
  slider2<- tkentry(t2,width=8,textvariable=s2)
  tkpack(text2,slider2,side="left")
  tkgrid(t2)

  t3<-tkframe(tt)
  text3<-tklabel(t3,text="Wide peak area")
  s3 <- tclVar("8")
  slider3<- tkentry(t3,width=8,textvariable=s3)
  tkpack(text3,slider3,side="left")
  tkgrid(t3)

  t4<-tkframe(tt)
  text4<-tklabel(t4,text="Interval size (in scans)")
  s4 <- tclVar("5")
  slider4<- tkentry(t4,width=8,textvariable=s4)
  tkpack(text4,slider4,side="left")
  tkgrid(t4)

  tkgrid(tklabel(tt,text=""))

  calc<-function()
  {
    if(mat1[1,1]!=1) m<-mat1
    if(mat2[1,1]!=1) m<-mat2
    if(mat3[1,1]!=1) m<-mat3
    if(mat4[1,1]!=1) m<-mat4
    if(mat5[1,1]!=1) m<-mat5
    if(mat7[1,1]!=1) m<-mat7
    if(mat8[1,1]!=1) m<-mat8
      
    radius=as.numeric(tclvalue(s1))
    lim=as.numeric(tclvalue(s2))
    digit=as.numeric(tclvalue(s3))
    int=as.numeric(tclvalue(s4))
    mat9<-binary(mat=m,radius=radius,int=int,lim=lim,digit=digit)
    mat9<<-mat9
    mat6<<-mat9
    print("Fingerprint profiles successfully transformed into presence absence profiles")
    tkfocus(MainMenu)
    
  }

  tkgrid(tklabel(tt,text=""))
  t6<-tkframe(tt)
  b1<-tkbutton(t6,text="Transform into presence/absence fingerprint profiles",command=calc)
  
  close<-function()
  {
    tkdestroy(tt)
  }
  
  b3<-tkbutton(t6,text="Cancel",command=close)
  
  tkpack(b1,b3,side="left")
  tkgrid(t6)
  tkgrid(tklabel(tt,text=""))
  tkfocus(tt)
}