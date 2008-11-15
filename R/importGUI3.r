#######################################################
#    "Import an ecological table (ASCII)" function    #
#######################################################

"importGUI3"<-function()
{
  fil=if (interactive()) choose.files(filters = Filters["All",],caption="Select ASCII files containing your ecological table")
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
  
  mm<-function()
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
    if(hear==TRUE) rownames(m1)<-hear1
    if (trans1==2) m1<-t(m1)
    mat<-list(profil=m1,rox=m1)
    mat<<-mat
    mat6<-m1;mat6<<-mat6
    rxref<-0
    rxref<<-rxref
    alig<-vector(length=length(rownames(mat$profil))) ;alig<-rownames(mat$profil)                         ;alig<<-alig
    fact<-matrix(nr=dim(mat$profil)[1],nc=2)          ;fact[]<-1 ;rownames(fact)<-rownames(mat$profil)    ;fact<<-fact
    param<-matrix(nr=dim(mat$profil)[1],nc=2)         ;param[]<-1;rownames(param)<-rownames(mat$profil)   ;param<<-param
    div<-vector(length=dim(mat$profil)[1])            ;div[]<-1  ;names(div)<-rownames(mat$profil)        ;div<<-div 
    mat1<-matrix(nr=dim(mat$profil)[1],nc=2)          ;mat1[]<-1 ;rownames(mat1)<-rownames(mat$profil)    ;mat1<<-mat1 
    mat2<-matrix(nr=dim(mat$profil)[1],nc=2)          ;mat2[]<-1 ;rownames(mat2)<-rownames(mat$profil)    ;mat2<<-mat2  
    mat3<-matrix(nr=dim(mat$profil)[1],nc=2)          ;mat3[]<-1 ;rownames(mat3)<-rownames(mat$profil)    ;mat3<<-mat3 
    mat4<-matrix(nr=dim(mat$profil)[1],nc=2)          ;mat4[]<-1 ;rownames(mat4)<-rownames(mat$profil)    ;mat4<<-mat4
    mat5<-matrix(nr=dim(mat$profil)[1],nc=2)          ;mat5[]<-1 ;rownames(mat5)<-rownames(mat$profil)    ;mat5<<-mat5 
    mat7<-matrix(nr=dim(mat$profil)[1],nc=2)          ;mat7[]<-1 ;rownames(mat7)<-rownames(mat$profil)    ;mat7<<-mat7 
    mat8<-matrix(nr=dim(mat$profil)[1],nc=2)          ;mat8[]<-1 ;rownames(mat8)<-rownames(mat$profil)    ;mat8<<-mat8  
    mat9<-matrix(nr=dim(mat$profil)[1],nc=2)          ;mat9[]<-1 ;rownames(mat9)<-rownames(mat$profil)    ;mat9<<-mat9  
    print("Successfully loaded!")
    tkdestroy(tt)
  }
  
  tkgrid(tklabel(tt,text=" "))
  t6<-tkframe(tt)
  b1<-tkbutton(t6,text="Import ecological table",command=mm)
  
  close<-function()
  {
    tkdestroy(tt)
  }

  b2<-tkbutton(t6,text="Cancel",command=close)
  tkpack(b1,b2,side="left")
  tkgrid(t6)
  tkgrid(tklabel(tt,text=" "))
  tkfocus(tt)
}
