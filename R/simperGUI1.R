simperGUI1 <-
function ()
{
  t2 <- tktoplevel()
  tkwm.title(t2, "SIMilarity PERcentages procedure")
  tkgrid(tklabel(t2, text = "                                                                                      "))
  
  tt3<-tkframe(t2)
  text3<-tklabel(tt3, text = "First level")
  repee <- tkwidget(tt3, "ComboBox", editable = FALSE, values = levels(fact[,ae]),height=length(levels(fact[,ae])))
  tkpack(text3,repee,side="left")
  tkgrid(tt3)
  
  tt4<-tkframe(t2)
  text4<-tklabel(tt4, text = "Second level")
  repee1 <- tkwidget(tt4, "ComboBox", editable = FALSE, values = levels(fact[,ae]),height=length(levels(fact[,ae])))
  tkpack(text4,repee1,side="left")
  tkgrid(tt4)
     
  tt2<-tkframe(t2)
  text2<-tklabel(tt2,text="Threshold in permille of contribution taking into account?")
  it <- tclVar("5")
  starte <- tkentry(tt2,width=8,textvariable=it)
  tkpack(text2,starte,side="left")
  tkgrid(tt2)
  tkgrid(tklabel(t2, text = "  "))
    
  compute.simper <- function() 
  {
    repee <- unlist(as.numeric(tcl(repee, "getvalue"))+1)
    repee1 <- unlist(as.numeric(tcl(repee1, "getvalue"))+1)
    niv1=levels(fact[,ae])
    niv<-c(niv1[repee],niv1[repee1])
    nb<- as.numeric( tclvalue(it) )
    nb=as.numeric(nb)/100
    a<-simper(a=mat.analyse,fact=fact[,ae],level=niv, seuil=nb)
    simper.data<<-a 
  }                                                         
  
  export.simper<-function(a)
  {
	  fileName<-tclvalue(tkgetSaveFile())
	  filename<-paste(fileName,".csv",sep="")
	  nomscol=c("Rank",colnames(simper.data))
	  write.table(t(nomscol),file=filename,sep=";",dec=".",row.names=F,col.names=F)
	  write.table(simper.data,file=filename,sep=";",dec=".", col.names=F,append=T)
	  tkmessageBox(message="Simper data successfully exported")
  }
  
  tt5<-tkframe(t2)
  b1<- tkbutton(tt5, text = "Compute", command = compute.simper)
  b2<- tkbutton(tt5,text = "Export simper data", command = export.simper)
  b3<- tkbutton(tt5,text="Cancel",command=function() tkdestroy(t2))
  tkpack(b1,b2,b3,side="left")
  tkgrid(tt5)
  tkgrid(tklabel(t2, text = ""))
  tkfocus(t2)
  }

