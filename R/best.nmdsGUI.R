###########################################################################
#    GUI for the "Non-Metric Multidimensional Scaling (nMDS)" analysis    #
###########################################################################

"best.nmdsGUI"<-function()
{
  tt<-tktoplevel()
  tkwm.title(tt,"Compute a non-metric Multidimensional Scaling  with random starts")
  tkgrid(tklabel(tt,text="                                                                                                                                                                         "))
  tkgrid(tklabel(tt,text="Choose the proximity index to compare fingerpint profiles"))
  index<-c("Euclidean (distance)","Maximum (distance)","Manhattan (distance)","Canberra (distance)","Minkowski (distance)","Bray Curtis (similarity -with abundances)","Chi-squared (similarity -with abundances)", "Ruzicka (similarity -with abundances)","Roberts (similarity -with abundances)","Jaccard (similarity -presence/absence)","Dyce-Sorensen (similarity -presence/absence)","Ochiai (similarity -presence/absence)","Steinhaus (similarity -presence/absence)")
  index1<-tkwidget(tt,"ComboBox",editable=FALSE,values=index,width=40,height=13)
  tkgrid(index1)
  tkgrid(tklabel(tt,text=""))  
  
  tt1<-tkframe(tt)
  text1<-tklabel(tt1,text="Number of dimension?")
  nb <- tclVar("2")
  dimens <- tkentry(tt1,width=8,textvariable=nb)
  tkpack(text1,dimens,side="left")
  tkgrid(tt1)
  
  tt2<-tkframe(tt)
  text2<-tklabel(tt2,text="Number of random start?")
  it <- tclVar("100")
  starte <- tkentry(tt2,width=8,textvariable=it)
  tkpack(text2,starte,side="left")
  tkgrid(tt2)
    
  tt3<-tkframe(tt)
  text3<-tklabel(tt3, text = "Return label")
  repe <- c("Yes", "No")
  repee <- tkwidget(tt3, "ComboBox", editable = FALSE, values = repe,width=8,height=2)
  tkpack(text3,repee,side="left")
  tkgrid(tt3)

  cal<-function()
  {
    diste <- unlist(as.numeric(tcl(index1, "getvalue")) + 1)
    if (diste == 1) index <- "euclidean"
    if (diste == 2) index <- "maximum"
    if (diste == 3) index <- "manhattan"
    if (diste == 4) index <- "canberra"
    if (diste == 5) index <- "minkowski"
    if (diste == 6) index <- "bray/curtis"
    if (diste == 7) index <- "chisq"
    if (diste == 8) index <- "ruzicka"
    if (diste == 9) index <- "roberts"
    if (diste == 10) index <-"jaccard"
    if (diste == 11) index <-"sorensen"
    if (diste == 12) index <-"ochiai"
    if (diste == 13) index <-"steinhaus"
    if (diste >= 10 & mat9[1,1]==1) tkmessageBox(message="This index works with presence/absence data. You can either transform your profiles into presence/absence profiles (profile processing menu) or use another index")
    if (diste >= 10 & mat9[1,1]==1) stop(message="This index works with presence/absence data. You can either transform your profiles into presence/absence profiles (profile processing menu) or use another index")
    nb<- as.numeric(tclvalue(nb))
    if (nb==1) tkmessageBox(message="Please select 2 or more dimensions")
    if (nb==1) stop(message="Please select 2 or more dimensions")    
    itr<-as.numeric(tclvalue(it))
    repe1 <- unlist(as.numeric(tcl(repee, "getvalue")) + 1)
    tt1 <- tktoplevel()
    tkwm.title(tt1,"Working")
    tkgrid(tklabel(tt1,font="arial 12",text="Please wait...\n This operation may take several minutes                           "))
    tkfocus(tt1)
    tkconfigure(tt1,cursor="watch")
    bestnmds<-best.nmds(mat=mat6,index=index,k=nb,itr=itr)
    tkdestroy(tt1)
    bestnmds<<-bestnmds
    if (nb==2) plot(bestnmds$points[,1],bestnmds$points[,2],xlab="1st PC",ylab="2nd PC",sub=paste("Stress=",c(round(bestnmds$stress,digit=3))),main="Non-metric multidimensional scaling")
    if(repe1==1 & nb==2) text(bestnmds$points[,1],bestnmds$points[,2],rownames(bestnmds$points))
    if (nb>=3) plot3d(bestnmds$points[,1],bestnmds$points[,2],bestnmds$points[,3],type="s",size=0.5,col="red",xlab="1st PC",ylab="2nd PC", zlab="3rd PC",sub=paste("Stress=",c(round(bestnmds$stress,digit=3))),main="Non-metric multidimensional scaling")
    tkmessageBox(message="You can improve your nMDS plot: save the nMDS and use Plot saved nMDS function in the Plot menu") 
  }
  
  tkgrid(tklabel(tt,text="   "))
  tkgrid(tkbutton(tt,text="Compute nMDS",command=cal) )
  
  save.nmds<-function()
  {
    fileName<-tclvalue(tkgetSaveFile())
    filename<-paste(fileName,".Rdata",sep="")
    save(bestnmds,file=filename)
    print("Your nMDS has been successfully saved")
    tkdestroy(tt)
  }

  tkgrid(tklabel(tt,text="   "))
  tkgrid(tkbutton(tt,text="Save the nMDS",command=save.nmds))
  
  close<-function()
  {
    tkdestroy(tt)
  }
  
  tkgrid(tklabel(tt,text="   "))
  tkgrid(tkbutton(tt,text="Cancel",command=close))
  tkgrid(tklabel(tt,text="   "))
  tkgrid(tklabel(tt,text="Note: advanced tools to plot nMDS is available on the Plot menu"))
  tkfocus(tt)
}