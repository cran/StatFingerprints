############################################
#    "Hierarchical clustering" function    #
############################################

"dendoGUI"<-function()
{
  tt<-tktoplevel()
  tkwm.title(tt,"Hierarchical clustering")
  tkgrid(tklabel(tt,text="                                                                                         "))
 
  tkgrid(tklabel(tt,text="Choose the proximity index to compare fingerpint profiles"))
  index<-c("Euclidean (distance)","Maximum (distance)","Manhattan (distance)","Canberra (distance)","Minkowski (distance)","Bray Curtis (similarity -with abundances)","Chi-squared (similarity -with abundances)", "Ruzicka (similarity -with abundances)","Roberts (similarity -with abundances)","Jaccard (similarity -presence/absence)","Dyce-Sorensen (similarity -presence/absence)","Ochiai (similarity -presence/absence)","Steinhaus (similarity -presence/absence)")
  index1<-tkwidget(tt,"ComboBox",editable=FALSE,values=index,width=40,height=13)
  tkgrid(index1)

  tkgrid(tklabel(tt,text="Choose the algorithm to plot the hierarchical clustering"))
  alg<-c("Ward","Single","Complete","Average","McQuitty","Median","Centroid")
  alg1<-tkwidget(tt,"ComboBox",editable=FALSE,values=alg,width=40,height=7)
  tkgrid(alg1)
  tkgrid(tklabel(tt,text="     "))

  calc<-function()
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

    
    meth <- unlist(as.numeric(tcl(alg1, "getvalue")) + 1)
    if (meth == 1) methx <- "ward"
    if (meth == 2) methx <- "single"
    if (meth == 3) methx <- "complete"
    if (meth == 4) methx <- "average"
    if (meth == 5) methx <- "mcquitty"
    if (meth == 6) methx <- "median"
    if (meth == 7) methx <- "centroid"
    

    plot(hclust(d=newdist(mat6,index), method = methx, members=NULL),hang=-1,main="Dendrogram",ylab="Level of similarity")
  }

  t11<-tkframe(tt)
  b1<-tkbutton(t11,text="Plot",command=calc)
  
  nw<-function()
  {
    x11() 
  }
  
  b2<- tkbutton(t11, text = "New plot window...", command = nw)
  
  close<-function()
  {
    tkdestroy(tt)
  }
  
  b3<-tkbutton(t11,text="Cancel",command=close)
  tkpack(b1,b2,b3,side="left")
  tkgrid(t11)
  tkgrid(tklabel(tt,text="   "))
  tkfocus(tt)
}