####################################
#    GUI for "Heatmap" function    #
####################################

"heat.mapGUI"<-function()
{
  tt<-tktoplevel()
  tkwm.title(tt,"Plot a heatmap")
  tkgrid(tklabel(tt,text="                                                                                         "))
 
  tkgrid(tklabel(tt,text="Choose the proximity index to compare fingerpint profiles"))
  index<-c("Euclidean (distance)","Maximum (distance)","Manhattan (distance)","Canberra (distance)","Minkowski (distance)","Bray Curtis (similarity -with abundances)","Chi-squared (similarity -with abundances)", "Ruzicka (similarity -with abundances)","Roberts (similarity -with abundances)","Jaccard (similarity -presence/absence)","Dyce-Sorensen (similarity -presence/absence)","Ochiai (similarity -presence/absence)","Steinhaus (similarity -presence/absence)")
  index1<-tkwidget(tt,"ComboBox",editable=FALSE,values=index,width=40,height=13)
  tkgrid(index1)

  tkgrid(tklabel(tt,text="Choose the algorithm to compute the hierarchical clustering of the heatmap"))
  alg<-c("Ward","Single","Complete","Average","McQuitty","Median","Centroid")
  alg1<-tkwidget(tt,"ComboBox",editable=FALSE,values=alg,width=40,height=7)
  tkgrid(alg1)
   
  tkgrid(tklabel(tt,text="Choose the color of the heatmap"))  
  colo<-c("Blues","Greens","Greys","Oranges","Purples","Reds","Orange/Red","Blue/Green")
  colo1<-tkwidget(tt,"ComboBox",editable=FALSE,values=colo,width=40,height=8)
  tkgrid(colo1)
  tkgrid(tklabel(tt,text="     "))

  calc<-function()
  {
    colo <- unlist(as.numeric(tcl(colo1, "getvalue")) + 1)
    if (colo == 1) myp <- brewer.pal(9,"Blues" )
    if (colo == 2) myp <- brewer.pal(9,"Greens")
    if (colo == 3) myp <- brewer.pal(9,"Greys")
    if (colo == 4) myp <- brewer.pal(9,"Oranges")
    if (colo == 5) myp <- brewer.pal(9,"Purples")
    if (colo == 6) myp <- brewer.pal(9,"Reds")
    if (colo == 7) myp <- brewer.pal(9,"OrRd")
    if (colo == 8) myp <- brewer.pal(9,"BuGn") 

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
    heat.map(b=mat6,index=index,algo=methx,myp=myp)
  }

  tt11<-tkframe(tt)
  b1<-tkbutton(tt11,text="Plot",command=calc)

  close<-function()
  {
    tkdestroy(tt)
  }

  b2<-tkbutton(tt11,text="Cancel",command=close)
  tkpack(b1,b2,side="left")
  tkgrid(tt11)
  tkgrid(tklabel(tt,text="   "))
  tkfocus(tt)
}