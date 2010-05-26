withingroupvariabilityGUI <-
function()
{
	checkprofile()
	checkfact()
	
	tt <- tktoplevel()
	tkwm.title(tt, "Within-group variability")
	tkgrid(tklabel(tt, text = "                                     "))
	
	tt2<-tkframe(tt)
	text2<-tklabel(tt2, text = "Test on the structure the effect of:")
	repee <- tkwidget(tt2, "ComboBox", editable = FALSE, values = names(fact),height=length(names(fact)),width=20)
	tkpack(text2,repee,side="left")
	tkgrid(tt2)
	tkgrid(tklabel(tt, text = " "))
	
	tkgrid(tklabel(tt,text="Choose the proximity index to compare fingerpint profiles"))
	index<-c("Euclidean (distance)","Maximum (distance)","Manhattan (distance)","Canberra (distance)","Minkowski (distance)","Pearson (correlation)","Bray Curtis (similarity -with abundances)","Chi-squared (similarity -with abundances)", "Ruzicka (similarity -with abundances)","Roberts (similarity -with abundances)","Jaccard (similarity -presence/absence)","Dice-Sorensen (similarity -presence/absence)","Ochiai (similarity -presence/absence)","Steinhaus (similarity -presence/absence)")
	index1<-tkwidget(tt,"ComboBox",editable=FALSE,values=index,width=40,height=14)
	tkgrid(index1)
	tkgrid(tklabel(tt, text = " "))
	
	tkgrid(tklabel(tt,text="Plot representation"))
	indexx<-c("Boxplot","Points and SD","Lines and SD")
	indexx1<-tkwidget(tt,"ComboBox",editable=FALSE,values=indexx,width=40,height=3)
	tkgrid(indexx1)
	tkgrid(tklabel(tt, text = " "))
	
	compute.withingroupvariability <- function() 
	{
		sel  <- unlist(as.numeric(tcl(repee, "getvalue")) + 1)
		diste <- unlist(as.numeric(tcl(index1, "getvalue")) + 1)
		if (diste == 1) index <- "euclidean"
		if (diste == 2) index <- "maximum"
		if (diste == 3) index <- "manhattan"
		if (diste == 4) index <- "canberra"
		if (diste == 5) index <- "minkowski"
		if (diste == 6) index <- "Pearson"
		if (diste == 7) index <- "bray/curtis"
		if (diste == 8) index <- "chisq"
		if (diste == 9) index <- "ruzicka"
		if (diste == 10) index <- "roberts"
		if (diste == 11) index <-"jaccard"
		if (diste == 12) index <-"sorensen"
		if (diste == 13) index <-"ochiai"
		if (diste == 14) index <-"steinhaus"
		if (diste >= 11 & mat.binary[1,1]==1) tkmessageBox(message="This index works with presence/absence data. You can either transform your profiles into presence/absence profiles (profile processing menu) or use another index")
		if (diste >= 11 & mat.binary[1,1]==1) stop(message="This index works with presence/absence data. You can either transform your profiles into presence/absence profiles (profile processing menu) or use another index")
		
		metth <- unlist(as.numeric(tcl(indexx1, "getvalue")) + 1)
		sel<<-sel
		withingroupvariability(mat=mat.analyse,fact1=fact[,sel],index=index,metth=metth)
		tkfocus(tt)
	}
	
	az<-tkframe(tt)
	b1<- tkbutton(az, text = "Compute", command = compute.withingroupvariability)
	b2<-tkbutton(az,text="Cancel",command=function() tkdestroy(tt))
	tkpack(b1,b2,side="left")
	tkgrid(az)
	tkgrid(tklabel(tt,text="   "))
	tkfocus(tt)
}

