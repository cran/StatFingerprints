globalanosimGUI <-
function ()
{
	checkprofile()
	checkfact()
	
	tt <- tktoplevel()
	tkwm.title(tt, "Global ANalysis Of SIMilarity")
	
	tkgrid(tklabel(tt, text = "                                                                                      "))
	
	tt2<-tkframe(tt)
	text2<-tklabel(tt2, text = "Test on the structure the effect of:")
	repee <- tkwidget(tt2, "ComboBox", editable = FALSE, values = names(fact),height=length(names(fact)),width=20)
	tkpack(text2,repee,side="left")
	tkgrid(tt2)
	
	tkgrid(tklabel(tt, text = "       "))
	tkgrid(tklabel(tt,text="Choose the proximity index to compare fingerpint profiles"))
	index<-c("Euclidean (distance)","Maximum (distance)","Manhattan (distance)","Canberra (distance)","Minkowski (distance)","Pearson (correlation)","Bray Curtis (similarity -with abundances)","Chi-squared (similarity -with abundances)", "Ruzicka (similarity -with abundances)","Roberts (similarity -with abundances)","Jaccard (similarity -presence/absence)","Dice-Sorensen (similarity -presence/absence)","Ochiai (similarity -presence/absence)","Steinhaus (similarity -presence/absence)")
	index1<-tkwidget(tt,"ComboBox",editable=FALSE,values=index,width=40,height=14)
	tkgrid(index1)
	
	tt1<-tkframe(tt)
	text1<-tklabel(tt1,text="Number of MC permutations?")
	it <- tclVar("10")
	slider1 <- tkentry(tt1,width=8,textvariable=it)
	
	tkpack(text1,slider1,side="left")
	tkgrid(tt1)
	tkgrid(tklabel(tt, text = "       "))
	
	compute.anosim <- function() 
	{
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
		
		facte <- unlist(as.numeric(tcl(repee, "getvalue")) + 1)
		nb=as.numeric(tclvalue(it))
		res=anosim_sf(dat=newdist(mat.analyse,index), grouping=fact[,facte], permutations = nb,wait=1)
		dev.off()
		print(res)
	}
	
	ff<-tkframe(tt)
	b1<- tkbutton(ff, text = "Compute", command = compute.anosim)
	b2<-tkbutton(ff,text="Cancel",command=function() tkdestroy(tt))
	tkpack(b1,b2,side="left")
	tkgrid(ff)
	tkgrid(tklabel(tt,text="   "))
	tkfocus(tt)
}

