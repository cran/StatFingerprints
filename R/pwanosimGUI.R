pwanosimGUI <-
function ()
{
	checkprofile()
	checkfact()
	
	tt <- tktoplevel()
	tkwm.title(tt, "Pairwise ANalysis Of SIMilarity")
	tkgrid(tklabel(tt, text = "                                                                                      "))
	
	tt2<-tkframe(tt)
	text2<-tklabel(tt2, text = "Choose the qualitative variable:")
	repee <- tkwidget(tt2, "ComboBox", editable = FALSE, values = names(fact),height=length(names(fact)),width=20)
	tkpack(text2,repee,side="left")
	tkgrid(tt2)
	
	pwanosimGUI1<-function ()
	{
		th <- tktoplevel()
		tkwm.title(th, "Pairwise ANalysis Of SIMilarity")
		tkgrid(tklabel(th, text = ""))
		tkgrid(tklabel(th, text = "1st level"))
		
		zzz=c("all pairwise Anosim",levels(fact[,ae]))
		repe <- zzz
		repee <- tkwidget(th, "ComboBox", editable = FALSE, values = repe)
		tkgrid(repee)
		
		tkgrid(tklabel(th, text = "2nd level"))
		repe1 <- zzz
		repee1 <- tkwidget(th, "ComboBox", editable = FALSE, values = repe1)
		tkgrid(repee1)
		
		tkgrid(tklabel(th,text="Choose the proximity index to compare fingerpint profiles"))
		index<-c("Euclidean (distance)","Maximum (distance)","Manhattan (distance)","Canberra (distance)","Minkowski (distance)","Pearson (correlation)","Bray Curtis (similarity -with abundances)","Chi-squared (similarity -with abundances)", "Ruzicka (similarity -with abundances)","Roberts (similarity -with abundances)","Jaccard (similarity -presence/absence)","Dice-Sorensen (similarity -presence/absence)","Ochiai (similarity -presence/absence)","Steinhaus (similarity -presence/absence)")
		index1<-tkwidget(th,"ComboBox",editable=FALSE,values=index,width=40,height=14)
		tkgrid(index1)
		
		tt2<-tkframe(th)
		text2<-tklabel(tt2,text="Number of permutations?")
		nb <- tclVar("100")
		starte <- tkentry(tt2,width=8,textvariable=nb)
		tkpack(text2,starte,side="left")
		tkgrid(tt2)
		
		compute.pwanosim <- function() 
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
			
			repee <- unlist(as.numeric(tcl(repee, "getvalue")) + 1)
			repee1 <- unlist(as.numeric(tcl(repee1, "getvalue")) + 1)
			niv=0
			niv1=levels(fact[,ae])
			niv<-c(niv1[repee-1],niv1[repee1-1])
			if (repee==1) niv<-"pw45pw"
			if (repee1==1) niv<-"pw45pw"
			nb=as.numeric(tclvalue(nb))
			
			pwanosim(mat=mat.analyse,param=fact[,ae],level=niv,index=index,permutations=nb)
			
		}                                                         
		
		ff<-tkframe(th)
		tkgrid(tklabel(th, text = ""))
		b1<- tkbutton(ff, text = "Select", command = compute.pwanosim)
		b2<-tkbutton(ff,text="Cancel",command=function() tkdestroy(th))
		tkpack(b1,b2,side="left")
		tkgrid(ff)
		tkgrid(tklabel(th, text = ""))
		tkfocus(th)
	}
	
	load.pwanosim <- function() 
	{
		sel <- unlist(as.numeric(tcl(repee, "getvalue")) + 1)
		ae<<-sel
		pwanosimGUI1()
		tkdestroy(tt)
	}
	
	tkgrid(tklabel(tt, text = " "))
	ff1<-tkframe(tt)
	b1<- tkbutton(ff1, text = "Compute", command = load.pwanosim)
	b2<-tkbutton(ff1,text="Cancel",command=function() tkdestroy(tt))
	tkpack(b1,b2,side="left")
	tkgrid(ff1)
	tkgrid(tklabel(tt, text = ""))
	tkfocus(tt)
}

