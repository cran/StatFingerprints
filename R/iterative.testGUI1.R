iterative.testGUI1 <-
function (fact,ae,mat.binary,mat.analyse)
{
	t2 <- tktoplevel()
	tkwm.title(t2, "Iterative test")
	tkgrid(tklabel(t2, text = "                             "))
	
	tt2<-tkframe(t2)
	text2<-tklabel(tt2, text = "First level")
	repee <- tkwidget(tt2, "ComboBox", editable = FALSE, values = levels(fact[,ae]),height=length(levels(fact[,ae])))
	tkpack(text2,repee,side="left")
	tkgrid(tt2)
	
	tt3<-tkframe(t2)
	text3<-tklabel(tt3, text = "Second level")
	repee1 <- tkwidget(tt3, "ComboBox", editable = FALSE, values = levels(fact[,ae]),height=length(levels(fact[,ae])))
	tkpack(text3,repee1,side="left")
	tkgrid(tt3)
	
	tt4<-tkframe(t2)
	text4<-tklabel(tt4, text = "Choose your iterative test")
	test <- tkwidget(tt4, "ComboBox", editable = FALSE, values = c("t-test (parametric)", "Mann Whitney (non-parametric)","Fisher's exact (presence/absence profiles)"),width=40,height=3)
	tkpack(text4,test,side="left")
	tkgrid(tt4)
	a<<-1  
	
	compute.iterative <- function() 
	{
		a<<-0
		repee <- unlist(as.numeric(tcl(repee, "getvalue"))+1)
		repee1 <- unlist(as.numeric(tcl(repee1, "getvalue"))+1)
		test <- unlist(as.numeric(tcl(test, "getvalue"))+1)
		if (test!=3 & mat.binary[1,1]!=1) tkmessageBox(message="You can only compute Fisher's exact test as you have presence/absence profiles") 
		if (test!=3 & mat.binary[1,1]!=1) stop("You can only compute Fisher's exact test as you have presence/absence profiles") 
		if (test==3 & mat.binary[1,1]==1) tkmessageBox(message="You can not compute Fisher's exact test as you have qualitative profiles") 
		if (test==3 & mat.binary[1,1]==1) stop("You can not compute Fisher's exact test as you have qualitative profiles") 
		
		niv1=levels(fact[,ae])
		niv<<-c(niv1[repee],niv1[repee1])
		a=iterative.test(profil=mat.analyse,fact1=fact[,ae],level=niv,method=test)
	}                                                         
	
	tkgrid(tklabel(t2, text = ""))
	tt5<-tkframe(t2)
	b1<- tkbutton(tt5, text = "Compute", command = compute.iterative)
	b2<-tkbutton(tt5,text="Cancel",command=function() tkdestroy(t2))
	
	view.distribution<-function()
	{
		if (a==1) tkmessageBox(message="First press the button Compute")
		if (a==1) stop("First press the button Compute")
		loc = round(locator(1, type = "p", pch = 4)[[1]],digit=0)
		n1<-which(fact[,ae]==niv[1])
		n2<-which(fact[,ae]==niv[2])
		proan<-mat.analyse[c(n1,n2),loc]
		factt<-fact[c(n1,n2),ae]
		factt<-factor(factt)
		x11()
		boxplot(proan~factt,main=paste("Boxplot of the scan ",loc,sep=""),ylab="Signal intensity")
	}
	
	b3<-tkbutton(t2,text="Visualize distribution of a scan",command=view.distribution)
	
	tkpack(b1,b2,side="left")
	tkgrid(tt5)
	tkgrid(tklabel(t2, text = ""))
	tkgrid(b3)
	tkgrid(tklabel(t2, text = ""))
	tkfocus(t2)
}

