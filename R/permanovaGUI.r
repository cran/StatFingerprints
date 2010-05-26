permanovaGUI <-
function()
{
	checkprofile()
	checkfact()
	
	tt<- tktoplevel()
	tkwm.title(tt, "Compute 50-50 MANOVA")
	tkgrid(tklabel(tt, text = "                                                                                      "))
	tkgrid(tklabel(tt, text = "Design your MANOVA"))
	t1<-tkframe(tt)
	text1<-tklabel(t1, text = "First qualitative variable")
	repee <- tkwidget(t1, "ComboBox", editable = FALSE, values = names(fact),height=length(names(fact)))
	tkpack(text1,repee,side="left")
	tkgrid(t1)
	
	t2<-tkframe(tt)
	text2<-tklabel(t2, text = "Link")
	repee1 <- tkwidget(t2, "ComboBox", editable = FALSE, values = c("Single factor","Interaction","Single factor + interaction"),height=3)
	tkpack(text2,repee1,side="left")
	tkgrid(t2)
	
	t3<-tkframe(tt)
	text3<-tklabel(t3, text = "Second qualitative variable")
	repee2 <- tkwidget(t3, "ComboBox", editable = FALSE, values = names(fact),height=length(names(fact)))
	tkpack(text3,repee2,side="left")
	tkgrid(t3)
	
	anol=matrix(nr=15,nc=3)
	colnames(anol)=c("1 fact","link","2 fact")
	i=0
	i<<-0
	j<<-i 
	
	create.model <- function()
	{
		facte <- unlist(as.numeric(tcl(repee, "getvalue")) + 1)
		inter <- unlist(as.numeric(tcl(repee1, "getvalue")) + 1)
		facte2 <- unlist(as.numeric(tcl(repee2, "getvalue")) + 1)
		i<<-j
		j<<-i+1 
		anol[j,1]<-facte
		anol[j,2]<-inter
		anol[j,3]<-facte2
		anol<<-anol
		ano<-na.omit(anol)
		
		for (k in 1:dim(ano)[1])
		{
			aa<-c("+",":","*")
			for (z in 1:length(names(fact)))
			{
				if(ano[k,1]==z) ano[k,1]<-names(fact)[z]
			}
			for (z in 1:3)
			{
				if(ano[k,2]==z) ano[k,2]<-aa[z]
			}
			for (z in 1:length(names(fact)))
			{
				if(ano[k,3]==z) ano[k,3]<-names(fact)[z]
			}
		}
		
		ano<-paste(ano[,1],ano[,2],ano[,3],sep="")
		ano<-as.vector(rbind(ano,rep("+",length(ano))))[1:c((length(ano)*2)-1)]
		ano<-paste(ano[1],ano[2],ano[3],ano[4],ano[5],ano[6],ano[7],ano[8] ,ano[9],ano[10],ano[11],ano[12],ano[13],ano[14],ano[15],ano[16],ano[17],ano[18] ,ano[19],ano[20],sep="")
		ano<-strsplit(ano, split="NA")[[1]][1]
		ano4<-paste("mat.analyse","~",ano,sep="")
		ano3<-formula(ano4)
		ano2<<-ano3
		tkgrid(tklabel(tt, text = c(paste("N",j,":",paste("Profils","~",ano,sep="")))))
	}
	
	reset.model<-function()
	{
		tkdestroy(tt)
		permanovaGUI()
	}
	
	ttt11<-tkframe(tt)
	b1<- tkbutton(ttt11, text = "Add element to the design of ANOVA", command = create.model)
	b2<- tkbutton(ttt11, text = "Reset the design of ANOVA", command = reset.model)
	tkpack(b1,b2,side="left")
	tkgrid(ttt11)
	tkgrid(tklabel(tt, text = ""))
	
	tt1<-tkframe(tt)
	text1<-tklabel(tt1,text="Number of rotations?")
	nb <- tclVar("10")
	slider1 <- tkentry(tt1,width=8,textvariable=nb)
	tkpack(text1,slider1,side="left")
	tkgrid(tt1)
	
	tkgrid(tklabel(tt, text = ""))
	
	compute.permanova<-function()
	{
		nb=as.numeric(tclvalue(nb))	
		z<-ffmanova_sf(ano2,data=fact,nSim=nb,stand=FALSE)
		print(z)
	}
	
	ttt12<-tkframe(tt)
	b1 <- tkbutton(ttt12, text = "Compute the 50-50 ANOVA", command =compute.permanova)
	b2<-tkbutton(ttt12,text="Cancel",command=function() tkdestroy(tt))
	tkpack(b1,b2,side="left")
	tkgrid(ttt12)
	tkgrid(tklabel(tt, text = ""))
	tkgrid(tklabel(tt, text = paste("see http://www.langsrud.com/stat/ffmanova.htm")))
	tkgrid(tklabel(tt, text = ""))
	tkgrid(tklabel(tt, text = c("Your 50-50 MANOVA formula is:")))
	tkfocus(tt)
}

