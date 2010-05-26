capscaleGUI <-
function()
{
	checkprofile()
	checkparam()
	
	if(sum(param[1,],na.rm=TRUE)!=length(param[1,]) & sum(div[1,],na.rm=TRUE)!=length(div[1,]))
	{
		param2<-matrix(nr=dim(mat.analyse)[1],nc=dim(param)[2]+7)
		param2<-cbind(div,param)
	}
	
	if(sum(param[1,],na.rm=TRUE)!=length(param[1,]) & sum(div[1,],na.rm=TRUE)==length(div[1,]))
	{
		param2<-matrix(nr=dim(mat.analyse)[1],nc=dim(param)[2])
		param2<-param
	}
	
	if(sum(param[1,],na.rm=TRUE)==length(param[1,]) & sum(div[1,],na.rm=TRUE)!=length(div[1,]))
	{
		param2<-matrix(nr=dim(mat.analyse)[1],nc=7)
		param2<-div
	}
	
	param2<-as.data.frame(param2)
	
	param.na.rows<-matrix(nr=5,nc=2)
	param.na.rows[]<-1
	param.na.columns<-matrix(nr=5,nc=2)
	param.na.columns[]<-1
	####  NA detection  ####
	pa.na<-na.omit(param2)
	a<-attributes(pa.na)
	b<-rownames(param2[a$na.action,])
	b<<-b
	if (length(b!=0))
	{
		tkmessageBox(message="WARNING : MISSING VALUES DETECTED",icon="warning",type="ok")
		res<-tkmessageBox(message="Do you want to continue, data with missing values will be temporarely deleted",icon="question",type="okcancel",default="ok")
		
		if (tclvalue(res) == "cancel")
			stop("")
		else
		{
			tb<-tktoplevel()
			tkwm.title(tb, "Delete data with missing value(s)")
			rb1<-tkradiobutton(tb)
			rb2<-tkradiobutton(tb)
			rbValue<-tclVar("rows")
			tkconfigure(rb1,variable=rbValue,value="rows")
			tkconfigure(rb2,variable=rbValue,value="columns")
			tkgrid(tklabel(tb,text="Which data do you want to delete ?              "))
			tkgrid(tklabel(tb,text="Delete profiles with NA"),rb1)
			tkgrid(tklabel(tb,text="Delete parameters with NA"),rb2)
			
			OnOK<-function()
			{
				rbVal<-as.character(tclvalue(rbValue))
				tkdestroy(tb)
				if (rbVal=="columns")
					param.na.columns<<-param2[,-which(apply(param2,2,function(x)any(is.na(x))))]
				if (rbVal=="rows")
					param.na.rows<<-param2[-which(apply(param2,1,function(x)any(is.na(x)))),]
			}
			
			OK.but<-tkbutton(tb,text="OK",command=OnOK)
			tkgrid(OK.but)
			tkfocus(tb)
		}
		tkwait.window(tb) 
	}
	
	
	if (sum(param.na.rows[1,],na.rm=TRUE)!=length(param.na.rows[1,]))
	{
		param.na<-param.na.rows
		mat.analyse.na<<-mat.analyse[-which(apply(param2,1,function(x)any(is.na(x)))),]
	}
	if (sum(param.na.columns[1,],na.rm=TRUE)!=length(param.na.columns[1,]))
	{
		param.na<-param.na.columns
		mat.analyse.na<<-mat.analyse
	}
	
	if ((sum(param.na.rows[1,],na.rm=TRUE)==length(param.na.rows[1,])) & (sum(param.na.columns[1,],na.rm=TRUE)==length(param.na.columns[1,])))
	{
		param.na<-param2
		mat.analyse.na<<-mat.analyse
	}
	
	
	tt<- tktoplevel()
	tkwm.title(tt, "Compute CAP")
	tkgrid(tklabel(tt, text = "                                                                                      "))
	tkgrid(tklabel(tt, text = "Design your CAP"))
	t1<-tkframe(tt)
	text1<-tklabel(t1, text = "First quantitative variable")
	repee <- tkwidget(t1, "ComboBox", editable = FALSE, values = names(param.na),height=length(names(param.na)))
	tkpack(text1,repee,side="left")
	tkgrid(t1)
	
	t2<-tkframe(tt)
	text2<-tklabel(t2, text = "Link")
	repee1 <- tkwidget(t2, "ComboBox", editable = FALSE, values = c("Single factor","Interaction","Single factor + interaction"),height=3)
	tkpack(text2,repee1,side="left")
	tkgrid(t2)
	
	t3<-tkframe(tt)
	text3<-tklabel(t3, text = "Second quantitative variable")
	repee2 <- tkwidget(t3, "ComboBox", editable = FALSE, values = names(param.na),height=length(names(param.na)))
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
			for (z in 1:length(names(param.na)))
			{
				if(ano[k,1]==z)
					ano[k,1]<-names(param.na)[z]
			}
			for (z in 1:3)
			{
				if(ano[k,2]==z)
					ano[k,2]<-aa[z]
			}
			for (z in 1:length(names(param.na)))
			{
				if(ano[k,3]==z)
					ano[k,3]<-names(param.na)[z]
			}
		}
		
		ano<-paste(ano[,1],ano[,2],ano[,3],sep="")
		ano<-as.vector(rbind(ano,rep("+",length(ano))))[1:c((length(ano)*2)-1)]
		ano<-paste(ano[1],ano[2],ano[3],ano[4],ano[5],ano[6],ano[7],ano[8] ,ano[9],ano[10],ano[11],ano[12],ano[13],ano[14],ano[15],ano[16],ano[17],ano[18] ,ano[19],ano[20],sep="")
		ano<-strsplit(ano, split="NA")[[1]][1]
		ano4<-paste("mat.analyse.na","~",ano,sep="")
		ano3<-formula(ano4)
		ano2<<-ano3
		tkgrid(tklabel(tt, text = c(paste("N",j,":",paste("Profils","~",ano,sep="")))))
	}
	
	reset.model<-function()
	{
		tkdestroy(tt)
		capscaleGUI()
	}
	compute.capscale<-function()
	{
		nb=as.numeric(tclvalue(nb))
		z1<-capscale(ano2,data=decostand(param.na,"standardize"))
		print(ano2)
		print(z1)
		print(anova(z1,step=nb))
	}
	stepwise.capscale<-function()
	{
		nb=as.numeric(tclvalue(nb))
		cca1 <- capscale(ano2,data=decostand(param.na,"standardize"))
		cca0 <- capscale(mat.analyse.na ~ 1, data=decostand(param.na,"standardize"))
		mod <- step(cca0, scope = formula(cca1), test = "perm")
		print(mod)
	}
	partial.capscale<-function()
	{
		nb=as.numeric(tclvalue(nb))
		z1<-capscale(ano2,data=decostand(param.na,"standardize"))
		
		print(anova(z1,step=nb,by="terms") )
	}
	plot.capscale<-function()
	{
		z1<-capscale(ano2,data=decostand(param.na,"standardize"))
		plotcca<-function(a,z)
		{
			a1<-max(a$CCA$wa[,1])
			a2<-max(a$CCA$wa[,2])
			a3<-max(a$CCA$biplot[,1])
			a4<-max(a$CCA$biplot[,2])
			maxx<-c(max(a1,a3),max(a2,a4))
			a1<-min(a$CCA$wa[,1])
			a2<-min(a$CCA$wa[,2])
			a3<-min(a$CCA$biplot[,1])
			a4<-min(a$CCA$biplot[,2])
			minxx<-c(min(a1,a3),min(a2,a4))
			plot(a$CCA$wa[,1],a$CCA$wa[,2],xlim=c(minxx[1],maxx[1]),ylim=c(minxx[2],maxx[2]),xlab="CCA1",ylab="CCA2")##donne les points des observations
			vvlimx<-c( min(a$CCA$biplot[,1]),max(a$CCA$biplot[,1]))
			vvlimy<-c( min(a$CCA$biplot[,2]),max(a$CCA$biplot[,2]))
			for (j in 1: dim(a$CCA$biplot)[1])
				arrows(0,0, c(a$CCA$biplot[j,1])*z,c(a$CCA$biplot[j,2])*z,col="blue",length=0.1)
			
			pointLabel(a$CCA$biplot[,1],a$CCA$biplot[,2],rownames(a$CCA$biplot),offset = 0, cex = 0.7)
			abline(v=0)
			abline(h=0)
		}
		plotcca(z1,1)
	}
	
	ttt11<-tkframe(tt)
	b1<- tkbutton(ttt11, text = "Add element to the design of CAP", command = create.model)
	b2<- tkbutton(ttt11, text = "Reset the design of CAP", command = reset.model)
	tkpack(b1,b2,side="left")
	tkgrid(ttt11)
	tkgrid(tklabel(tt, text = ""))
	
	tt1<-tkframe(tt)
	text1<-tklabel(tt1,text="Number of permutations?")
	nb <- tclVar("10")
	slider1 <- tkentry(tt1,width=8,textvariable=nb)
	tkpack(text1,slider1,side="left")
	tkgrid(tt1)
	tkgrid(tklabel(tt, text = ""))
	tkgrid(tkbutton(tt, text = "Stepwise selection of parameters to include in CAP", command =stepwise.capscale))
	tkgrid(tkbutton(tt, text = "Compute the CAP", command =compute.capscale))
	tkgrid(tkbutton(tt, text = "Plot CAP", command =plot.capscale))
	tkgrid(tkbutton(tt, text = "Compute the partialCAP", command =partial.capscale))
	tkgrid(tklabel(tt, text = ""))
	tkgrid(tkbutton(tt,text="Cancel",command=function() tkdestroy(tt)))
	tkgrid(tklabel(tt, text = ""))
	tkgrid(tklabel(tt, text = c("Your CAP formula is:")))
	tkfocus(tt)
	
	
}

