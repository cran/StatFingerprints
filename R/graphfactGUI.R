graphfactGUI <-
function ()
{
	checkparam()
	checkfact()
	
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
	
	tt <- tktoplevel()
	tkwm.title(tt, "Two-way factor plot")
	tkgrid(tklabel(tt, text = "                                                                                      "))
	
	tt2<-tkframe(tt)
	text2<-tklabel(tt2, text = "Choose the quantitative variable:")
	valparam <- tkwidget(tt2, "ComboBox", editable = FALSE, values = c(names(param2)),height=c(length(names(param2))+1),width=20)
	tkpack(text2,valparam,side="left")
	tkgrid(tt2)
	
	tkgrid(tklabel(tt,text="  "))
	tt3<-tkframe(tt)
	text3<-tklabel(tt3, text = "Choose the 1st qualitative variable:")
	valfact1 <- tkwidget(tt3, "ComboBox", editable = FALSE, values = names(fact),height=length(names(fact)),width=20)
	tkpack(text3,valfact1,side="left")
	tkgrid(tt3)
	
	tkgrid(tklabel(tt,text="  "))
	tt4<-tkframe(tt)
	text4<-tklabel(tt4, text = "Choose the 2nd qualitative variable:")
	valfact2 <- tkwidget(tt4, "ComboBox", editable = FALSE, values = c("none",names(fact)),height=c(length(names(fact))+1),width=20)
	tkpack(text4,valfact2,side="left")
	tkgrid(tt4) 
	
	tkgrid(tklabel(tt,text="  "))
	tt5<-tkframe(tt)
	text5<-tklabel(tt5, text = "Choose the plot type:")
	plotype <- tkwidget(tt5, "ComboBox", editable = FALSE, values = c("Points","Points and lines","boxplot","histogram"),height=4,width=20)
	tkpack(text5,plotype,side="left")
	tkgrid(tt5)  
	
	graphfact <- function() 
	{ 
		x11() 
		valparam <- unlist(as.numeric(tcl(valparam, "getvalue")) + 1)
		valfact1 <- unlist(as.numeric(tcl(valfact1, "getvalue")) + 1)
		valfact2 <- unlist(as.numeric(tcl(valfact2, "getvalue")) + 1)
		plotype <- unlist(as.numeric(tcl(plotype, "getvalue")) + 1)
		pp<-param2[,c(valparam)]
		names(pp)<-names(param2)[c(valparam)]
		pp<<-pp
		casun<-function()
		{
			if(plotype==2 & length(which(is.na(pp)=="TRUE"))!=length(pp)) lineplot.CI(fact[,valfact1],type="p",pp,xlab=names(fact)[valfact1],ylab=names(pp))
			if(plotype==1 & length(which(is.na(pp)=="TRUE"))!=length(pp)) lineplot.CI(fact[,valfact1],pp,type="p",xlab=names(fact)[valfact1],ylab=names(pp))           
			if(plotype==3 & length(which(is.na(pp)=="TRUE"))!=length(pp))  boxplot(pp~fact[,valfact1],xlab=names(fact)[valfact1],ylab=names(pp))
			if(plotype==4 & length(which(is.na(pp)=="TRUE"))!=length(pp)) bargraph.CI(fact[,valfact1],pp,xlab=names(fact)[valfact1],ylab=names(pp))
			
			ajseg<-function()
			{
				aa<-split(pp,fact[,valfact1])
				bb<-vector(length=length(aa))
				for (i in 1:length(aa))
				{
					bb[i]<-mean(aa[[i]],na.rm=TRUE)
				}
				cc<-1:length(bb)
				bb[which(bb=="NaN")]<-NA
				dd<-which(bb!="NA")
				cc<-cc[dd];bb<-bb[dd]
				for(i in 1:c(length(dd)-1))
				{
					segments(cc[i],bb[i],cc[c(i+1)],bb[c(i+1)]) 
				}
			}
			if(plotype==2 & length(which(is.na(pp)=="TRUE"))!=length(pp)) ajseg()
		}
		
		casdeux<-function()
		{
			if(plotype==4) bargraph.CI(fact[,valfact1],pp,group=fact[,c(valfact2-1)],xlab=names(fact)[valfact1],ylab=names(pp),legend=TRUE)
			if(plotype==3) boxplot(pp~factor(paste(fact[,valfact1],fact[,c(valfact2-1)],sep="_")))
			casdeuxd<-function()
			{
				lev<-levels(fact[,c(valfact2-1)])
				##calcul ylim
				mm<-vector(length=2)
				ffff<-factor(paste(fact[,c(valfact2-1)],fact[,valfact1],sep="_"))
				ffff2<-split(pp,ffff)
				ffff3<-vector(length=length(ffff2))
				for (j in 1:length(ffff2))
				{
					ffff3[j]<-c(mean(ffff2[[j]],na.rm=TRUE)+c(sd(ffff2[[j]],na.rm=TRUE)/2))
				}
				mm[2]<-max(na.omit(ffff3))
				ffff3<-vector(length=length(ffff2))
				for (j in 1:length(ffff2))
				{
					ffff3[j]<-c(mean(ffff2[[j]],na.rm=TRUE)-c(sd(ffff2[[j]],na.rm=TRUE)/2))
				}
				mm[1]<-min(na.omit(ffff3))
				
				#calcul ylim
				for (i in 1:length(lev))
				{
					nn<-which(fact[,c(valfact2-1)]==lev[i])
					ppp<-pp[nn]
					factt<-fact[nn,valfact1]
					if(plotype==2 & length(which(is.na(ppp)=="TRUE"))!=length(ppp)) 
					{
						lineplot.CI(factt,ppp,pch=2,type="p",xlab=names(fact)[valfact1],ylab=names(pp),ylim=c(mm[1],mm[2]),col=i,err.width=0.03)
						legend("topright",lev,lty=rep(1,length(lev)),col=c(1:length(lev)))
					}
					if(plotype==1 & length(which(is.na(ppp)=="TRUE"))!=length(ppp)) 
					{
						lineplot.CI(factt,ppp,pch=2,type="p",xlab=names(fact)[valfact1],ylab=names(pp),ylim=c(mm[1],mm[2]),col=i,err.width=0.03)           
						legend("topright",lev,lty=rep(1,length(lev)),col=c(1:length(lev)))
					}
					ajsegg<-function()
					{
						##aj des segments
						aa<-split(ppp,factt)
						bb<-vector(length=length(aa))
						for (j in 1:length(aa))
						{
							bb[j]<-mean(aa[[j]],na.rm=TRUE)
						}
						cc<-1:length(bb)
						bb[which(bb=="NaN")]<-NA
						dd<-which(bb!="NA")
						cc<-cc[dd];bb<-bb[dd]
						for(j in 1:c(length(dd)-1))
						{
							segments(cc[j],bb[j],cc[c(j+1)],bb[c(j+1)],col=i) 
						} 
					}
					if(plotype==2 & length(which(is.na(ppp)=="TRUE"))!=length(ppp)) ajsegg()
					if(i<=c(length(lev)+1)) par(new=T) 	
				}
			}
			if(plotype!=4)casdeuxd()
			if(plotype!=3)casdeuxd()          
		}   
		if(valfact2==1) casun()         
		if(valfact2!=1) casdeux()          
	}
	
	tkgrid(tklabel(tt, text = " "))
	ff1<-tkframe(tt)
	b1<- tkbutton(ff1, text = "Plot", command = graphfact)
	b2<-tkbutton(ff1,text="Cancel",command=function() tkdestroy(tt))
	tkpack(b1,b2,side="left")
	tkgrid(ff1)
	tkgrid(tklabel(tt, text = " "))
	tkfocus(tt)
	
}

