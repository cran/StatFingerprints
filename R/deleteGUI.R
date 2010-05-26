deleteGUI <-
function()
{
	checkprofile()
	
	tt <- tktoplevel()
	tkwm.title(tt,"Delete profiles")
	tkgrid(tklabel(tt,text=""))
	scr <- tkscrollbar(tt, repeatinterval=5,command=function(...)tkyview(tl,...))
	tl<-tklistbox(tt,height=20,width =50 ,selectmode="extended",yscrollcommand=function(...)tkset(scr,...),background="white")
	tkgrid(tklabel(tt,text="Which profiles do you want to delete?"))
	tkgrid(tklabel(tt,text=""))
	tkgrid(tl,scr)
	tkgrid.configure(scr,rowspan=10,sticky="nsw")
	
	prof <- alig
	for (i in 1:length(prof))
	{
		tkinsert(tl,"end",prof[i])
	}
	tkselection.set(tl,0)
	tkgrid(tklabel(tt,text=""))
	tkgrid(tklabel(tt,text=paste("You have ",length(prof)," fingerprint profiles",sep="")))
	
	delete.profile<-function()
	{
		sel<- as.numeric(tkcurselection(tl))+1
		mat.raw$profil<<-mat.raw$profil[-sel,] 
		mat.raw$rox<<-mat.raw$rox[-sel,] 
		div<<-div[-sel,] 
		fact<-fact[-sel,]
		
		relevels <- function(ff) 
		{
			temp<-ff
			for (i in 1:length(ff))
			{
				temp[,i]<-factor(as.character(ff[,i]))
			}
			return(temp)
		}   
		
		if (is.factor(fact[1,1])==TRUE)
			fact<-relevels(fact)
		fact<<-fact 
		param<<-param[-sel,]
		mat.align<<-mat.align[-sel,]
		alig<<-alig[-sel]
		mat.background<<-mat.background[-sel,]
		mat.baseline<<-mat.baseline[-sel,]
		mat.range<<-mat.range[-sel,]
		mat.analyse<<-mat.analyse[-sel,]
		mat.normalise<<-mat.normalise[-sel,]
		mat.rebuilt<<-mat.rebuilt[-sel,]
		mat.binary<<-mat.binary[-sel,]
		tkdestroy(tt)
		deleteGUI()
	}
	
	tkgrid(tklabel(tt,text=""))
	t1<-tkframe(tt)
	b1<-tkbutton(t1,text="Delete",command=delete.profile)
	b2<-tkbutton(t1,text="Cancel",command=function() tkdestroy(tt))
	tkpack(b1,b2,side="left")
	tkgrid(t1)
	tkgrid(tklabel(tt,text=""))
	tkfocus(tt)
}

