#######################################
#    Delete profile to the project    #
#######################################

"deleteGUI"<-function()
{
  tt <- tktoplevel()
  tkwm.title(tt,"Delete profiles")
  tkgrid(tklabel(tt,text=""))
  scr <- tkscrollbar(tt, repeatinterval=5,command=function(...)tkyview(tl,...))
  tl<-tklistbox(tt,height=10,width =50 ,selectmode="single",yscrollcommand=function(...)tkset(scr,...),background="white")
  tkgrid(tklabel(tt,text="Which profiles do you want to delete?"))
  tkgrid(tklabel(tt,text=""))
  tkgrid(tl,scr)
  tkgrid.configure(scr,rowspan=10,sticky="nsw")

  prof <- rownames(mat$profil)
  for (i in 1:length(prof))
  {
    tkinsert(tl,"end",prof[i])
  }
  tkselection.set(tl,0)
  tkgrid(tklabel(tt,text=""))
  tkgrid(tklabel(tt,text=paste("You have ",length(prof)," fingerprint profiles",sep="")))

  mm<-function()
  {
    sel<- as.numeric(tkcurselection(tl))+1
    mat$profil<<-mat$profil[-sel,] 
    mat$rox<<-mat$rox[-sel,] 
    div<<-div[-sel] 
    fact<-fact[-sel,]
    
    relevels<-function(ff)
    {
      for (i in 1:dim(ff)[2])
      {
        f<-ff[,i]
        a=vector(length=length(levels(f)))
        for (j in 1:length(levels(f)))
        {
          a[j]<-length(which(f==levels(f)[j]))
        }
      
        a=which(a==0)
        j<-vector(length=length(f))
        j<-factor(j)
        if (length(a)!=0) levels(j)<-levels(f)[-a]
        if (length(a)==0) levels(j)<-levels(f)
        j[]<-f
        ff[,i]<-j
      }
      return(ff)
    }   
    if (fact[1,1]!=1) fact<-relevels(fact)
    fact<<-fact 
    param<<-param[-sel,]
    mat1<<-mat1[-sel,]
    alig<<-alig[-sel]
    mat2<<-mat2[-sel,]
    mat3<<-mat3[-sel,]
    mat4<<-mat4[-sel,]
    mat5<<-mat5[-sel,]
    mat6<<-mat6[-sel,]
    mat7<<-mat7[-sel,]
    mat8<<-mat8[-sel,]
    mat9<<-mat9[-sel,]
    tkdestroy(tt)
    deleteGUI()
  }
  tkgrid(tklabel(tt,text=""))
  t1<-tkframe(tt)
  b1<-tkbutton(t1,text="Delete",command=mm)
  
  close<-function()
  {
    tkdestroy(tt)
  }
  
  b2<-tkbutton(t1,text="Cancel",command=close)
  tkpack(b1,b2,side="left")
  tkgrid(t1)
  tkgrid(tklabel(tt,text=""))
  tkfocus(tt)
}