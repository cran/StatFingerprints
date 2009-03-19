######################################################
#    Plot all profiles of the project in one graph   #
######################################################

"plot3dimGUI"<-function()
{
  checkprofile()
  
  tt<-tktoplevel()
  tkwm.title(tt,"Plot fingerprint profiles in 3D")
  tkgrid(tklabel(tt,text="                                                                                                                    "))
  shook<-tclVar(1)
  distFrame5 <- tkframe(tt, relief="groove", borderwidth=2)
  tkgrid(tklabel(distFrame5, text="Color of the fingerprint profiles", foreground="black"))
  tkgrid(tkradiobutton(distFrame5, text="Terrain.colors (may be long)", value=1, variable=shook), sticky="w")
  tkgrid(tkradiobutton(distFrame5, text="Cm.colors (may be long)", value=2, variable=shook), sticky="w")
  tkgrid(tkradiobutton(distFrame5, text="Topo.colors (may be long)", value=3, variable=shook), sticky="w")
  tkgrid(tkradiobutton(distFrame5, text="Heat.colors (may be long)", value=4, variable=shook), sticky="w")
  tkgrid(tkradiobutton(distFrame5, text="Rainbow (may be long)", value=5, variable=shook), sticky="w")
  tkgrid(tkradiobutton(distFrame5, text="Blues (fast)", value=6, variable=shook), sticky="w")
  tkgrid(tkradiobutton(distFrame5, text="Greens (fast)", value=7, variable=shook), sticky="w")
  tkgrid(tkradiobutton(distFrame5, text="Greys (fast)", value=8, variable=shook), sticky="w")
  tkgrid(tkradiobutton(distFrame5, text="Oranges (fast)", value=9, variable=shook), sticky="w")
  tkgrid(tkradiobutton(distFrame5, text="Purples (fast)", value=10, variable=shook), sticky="w")
  tkgrid(tkradiobutton(distFrame5, text="Reds (fast)", value=11, variable=shook), sticky="w")
  tkgrid(tkradiobutton(distFrame5, text="Orange/Red (fast)", value=12, variable=shook), sticky="w")
  tkgrid(tkradiobutton(distFrame5, text="Blue/Green (fast)", value=13, variable=shook), sticky="w")
  tkgrid(distFrame5)
  colorlut<<-0
  z<-t(mat6)
  z<<-z
  myp<<-vector(length=length(as.numeric(z)))
  
  calc<-function()
  {
    colo <- tclvalue(shook)
    if (colo == 1) for (i in 1:99){myp[as.vector(z)>=quantile(as.vector(z),p=c( c(1:99)/100))[i]]<-terrain.colors(100)[i]}
    if (colo == 1) myp[as.vector(z)<=quantile(as.vector(z),p=c( c(1:99)/100))[1]]<-terrain.colors(100)[1]
    if (colo == 2) for (i in 1:99){myp[as.vector(z)>=quantile(as.vector(z),p=c( c(1:99)/100))[i]]<-cm.colors(100)[i]}
    if (colo == 2) myp[as.vector(z)<=quantile(as.vector(z),p=c( c(1:99)/100))[1]]<-cm.colors(100)[1]
    if (colo == 3) for (i in 1:99){colorlut[i]<-topo.colors(99)[100-i]} 
    if (colo == 3) for (i in 1:99){myp[as.vector(z)>=quantile(as.vector(z),p=c( c(1:99)/100))[i]]<-colorlut[i]}
    if (colo == 3) myp[as.vector(z)<=quantile(as.vector(z),p=c( c(1:99)/100))[1]]<-colorlut[1]
    if (colo == 4) for (i in 1:99){colorlut[i]<-heat.colors(99)[100-i]} 
    if (colo == 4) for (i in 1:99){myp[as.vector(z)>=quantile(as.vector(z),p=c( c(1:99)/100))[i]]<-colorlut[i]}
    if (colo == 4) myp[as.vector(z)<=quantile(as.vector(z),p=c( c(1:99)/100))[1]]<-colorlut[1]
    if (colo == 5) for (i in 1:99){myp[as.vector(z)>=quantile(as.vector(z),p=c( c(1:99)/100))[i]]<-rainbow(100)[i]}
    if (colo == 5) myp[as.vector(z)<=quantile(as.vector(z),p=c( c(1:99)/100))[1]]<-rainbow(100)[1]
    if (colo == 6) for (i in 1:9){myp[as.vector(z)>=quantile(as.vector(z),  p=c( c(1:9)/10))[i]]<-brewer.pal(9,"Blues" )[i]}
    if (colo == 6) myp[as.vector(z)<=quantile(as.vector(z),  p=c( c(1:9)/10))[1]]<-brewer.pal(9,"Blues" )[1]
    if (colo == 7) for (i in 1:9){myp[as.vector(z)>=quantile(as.vector(z),  p=c( c(1:9)/10))[i]]<-brewer.pal(9,"Greens" )[i]}
    if (colo == 7) myp[as.vector(z)<=quantile(as.vector(z),  p=c( c(1:9)/10))[1]]<-brewer.pal(9,"Greens" )[1]
    if (colo == 8) for (i in 1:9){myp[as.vector(z)>=quantile(as.vector(z),  p=c( c(1:9)/10))[i]]<-brewer.pal(9,"Greys" )[i]}
    if (colo == 8) myp[as.vector(z)<=quantile(as.vector(z),  p=c( c(1:9)/10))[1]]<-brewer.pal(9,"Greys" )[1]
    if (colo == 9) for (i in 1:9){myp[as.vector(z)>=quantile(as.vector(z),  p=c( c(1:9)/10))[i]]<-brewer.pal(9,"Oranges" )[i]}
    if (colo == 9) myp[as.vector(z)<=quantile(as.vector(z),  p=c( c(1:9)/10))[1]]<-brewer.pal(9,"Oranges" )[1]
    if (colo == 10) for (i in 1:9){myp[as.vector(z)>=quantile(as.vector(z),  p=c( c(1:9)/10))[i]]<-brewer.pal(9,"Purples" )[i]}
    if (colo == 10) myp[as.vector(z)<=quantile(as.vector(z),  p=c( c(1:9)/10))[1]]<-brewer.pal(9,"Purples" )[1]
    if (colo == 11) for (i in 1:9){myp[as.vector(z)>=quantile(as.vector(z),  p=c( c(1:9)/10))[i]]<-brewer.pal(9,"Reds" )[i]}
    if (colo == 11) myp[as.vector(z)<=quantile(as.vector(z),  p=c( c(1:9)/10))[1]]<-brewer.pal(9,"Reds" )[1]
    if (colo == 12) for (i in 1:9){myp[as.vector(z)>=quantile(as.vector(z),  p=c( c(1:9)/10))[i]]<-brewer.pal(9,"OrRd" )[i]}
    if (colo == 12) myp[as.vector(z)<=quantile(as.vector(z),  p=c( c(1:9)/10))[1]]<-brewer.pal(9,"OrRd" )[1]
    if (colo == 13) for (i in 1:9){myp[as.vector(z)>=quantile(as.vector(z),  p=c( c(1:9)/10))[i]]<-brewer.pal(9,"BuGn" )[i]}
    if (colo == 13) myp[as.vector(z)<=quantile(as.vector(z),  p=c( c(1:9)/10))[1]]<-brewer.pal(9,"BuGn" )[1]
    myp<<-myp
    plot3dim(mat=mat6,col=myp)
    tkfocus(MainMenu)
  }
  
  tkgrid(tklabel(tt,text=""))   
  t1<-tkframe(tt)
  b1<-tkbutton(t1,text="Plot in 3D",command=calc)
  
  ss<-function()
  {
    fileName<-tclvalue(tkgetSaveFile())
    filename<-paste(fileName,".png",sep="")
    if (filename == "")
      return()
    rgl.snapshot(filename=filename, fmt="png", top=TRUE )
    print("The plot has been successfully saved")
  }
  
  b2<-tkbutton(t1,text="Save picture",command=ss)
  
  close<-function()
  {
    tkdestroy(tt)
  }
  
  b3<-tkbutton(t1,text="Cancel",command=close)
  tkpack(b1,b2,b3,side="left")
  tkgrid(t1)
  tkgrid(tklabel(tt,text="")) 
  tkfocus(tt)
}
