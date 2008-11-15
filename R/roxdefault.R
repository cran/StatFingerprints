################################################
#    GUI for the "define ROX peak" function    #  
################################################

"roxdefault"<-function()
{
####    ROX HD400 - 21 peaks
  
  roxref=c(4907,5214,6053,6293,7249,7681,8207,8588,8964,9298,9921,10432,10734,11124,11800,12119,12693,13095,13475,15298,17348)
  roxref<<-roxref
  defroxref<-function(roxref)
  {

####    Select range peaks

    plot(0,0,col="white",xlim=c(0,roxref[length(roxref)]),ylim=c(0,1),main="2 clicks before the first and after the last peaks used to align")
    abline(v=roxref,col="red",main="rox ref")
    loca=locator(2,type="p",pch=4) 
    roxref1<-roxref
    x1<-round(loca$x[1]);x2<-round(loca$x[2])
    for (i in 1:length(roxref))
    {
      if (x1>roxref[i]) 
        x11<-c(1:length(roxref))[i+1]
    }
    for (i in 1:length(roxref))
    {
      if (x2>roxref[i])
        x22<-c(1:length(roxref))[i]
    }
    
  roxref=roxref[x11:x22]
  plot(0,0,col="white",xlim=c(0,roxref1[length(roxref1)]),ylim=c(0,1),main=paste("You have selected ",length(roxref)," peaks"))
  abline(v=roxref1,col="red",main="rox ref")
  abline(v=roxref,col="green",lty=3)
  roxref=roxref-roxref[1]+25
  return(roxref)
  }

  r<-defroxref(roxref)
  rxref<<-r
  print("Reference standard successfully defined")
  print(rxref)
  tkfocus(MainMenu)
  
  
}
        