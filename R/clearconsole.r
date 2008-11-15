############################################################
#    Function to clear the two consoles in the main GUI    #
############################################################

"clearconsole"<-function()
{
closeAllConnections()
R.base.dir <- system.file()
Met.Console<- paste(R.base.dir,"/../../library/StatFingerprints/StatFingerprints_Console_O",sep="")
Met.Console2<-paste(R.base.dir,"/../../library/StatFingerprints/StatFingerprints_Console_W",sep="")
z<-file(Met.Console,"w+b")
z2<-file(Met.Console2,"w+b")
sink(file = z, append = TRUE, type = c("output", "message"), split = FALSE)
sink(file = z2, append = TRUE, type =c("message"),  split = FALSE)
}