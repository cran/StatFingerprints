################################
#Close StatFingerprints program#
################################

"askquitGUI"<-function(){
closeAllConnections()
tkdestroy(MainMenu)
}