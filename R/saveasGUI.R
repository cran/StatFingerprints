######################################
#    GUI for saving as... project    #
######################################
 
"saveasGUI"<-function()
{
  fileName<-tclvalue(tkgetSaveFile())
  filename<-paste(fileName,".Rdata",sep="")
  if (filename == "")
    return()
  print("Please wait while saving")
  save(list=ls(envir=.GlobalEnv), file=filename)
  save(mat,fact,param,div,alig,filename,rxref,mat1,mat2,mat3,mat4,mat5,mat6,mat7,mat8,mat9,file=filename)
  filename<<-filename
  print("Your project has been successfully saved")
  tkfocus(MainMenu)
}