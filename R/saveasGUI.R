######################################
#    GUI for saving as... project    #
######################################
 
"saveasGUI"<-function()
{
  fileName<-tclvalue(tkgetSaveFile())
  filename<-paste(fileName,".Rdata",sep="")
  if (filename == "")
    return()
  
  tt1 <- tktoplevel()
  tkwm.title(tt1,"Loading")
  tkgrid(tklabel(tt1,font="arial 12",text="Please wait...\n This operation may take several minutes                           "))
  tkfocus(tt1)
  tkconfigure(tt1)
  
  save(list=ls(envir=.GlobalEnv), file=filename)
  save(mat,fact,param,div,alig,filename,rxref,mat1,mat2,mat3,mat4,mat5,mat6,mat7,mat8,mat9,file=filename)
  
  filename<<-filename

  tkdestroy(tt1)

  tkfocus(MainMenu)
  
}
