################################
#    GUI for saving project    #
################################

"saveGUI"<-function()
{
  if(exists("filename")==FALSE) saveasGUI()
  ss<-function()
  {
    tt1 <- tktoplevel()
    tkwm.title(tt1,"Loading")
    tkgrid(tklabel(tt1,font="arial 12",text="Please wait...\n This operation may take several minutes                           "))
    tkfocus(tt1)
    tkconfigure(tt1)
    
    save(mat,filename,fact,alig,param,div,rxref,mat1,mat2,mat3,mat4,mat5,mat6,mat7,mat8,mat9,file=filename)
    
    tkdestroy(tt1)
    tkfocus(MainMenu) 
  }
  if(exists("filename")==TRUE) ss()
}            
