################################
#    GUI for saving project    #
################################

"saveGUI"<-function()
{
  if(exists("filename")==FALSE) saveasGUI()
  ss<-function()
  {
    print("Please wait while saving")
    save(mat,filename,fact,alig,param,div,rxref,mat1,mat2,mat3,mat4,mat5,mat6,mat7,mat8,mat9,file=filename)
    print("Your project has been successfully saved")
    tkfocus(MainMenu) 
  }
  if(exists("filename")==TRUE) ss()
}            