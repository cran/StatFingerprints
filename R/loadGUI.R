#######################################
#    Load a project of the program    #
#######################################

"loadGUI"<-function()
{
#### Load project
  
  fil=if (interactive()) choose.files(filters = Filters["All",],caption="Select your project")
  print("Please wait...This operation may take several minutes")
  
   load(file=fil)
 filename<<-filename
  mat<<-mat
  mat1<<-mat1
  rxref<<-rxref
  fact<<-fact
  param<<-param
  div<<-div
  alig<<-alig
  mat2<<-mat2
  mat3<<-mat3
  mat4<<-mat4
  mat5<<-mat5
  mat6<<-mat6
  mat7<<-mat7
  mat8<<-mat8
  mat9<<-mat9  
  print("Your project has been successfully loaded")
  tkfocus(MainMenu)
}
