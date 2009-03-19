#############################################################
#    Function to change name of the fingreprint profiles    #
#############################################################

"change.nameGUI"<-function()
{
  checkprofile()
  
  vec<-cbind(rownames(mat$profil),vector(length=length(rownames(mat$profil))))
  vec[,2]=""
  vec<-edit(vec)
  vec=vec[,1]  
  rownames(mat$profil)<-vec
  rownames(mat$rox)<-vec; mat<<-mat
  rownames(mat1)<-vec;    mat1<<-mat1
  rownames(mat2)<-vec;    mat2<<-mat2
  rownames(mat3)<-vec;    mat3<<-mat3
  rownames(mat4)<-vec;    mat4<<-mat4 
  rownames(mat5)<-vec;    mat5<<-mat5 
  rownames(mat6)<-vec;    mat6<<-mat6
  rownames(mat7)<-vec;    mat7<<-mat7 
  rownames(mat8)<-vec;    mat8<<-mat8
  rownames(mat9)<-vec;    mat9<<-mat9
  rownames(fact)<-vec;    fact<<-fact
  rownames(param)<-vec;   param<<-param 
  
  for (i in 1:length(alig))
  {
    if (substr(alig[i],1,3)=="Ali") alig<-paste("Align_",vec,sep="")
    if (substr(alig[i],1,3)!="Ali") alig<-vec 
    alig<<-alig
  }
  
  names(div)<-vec
  div<<-div
  print("Names have been successfully changed")
}
