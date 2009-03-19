#####################################################
#    Function to check if profiles were imported    #
#####################################################

checkprofile<-function()
{
if(sum(mat$profil[1,])==length(mat$profil[1,])) tkmessageBox(message="Error, no profile imported")
if(sum(mat$profil[1,])==length(mat$profil[1,])) stop("Error, no profile imported")
}
