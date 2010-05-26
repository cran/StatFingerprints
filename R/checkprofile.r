checkprofile <-
function()
{
	if(sum(mat.raw$profil[1,],na.rm=TRUE)==length(mat.raw$profil[1,])) tkmessageBox(message="Error, no profile imported", icon="warning", type="ok")
	if(sum(mat.raw$profil[1,],na.rm=TRUE)==length(mat.raw$profil[1,])) stop("Error, no profile imported")
}

