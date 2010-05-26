alignGUI <-
function()
{
	checkprofile()
	
	####  Warning: ecological data cannot be aligned
	if(length(which(mat.raw$rox==mat.raw$profil)==TRUE)==dim(mat.raw$profil)[1]*dim(mat.raw$profil)[2])
	{
		tkmessageBox(message="You have import an ecological table. Alignment cannot be performed", icon="info", type="ok")
		stop("You have import an ecological table. Alignment cannot be performed")
	}
	
	####  Warning: rox must be define first	
	if (rxref[1]==0)
	{
		tkmessageBox(message="First, you must define the reference, on which profiles will be aligned. See Data transformation/Rox", icon="info", type="ok")
		stop("First, you must define the reference, on which profiles will be aligned. See Data transformation/Rox")
	}
	
	####  create mat.align
	if (dim(mat.align)[2]==2)
		mat.align<-matrix(nc=rxref[length(rxref)]-rxref[1],nr=dim(mat.raw$rox)[1])
	mat.align<<-mat.align 
	
	alignGUI2()
}

