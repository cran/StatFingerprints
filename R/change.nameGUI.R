change.nameGUI <-
function()
{
	checkprofile()
	
	vec<-cbind(rownames(mat.raw$profil),vector(length=length(rownames(mat.raw$profil))))
	vec[,2]=""
	vec<-edit(vec)
	vec=vec[,1]  
	rownames(mat.raw$profil)			<-vec;
	rownames(mat.raw$rox)			<-vec;	mat.raw<<-mat.raw
	rownames(mat.align)					<-vec;  mat.align<<-mat.align
	rownames(mat.background)		<-vec;  mat.background<<-mat.background
	rownames(mat.baseline)			<-vec;  mat.baseline<<-mat.baseline
	rownames(mat.range)				<-vec;  mat.range<<-mat.range 
	rownames(mat.analyse)				<-vec;  mat.analyse<<-mat.analyse
	rownames(mat.normalise)			<-vec;  mat.normalise<<-mat.normalise 
	rownames(mat.rebuilt)				<-vec;  mat.rebuilt<<-mat.rebuilt
	rownames(mat.binary)				<-vec;  mat.binary<<-mat.binary
	rownames(fact)							<-vec;  fact<<-fact
	rownames(param)						<-vec;  param<<-param
	rownames(div)							<-vec;  div<<-div
	
	for (i in 1:length(alig))
	{
		if (substr(alig[i],1,3)=="Ali") alig<-paste("Align_",vec,sep="")
		if (substr(alig[i],1,3)!="Ali") alig<-vec 
		alig<<-alig
	}
}

