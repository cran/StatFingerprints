checkparam <-
function()
{
	if(sum(param[1,],na.rm=TRUE)==length(param[1,]) & sum(div[1,],na.rm=TRUE)==length(div[1,])) tkmessageBox(message="Error, no quantitative variable imported",  icon="warning", type="ok")
	if(sum(param[1,],na.rm=TRUE)==length(param[1,]) & sum(div[1,],na.rm=TRUE)==length(div[1,])) stop("Error, no quantitative variable imported")
}

