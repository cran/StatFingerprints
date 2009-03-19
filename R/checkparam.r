###################################################################
#    Function to check if quantitative variables were imported    #
###################################################################

checkparam<-function()
{
if(sum(param[1,],na.rm=TRUE)==length(param[1,])) tkmessageBox(message="Error, no quantitative variable imported")
if(sum(param[1,],na.rm=TRUE)==length(param[1,])) stop("Error, no quantitative variable imported")
}
