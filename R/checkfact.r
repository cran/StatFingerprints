##################################################################
#    Function to check if qualitative variables were imported    #
##################################################################

checkfact<-function()
{
if (is.factor(fact[1,1])==FALSE) tkmessageBox(message="Error, no qualitative variable imported")
if (is.factor(fact[1,1])==FALSE) stop("Error, no qualitative variable imported")
}
