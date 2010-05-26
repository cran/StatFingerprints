checkfact <-
function()
{
	if (is.factor(fact[1,1])==FALSE) tkmessageBox(message="Error, no qualitative variable imported", icon="warning", type="ok")
	if (is.factor(fact[1,1])==FALSE) stop("Error, no qualitative variable imported")
}

