newrdata <-
function()
{
	filename			<-"Don't forget to save your new project";              				filename<<-filename
	profil					<-matrix(nr=5,nc=2);                										profil[]<-1
	rox					<-matrix(nr=5,nc=2);                   										rox[]<-1
	mat.raw			<-list(profil=profil,rox=rox);                           					mat.raw<<-mat.raw
	mat.analyse		<-matrix(nr=5,nc=2);           			mat.analyse[]<-1;     	mat.analyse<<-mat.analyse
	rxref					<-0;                                                       							rxref<<-rxref
	alig					<-vector(length=5) ;                  	alig[]<-1;            			alig<<-alig
	fact					<-matrix(nr=5,nc=2);                  	fact[]<-1;            			fact<<-fact
	param				<-matrix(nr=5,nc=2);                 	param[]<-1;           		param<<-param
	div					<-matrix(nr=5,nc=2) ;		            div[]<-1;             			div<<-div
	mat.align			<-matrix(nr=5,nc=2);             		mat.align[]<-1;       		mat.align<<-mat.align
	mat.background	<-matrix(nr=5,nc=2);        			mat.background[]<-1;  	mat.background<<-mat.background
	mat.baseline		<-matrix(nr=5,nc=2);          			mat.baseline[]<-1 ;   	mat.baseline<<-mat.baseline
	mat.range			<-matrix(nr=5,nc=2);             		mat.range[]<-1;      		mat.range<<-mat.range
	mat.normalise	<-matrix(nr=5,nc=2);        			mat.normalise[]<-1;   	mat.normalise<<-mat.normalise
	mat.rebuilt			<-matrix(nr=5,nc=2);           			mat.rebuilt[]<-1;     		mat.rebuilt<<-mat.rebuilt  
	mat.binary			<-matrix(nr=5,nc=2);            		mat.binary[]<-1;      		mat.binary<<-mat.binary
}

