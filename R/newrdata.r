##############################################################
#    Function to defined the basic project of the program    #
##############################################################

"newrdata"<-function()
{
    filename<-"Don't forget to save your new project";filename<<-filename
    profil<-matrix(nr=5,nc=2);               profil[]<-1
    rox<-matrix(nr=5,nc=2);                  rox[]<-1
    mat<-list(profil=profil,rox=rox);        mat<<-mat
    mat6<-matrix(nr=5,nc=2);   mat6[]<-1;    mat6<<-mat6
    rxref<-0;                                rxref<<-rxref
    alig<-vector(length=5) ;   alig[]<-1;    alig<<-alig
    fact<-matrix(nr=5,nc=2);   fact[]<-1;    fact<<-fact
    param<-matrix(nr=5,nc=2);  param[]<-1;   param<<-param
    div<-vector(length=5) ;    div[]<-1;     div<<-div
    mat1<-matrix(nr=5,nc=2);   mat1[]<-1;    mat1<<-mat1
    mat2<-matrix(nr=5,nc=2);   mat2[]<-1;    mat2<<-mat2
    mat3<-matrix(nr=5,nc=2);   mat3[]<-1 ;   mat3<<-mat3
    mat4<-matrix(nr=5,nc=2);   mat4[]<-1;    mat4<<-mat4
    mat5<-matrix(nr=5,nc=2);   mat5[]<-1;    mat5<<-mat5
    mat7<-matrix(nr=5,nc=2);   mat7[]<-1;    mat7<<-mat7
    mat8<-matrix(nr=5,nc=2);   mat8[]<-1;    mat8<<-mat8
    mat9<-matrix(nr=5,nc=2);   mat9[]<-1;    mat9<<-mat9
}
