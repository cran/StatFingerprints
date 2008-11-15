##################################################
#    Function to convert FSA files and import    #
##################################################

"convertGUI"<-function()
{

####    Require DataFileConverter (Applied Biosystems)

  tkmessageBox(message="This function needs Datafileconverter software (Applied Biosystems)")
  print("Importing")
  print("Please wait...\n This operation may take several minutes")
  locate.file.exe<-if (interactive()) choose.dir(getwd(), "Select the folder of DataFileConverter")
  locate.file.exe<-paste(locate.file.exe,"\\DataFileConverter.exe",sep="")
  locate.file.exe<-shQuote(locate.file.exe,type="cmd")

####    Import fsa files

  locate.dir.fsa<-if (interactive()) choose.dir(getwd(), "Select folder containing fsa files")
  print("Importing")
  print("Please wait...\n This operation may take several minutes")
  
  file.fsa<-"\\*.fsa"
  dir.file.fsa<-paste(locate.dir.fsa,file.fsa,sep="")
  dir.file.fsa<-shQuote(dir.file.fsa,type="cmd")

  dir.txt<-"\\txt"
  dir.file.txt<-paste(locate.dir.fsa,dir.txt,sep="")
  dir.create(dir.file.txt)
  dir.txt2<-shQuote(dir.file.txt,type="cmd")

  args<-paste("-i", dir.file.fsa ,"-t -o", dir.txt2, " -d -s")
  system(paste(locate.file.exe,args))

####    Create the list "raw.data" including the four channel of each sample and a vector with the name of the samples
  
  raw.data<-list()

####    Fill raw.data list

  list.file.fsa<-list.files(dir.file.txt)
  delimiter<-"\\"
  list.file.txt<-paste(dir.file.txt,delimiter,list.file.fsa,sep="")

  for (i in 1:length(list.file.txt))
  {
    a<-readLines(list.file.txt[i])
    DATA=which(nchar(a)>500)
    channel1<-unlist(strsplit(a[DATA[1]]," "))
    channel1<-channel1[-1]
    channel2<-unlist(strsplit(a[DATA[2]]," "))
    channel2<-channel2[-1]
    channel3<-unlist(strsplit(a[DATA[3]]," "))
    channel3<-channel3[-1]
    channel4<-unlist(strsplit(a[DATA[4]]," "))
    channel4<-channel4[-1]
    channels<-cbind(channel1,channel2,channel3,channel4)
    raw.data[[i]]<-channels
  }
  raw.data[[length(list.file.txt)+1]]<-basename(list.file.txt)
  mat10<-raw.data
  mat10<<-mat10
  zf2<-vector("list", length=c(length(mat10)-1))
  names(zf2)<-mat10[[length(mat10)]]

####    Building matrix data

  for (i in 1:c(length(mat10)-1))
  {
    z1=mat10[[i]]
    zf<-matrix(nr=dim(z1)[1],nc=dim(z1)[2])
    for (j in 1:dim(z1)[2])
    {
      z2=as.numeric(z1[,j])
      zf[,j]<-z2
    }
  zf2[[i]]<-zf
  }
  zf3<-zf2
  zf3<<-zf3
  tt <- tktoplevel()
  tclRequire("BWidget")
  tkwm.title(tt,"Import fingerprint profiles")

####    Displays profiles

  view.channels<-function()
  {
    plot(1:dim(zf3[[1]])[1],zf3[[1]][,1],type="l",col="blue",xlab="Scan of your 1 st fingerprint profile",ylab="Signal intensity")
    par(new=TRUE)
    plot(1:dim(zf3[[1]])[1],zf3[[1]][,2],type="l",col="black",xaxt="n",yaxt="n",ylab=NA,xlab=NA)
    par(new=TRUE)
    plot(1:dim(zf3[[1]])[1],zf3[[1]][,4],type="l",col="red",xaxt="n",yaxt="n",ylab=NA,xlab=NA)
    par(new=TRUE)
    plot(1:dim(zf3[[1]])[1],zf3[[1]][,3],type="l",col="green",xaxt="n",yaxt="n",ylab=NA,xlab=NA)
    legend("topright",col=c("blue","green","black","red"),c("1 st column","2 nd column","3 rd column","4 th column"),lty=c(1,1,1,1))
  }

  tkgrid(tklabel(tt,text=""))
  tkgrid(tkbutton(tt,text="How to choose community and internal standard location",command=view.channels))
   tkgrid(tklabel(tt,text=""))

  tt1<-tkframe(tt)
 text1<-tklabel(tt1,text="Community location in raw files?")
  com <- c("Column 1","Column 2","Column 3","Column 4")
  comu <- tkwidget(tt1,"ComboBox",editable=FALSE,values=com,height=4)
  tkpack(text1,comu,side="left")
  tkgrid(tt1)

  tt2<-tkframe(tt)
  text2<-tklabel(tt2,text="Internal standard location in raw files ?")
  int <- c("Column 1","Column 2","Column 3","Column 4")
  inte <- tkwidget(tt2,"ComboBox",editable=FALSE,values=int,height=4)
  tkpack(text2,inte,side="left")
  tkgrid(tt2)

  mm <- function()
  {
    
    com1 <-unlist(as.numeric(tcl(comu,"getvalue"))+1)
    ref1 <-unlist(as.numeric(tcl(inte,"getvalue"))+1 )
    mat<-list(profil=matrix(ncol=dim(zf3[[1]])[1],nrow=length(zf3)),rox=matrix(ncol=dim(zf3[[1]])[1],nrow=length(zf3)))
    rownames(mat$profil)<-names(zf3);rownames(mat$rox)<-names(zf3)
    for (i in 1:length(zf3))
    {
      mat$profil[i,]<-zf3[[i]][,com1]
    }
    for (i in 1:length(zf3))
    {
      mat$rox[i,]<-zf3[[i]][,ref1]
    }
    mat<<-mat 
    mat6<<-mat$profil
    rxref<-0
    rxref<<-rxref 
    alig<-vector(length=length(rownames(mat$profil)));alig<-rownames(mat$profil)                          ;alig<<-alig
    fact<-matrix(nr=dim(mat$profil)[1],nc=2)         ;fact[]<-1   ;rownames(fact)<-rownames(mat$profil)   ;fact<<-fact
    param<-matrix(nr=dim(mat$profil)[1],nc=2)        ;param[]<-1  ;rownames(param)<-rownames(mat$profil)  ;param<<-param
    div<-vector(length=dim(mat$profil)[1])           ;div[]<-1    ;names(div)<-rownames(mat$profil)       ;div<<-div 
    mat1<-matrix(nr=dim(mat$profil)[1],nc=2)         ;mat1[]<-1   ;rownames(mat1)<-rownames(mat$profil)   ;mat1<<-mat1
    mat2<-matrix(nr=dim(mat$profil)[1],nc=2)         ;mat2[]<-1   ;rownames(mat2)<-rownames(mat$profil)   ;mat2<<-mat2 
    mat3<-matrix(nr=dim(mat$profil)[1],nc=2)         ;mat3[]<-1   ;rownames(mat3)<-rownames(mat$profil)   ;mat3<<-mat3 
    mat4<-matrix(nr=dim(mat$profil)[1],nc=2)         ;mat4[]<-1   ;rownames(mat4)<-rownames(mat$profil)   ;mat4<<-mat4 
    mat5<-matrix(nr=dim(mat$profil)[1],nc=2)         ;mat5[]<-1   ;rownames(mat5)<-rownames(mat$profil)   ;mat5<<-mat5 
    mat7<-matrix(nr=dim(mat$profil)[1],nc=2)         ;mat7[]<-1   ;rownames(mat7)<-rownames(mat$profil)   ;mat7<<-mat7 
    mat8<-matrix(nr=dim(mat$profil)[1],nc=2)         ;mat8[]<-1   ;rownames(mat8)<-rownames(mat$profil)   ;mat8<<-mat8 
    mat9<-matrix(nr=dim(mat$profil)[1],nc=2)         ;mat9[]<-1   ;rownames(mat9)<-rownames(mat$profil)   ;mat9<<-mat9 
    print("Successfully loaded!")
    tkdestroy(tt)
    tkfocus(MainMenu)
   }
  tkgrid(tklabel(tt,text=""))
  tt3<-tkframe(tt)
  b1<-tkbutton(tt3,text="Import all",command=mm)
  close<-function()
  {
    tkdestroy(tt)
  }

  b2<-tkbutton(tt3,text="Cancel",command=close)
  tkpack(b1,b2,side="left")
  tkgrid(tt3)
  tkgrid(tklabel(tt,text=""))
  tkfocus(tt)
}