###################################################
# Add fingerprints profiles to the current project#
###################################################

"addGUI"<-function()
{ 
  tt<-tktoplevel()
  tkwm.title(tt,"Add files")

  load1<-function()
  { 
    loadGUI()
    filename1<<-filename
    mat0<<-mat
    mat11<<-mat1
    rxref1<<-rxref
    fact1<<-fact
    param1<<-param
    div1<<-div
    alig1<<-alig
    mat22<<-mat2
    mat33<<-mat3
    mat44<<-mat4
    mat55<<-mat5
    mat66<<-mat6
    mat77<<-mat7
    mat88<<-mat8
    mat99<<-mat9
    loadGUI()
    filename11<<-filename
    mat00<<-mat
    mat111<<-mat1
    rxref11<<-rxref
    fact11<<-fact
    param11<<-param
    div11<<-div
    alig11<<-alig
    mat222<<-mat2
    mat333<<-mat3
    mat444<<-mat4
    mat555<<-mat5
    mat666<<-mat6
    mat777<<-mat7
    mat888<<-mat8
    mat999<<-mat9
    print("Your two projects have been successfully loaded")
  }
  
  mergee<-function()
  {
    a01<-0
    for (i in 1:length(rxref11)){if (rxref11[i]!=rxref1[i]) a01<-1}
    if (a01==1)
      tkmessageBox(message="Peaks of your both reference standard are not the same and thus fingerprint profiles between the two projects could not be adequatly compared",icon="info",type="ok")
    if (dim(fact1)[2]!= dim(fact11)[2])
      tkmessageBox(message="Merge can not be done. Problem is located in the factor file" ,icon="info",type="ok")
    if (dim(param1)[2]!= dim(param11)[2])
      tkmessageBox(message="Merge can not be done. Problem is located in the parameter file" ,icon="info",type="ok")
    if (dim(mat00$profil)[2]!=dim(mat0$profil)[2])
      tkmessageBox(message="Merge can not be done. Problem occured as fingerprint profiles in the two projects have not the same number of variable" ,icon="info",type="ok")
    if (dim(mat11)[2]!=dim(mat111)[2]) 
      tkmessageBox(message="Merge can not be done. Problem occured as fingerprint profiles in the two projects have not the same number of variable" ,icon="info",type="ok")
    mat$profil<<-rbind(mat0$profil,mat00$profil)
    mat$rox<<-rbind(mat0$rox,mat00$rox)
    mat1<<-rbind(mat11,mat111)
    mat2<<-rbind(mat22,mat222)
    rxref<<-rxref1
    fact<-rbind(fact1,fact11)
      relevels<-function(ff)
    {
      for (i in 1:dim(ff)[2])
      {
        f<-ff[,i]
        a=vector(length=length(levels(f)))
        for (j in 1:length(levels(f)))
        {
          a[j]<-length(which(f==levels(f)[j]))
        }
      
        a=which(a==0)
        j<-vector(length=length(f))
        j<-factor(j)
        if (length(a)!=0) levels(j)<-levels(f)[-a]
        if (length(a)==0) levels(j)<-levels(f)
        j[]<-f
        ff[,i]<-j
      }
      return(ff)
    }   
    fact<<-relevels(fact)
    alig<<-c(alig1,alig11)
    param<<-rbind(param1,param11)
    div<<- c(div1,div11)
    mat3<-matrix(nr=dim(mat1)[1],nc=2);mat3[]<-1;rownames(mat3)<-rownames(mat1);mat3<<-mat3
    mat4<-matrix(nr=dim(mat1)[1],nc=2);mat4[]<-1;rownames(mat4)<-rownames(mat1);mat4<<-mat4
    mat5<-matrix(nr=dim(mat1)[1],nc=2);mat5[]<-1;rownames(mat5)<-rownames(mat1);mat5<<-mat5
    mat6<<-mat$profil
    mat7<-matrix(nr=dim(mat1)[1],nc=2);mat7[]<-1;rownames(mat7)<-rownames(mat1);mat7<<-mat7
    mat8<-matrix(nr=dim(mat1)[1],nc=2);mat8[]<-1;rownames(mat8)<-rownames(mat1);mat8<<-mat8
    mat9<-matrix(nr=dim(mat1)[1],nc=2);mat9[]<-1;rownames(mat9)<-rownames(mat1);mat9<<-mat9
    print("Your two projects have been successfully merged")
  }

  save1<-function()
  {
    saveasGUI()
  }

  F1 <- tkframe(tt)
  load1<-tkbutton(F1,text="1 Step : load two projects",command=load1)
  mergee<-tkbutton(F1,text="2 Step : merge two projects",command=mergee)
  savee<-tkbutton(F1,text="3 Step : save the merged project",command=save1)
  tkpack(load1,mergee,savee,side="left")
  tkgrid(F1)

  F2 <-tkframe(tt)
  j<- tklabel(F2,font="arial 10",wraplength="4i",justify="left",text="Before using these functions, you have to create the two projects to merge.")
  k<- tklabel(F2,font="arial 7",wraplength="4i",justify="left",text="For example, you have to 1) save your current project (Files/save project as...), 2)create a new project (Files/New project/import communities profiles), process profiles until step 3 (define a common baseline for all profiles) and save this new project. Don't forget in the new project to imports variables (Files/new project/import variables) if you first project contained these information.  ")
  l<- tklabel(F2,font="arial 10",wraplength="4i",justify="left",text="After having merge the two projects and save the new project, and before statistical analyses, don't forget to process profiles from step 3 to 5 of  the profile processing menu")

  tkpack(l,k,j,side="bottom")
  tkpack(F2,F1, side="bottom")
  tkfocus(tt)
} 