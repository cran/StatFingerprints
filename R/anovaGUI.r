#######################################################
#    Function for the "Multifactor ANOVA" analysis    #
#######################################################

"anovaGUI"<-function()
{
  if (fact[1,1]==1) tkmessageBox(message="Error, no qualitative variables to compute ANOVA")
  if (fact[1,1]==1) stop("Error, no qualitative variables to compute ANOVA")
  tt<- tktoplevel()
  tkwm.title(tt, "Compute ANOVA")
  tkgrid(tklabel(tt, text = ""))
  t1<-tkframe(tt)
  text1<-tklabel(t1, text = "Quantitative variable to explain")
  a3 <- tkwidget(t1, "ComboBox", editable = FALSE, values = c("Diversity index",names(param)),height=length(c("Diversity index",names(param))))
  tkpack(text1,a3,side="left")
  tkgrid(t1)
  tkgrid(tklabel(tt, text = ""))
  tkgrid(tklabel(tt, text = c("Model of your ANOVA"))) 
  t2<-tkframe(tt)
  text2<-tklabel(t2, text = "First qualitative variable")
  repee <- tkwidget(t2, "ComboBox", editable = FALSE, values = names(fact),height=length(names(fact)))
  tkpack(text2,repee,side="left")
  tkgrid(t2)
  t3<-tkframe(tt)
  text3<-tklabel(t3, text = "Link")
  repee1 <- tkwidget(t3, "ComboBox", editable = FALSE, values = c("Single factor","Interaction","Single factor + interaction"),height=3)
  tkpack(text3,repee1,side="left")
  tkgrid(t3)
  t4<-tkframe(tt)
  text4<-tklabel(t4, text = "Second qualitative variable")
  repee2 <- tkwidget(t4, "ComboBox", editable = FALSE, values = names(fact),height=length(names(fact)))
  tkpack(text4,repee2,side="left")
  tkgrid(t4)
  anol=matrix(nr=15,nc=3);colnames(anol)=c("1 fact","link","2 fact")
  i=0;i<<-0
  j<<-i 
  mm <- function()
  {
    varex <- unlist(as.numeric(tcl(a3, "getvalue")) + 1)
    facte <- unlist(as.numeric(tcl(repee, "getvalue")) + 1)
    inter <- unlist(as.numeric(tcl(repee1, "getvalue")) + 1)
    facte2 <- unlist(as.numeric(tcl(repee2, "getvalue")) + 1)    
    i<<-j
    j<<-i+1
    anol[j,1]<-facte
    anol[j,2]<-inter
    anol[j,3]<-facte2
    anol<<-anol
    ano<-na.omit(anol)
  for (k in 1:dim(ano)[1])
  {
    aa<-c("+",":","*")
    for (z in 1:length(names(fact)))
    {
      if(ano[k,1]==z) ano[k,1]<-names(fact)[z]
    }
    for (z in 1:3)
    {
      if(ano[k,2]==z) ano[k,2]<-aa[z]
    }
    for (z in 1:length(names(fact)))
    {  
      if(ano[k,3]==z) ano[k,3]<-names(fact)[z]
    }
  }

  ano<-paste(ano[,1],ano[,2],ano[,3],sep="")
  ano<-as.vector(rbind(ano,rep("+",length(ano))))[1:c((length(ano)*2)-1)]
  ano<-paste(ano[1],ano[2],ano[3],ano[4],ano[5],ano[6],ano[7],ano[8] ,ano[9],ano[10],ano[11],ano[12],ano[13],ano[14],ano[15],ano[16],ano[17],ano[18] ,ano[19],ano[20],sep="")
  ano<-strsplit(ano, split="NA")[[1]][1]
  varex3<-c("diversity",names(param))
  varex3<-varex3[varex]
  ano4<-paste(varex3,"~",ano,sep="")
  varex3<-c("div",paste("param$",names(param),sep=""))
  varex3<-varex3[varex]
  ano<-paste(varex3,"~",ano,sep="")
  ano3<-formula(ano)
  ano2<<-ano3  
  tkgrid(tklabel(tt, text = c(paste("N",j,":",ano4))))
  }
      resett<-function(){tkdestroy(tt)
      anovaGUI()}
      res<-function(){
      z<-Anova(aov(ano2,data=fact),type="II")
      print(z)
  print(paste("MSE:   ",z[[1]][3]/z[[2]][3]))
  print(paste("RMSE:   ",sqrt(z[[1]][3]/z[[2]][3])))
}

res3<-function()
{
  
  print(TukeyHSD(aov(ano2,data=fact),type="II"))
}
    
  t5<-tkframe(tt)  
  b1 <- tkbutton(t5, text = "Add element to the model of ANOVA", command = mm)
  b2 <- tkbutton(t5, text = "Reset the model of ANOVA", command = resett)
  tkpack(b1,b2,side="left")
  tkgrid(t5)
  
  tkgrid(tklabel(tt, text = ""))
  t6<-tkframe(tt)
  b1<- tkbutton(t6, text = "Compute the ANOVA", command =res)
  b2<- tkbutton(t6, text = "Compute the TUKEY HSD post hoc test", command = res3)
  cc<-function(){tkdestroy(tt)}
  b3<-tkbutton(t6,text="Cancel",command=cc)
  tkpack(b1,b2,b3,side="left")
  tkgrid(t6)
  tkgrid(tklabel(tt, text = ""))
       tkgrid(tklabel(tt, text = c("Your ANOVA model is:"))) 
}



    
     
    
    
     
    