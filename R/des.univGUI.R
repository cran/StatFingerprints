###########################################
#    "Descriptive statistics" function    #
###########################################

"des.univGUI"<-function()
{
  if(sum(param)==length(param) & sum(div)==length(div)) tkmessageBox(message="Error, no quantitative variables and diversity index")
  if(sum(param)==length(param) & sum(div)==length(div)) stop("Error, no quantitative variables and diversity index")
  
  tt <- tktoplevel()
  tkwm.title(tt,"Mean, Standard deviation, Distribution")
  tkgrid(tklabel(tt, text = "                                                                                                                                              "))
  
  t1<-tkframe(tt)
  text1<-tklabel(t1, text = "Quantitative variable")
  repe <- c("Diversity index",names(param))
  repee <- tkwidget(t1, "ComboBox", editable = FALSE, values = repe)
  tkpack(text1,repee,side="left")
  tkgrid(t1)
  t2<-tkframe(tt)
  text2<-tklabel(t2, text = "Qualitative variable")
  repe1 <- c("none",names(fact))
  repee1 <- tkwidget(t2, "ComboBox", editable = FALSE, values = repe1)
    tkpack(text2,repee1,side="left")
  tkgrid(t2)
    
  mm<-function()
  {
    para <- unlist(as.numeric(tcl(repee, "getvalue")))
    fa <- unlist(as.numeric(tcl(repee1, "getvalue")))
    pa=0

    if (para==0) pa<-div
    if (para!=0) pa<-param[,para]
    if (fa==0) print(paste("Mean", mean(pa)))
    if (fa==0) print(paste("Standard deviation", sd(pa)))
    if (fa==0 & para!=0) boxplot(pa,main="Boxplot",ylab=names(param)[para])
    if (fa==0 & para==0) boxplot(pa,main="Boxplot",ylab="Diversity index")
    if (fa!=0) s<-split(pa,fact[,fa])
    if (fa!=0) ss<-matrix(nc=length(s),nr=3)
    if (fa!=0) colnames(ss)<-names(s)
    if (fa!=0) rownames(ss)<-c("Number","Mean","Standard Deviation")
    if (fa!=0)  for (i in 1:length(s)) {ss[1,i]<-length(s[[i]])}
    if (fa!=0)  for (i in 1:length(s)) {ss[2,i]<-mean(s[[i]])}
    if (fa!=0) for (i in 1:length(s)) {ss[3,i]<-sd(s[[i]])}
    if (fa!=0) print(ss)
    if (fa!=0 & para==0) boxplot(pa~factor(fact[,fa]),main="Boxplot",ylab="Diversity index")
    if (fa!=0 & para!=0) boxplot(pa~factor(fact[,fa]),main="Boxplot",ylab=names(param)[para])
  }
 
  nor<-function(){para <- unlist(as.numeric(tcl(repee, "getvalue")))
  pa=0
  if (para==0) pa<-div
  if (para!=0) pa<-param[,para]
  print(shapiro.test(pa))
  qqnorm(pa)}

  hom<-function()
  {
    para <- unlist(as.numeric(tcl(repee, "getvalue")))
    fa <- unlist(as.numeric(tcl(repee1, "getvalue")))
    pa=0
    if (para==0) pa<-div
    if (para!=0) pa<-param[,para]
    if (fa==0) tkmessageBox(message="No quantitative variable. Can not compute Bartlett test")
    if (fa==0) stop("No quantitative variable. Can not compute Bartlett test")
    if (fa!=0) print(bartlett.test(pa~factor(fact[,fa])))
  } 

  
  tkgrid(tklabel(tt, text = ""))
  t3<-tkframe(tt)
  b1<-tkbutton(t3,text="Calculate mean and standard deviation",command=mm)

  close<-function()
  {
    tkdestroy(tt)
  }

  b2<-tkbutton(t3,text="Cancel",command=close)
  tkpack(b1,b2,side="left")
  tkgrid(t3)
  tkgrid(tklabel(tt,text="   "))
  t4<-tkframe(tt)    
  b1<-tkbutton(t4,text="Normality Shapiro-Wilk test",command=nor)
  b2<-tkbutton(t4,text="Variances homogeneity",command=hom)
  tkpack(b1,b2,side="left")
  tkgrid(t4)
  tkgrid(tklabel(tt,text="   "))     
  tkfocus(tt)
}