#title: Lab 9A
#author: Gabrielle Giacoppe, GROUP A5
#date:11/11/2021

#Create functions that compute metrics to compare models.

PRESS <- function(model) {
  #' calculate the predictive residuals: residuals divided by (1-influence)
  x <- model.matrix(model)
  pr <- resid(model)/(1-hat(x))   
  #' calculate the PRESS
  PRESS <-sum(pr^2)    
  return(PRESS)
  
}

Criteria<-function(model,FullMSE,label=F){
  s.model<-summary(model)
  #1 – [(1-R2)*(n-1)/(n-k-1)]
  #[(1-(PRESS(model)/sum(resid(model)^2)))*(32-1)/(32-10-1)]
  
  
  SST = sum(anova(model)$'Sum Sq')
  R2= summary(model)$'r.squared'
  R2adj<- summary(model)$'adj.r.squared'            #adjusted r-sq
  SSE<- SST*(1-R2)     # SSE for the model
  dfsse<-tail(anova(model)$Df,1)                        # df of SSE
  samplesize<- length(model$residuals)             # n
  nparameters<- as.integer(samplesize - dfsse) # p+1
  #mse = sse*(32-10)
  Cp<-(SSE/(FullMSE))-samplesize + 2*(nparameters)
  AIC<- samplesize*log(SSE)-samplesize*log(samplesize) +2*(nparameters) #n ln(SSEp) − n ln n + 2(p + 1)
  press<-PRESS(model)
  if(label==T)
    c("p+1"=nparameters,R2adj=round(R2adj,4),Cp=round(Cp,2),AIC=round(AIC,2),PRESS=press)
  else
    c(nparameters,round(R2adj,4),round(Cp,2),round(AIC,2),press)
  
  #returns: size, adjusted R2, Cp, AIC and PRESS
  #return(list(nparameters,R2adj,Cp,AIC,PRESS(model)))
  #return(R2adj)
  #return(Cp)
  #return(AIC)
  #return(PRESS)
}

matrix.selection<-function(best.subset.model,Xnames,Yname,FullMSE,mydata){
  m<-dim(summary(best.subset.model)[[1]])
  df.out<-data.frame()
  for(i in 1:m[1])
  {
    foo<-summary(best.subset.model)[[1]][i,-1]
    form <- Xnames[foo]
    #form <- best.subset.model$xnames[foo]  #right thing to do but don't know how to handle dummy vars
    form <- paste(form, collapse = " + ")
    form <- paste(Yname, form,sep='~')
    model <- lm(as.formula(form),data=mydata)
    df.out<-rbind(df.out,Criteria(model,FullMSE))
  }
  names(df.out)<-c('p+1','R2adj','Cp','AIC','PRESS')
  dfsummary<- ifelse(summary(best.subset.model)[[1]],1,0)
  nnames<-length(dimnames(dfsummary)[[2]])
  U<-cbind(rep('x',(nnames-1)),seq(1:(nnames-1)))
  dimnames(dfsummary)[[2]]<-c('',apply(U,1,function(x) paste(x,collapse="")) )
  #'x1','x2','x3','x4','x5','x6','x7','x8','x9','x10','x11','x12','x13')
  row.names(dfsummary)<-NULL
  df.out<-cbind(dfsummary[,-1],df.out)
  df.out
}

model<-modp_log
FullMSE<-summary(modp_log)$sig^2  
Criteria(model,FullMSE,label=T)


