#'Calculate cumulative incidence curves
#'
#'@param df A data frame
#'@param time character name of time variable
#'@param status character name of status variable
#'@param cause Numeric status for CIC calculation
#'@param add.CPC logical Whether or not add conditional probability curves(CPC)
#'@return A data.frame
#'@importFrom survival survfit Surv
#'@export
#'@examples
#'time=c(0.7,3,4.9,6,6,6.9,10,10.8,17.1,20.3,1.5,2.8,3.8,4.7,7,10,10,11.2,
#'3.2,7.6,10,11,15,24.4)
#'status=c(rep(1,10),rep(2,8),rep(0,6))
#'df=data.frame(time,status)
#'data2CIC(df,cause=1,add.CPC=FALSE)
#'data2CIC(df,cause=NULL,add.CPC=FALSE)
#'data2CIC(df)
data2CIC=function(df,time="time",status="status",cause=1,add.CPC=TRUE){
  #time="time";status="status";cause=1;add.CPC=TRUE
  #df=Byar[Byar$Rx==0,];time="dtime";status="status2";cause=1;add.CPC=TRUE
  t=sort(unique(df[[time]]))
  t=c(0,t)
  allstatus=as.numeric(df[[status]]>=1)
  fit=survfit(Surv(df[[time]],allstatus)~1)
  S=c(NA,1,fit$surv[-length(fit$surv)])
  if(is.null(cause)){
    cause=setdiff(unique(df[[status]]),0)
  }
  CIC=list()
  if(add.CPC){
    cause=cause[1]
    n<-m<-h<-I<-c()
    df1=df[df[[status]]==cause,]
    for(j in seq_along(t)){
      tempn=length(which(df[[time]]>=t[j]))
      tempm=length(which(df1[[time]]==t[j]))
      n=c(n,tempn)
      m=c(m,tempm)
      h=c(h,tempm/tempn)
    }
    I=h*S
    I[1]=0
    CIC[[1]]=cumsum(I)

    n<-m<-h<-I<-c()
    status2=setdiff(unique(df[[status]]),c(0,cause))

    df1=df[df[[status]] %in% status2,]
    for(j in seq_along(t)){
      tempn=length(which(df[[time]]>=t[j]))
      tempm=length(which(df1[[time]]==t[j]))
      n=c(n,tempn)
      m=c(m,tempm)
      h=c(h,tempm/tempn)
    }
    I=h*S
    I[1]=0
    CIC[[2]]=cumsum(I)


  } else{
    for(i in seq_along(cause)){
      n<-m<-h<-I<-c()
      df1=df[df[[status]]==cause[i],]
      for(j in seq_along(t)){
        tempn=length(which(df[[time]]>=t[j]))
        tempm=length(which(df1[[time]]==t[j]))
        n=c(n,tempn)
        m=c(m,tempm)
        h=c(h,tempm/tempn)
      }
      I=h*S
      I[1]=0
      CIC[[i]]=cumsum(I)
    }
  }
  if((!add.CPC) & (length(cause)==1)){
    result=data.frame(t,n,m,h,S,I,CIC[[1]])
    names(result)[7]=paste0("CIC",cause)
    for(i in 4:7) result[[i]]=round(result[[i]],3)
  } else{
    result=data.frame(t,n,S,CIC)
    names(result)=c("t","n","S",paste0("CIC",1:length(CIC)))
    if(add.CPC) result$CPC=result$CIC1/(1-result$CIC2)
    for(i in 3:ncol(result)) result[[i]]=round(result[[i]],3)
  }
  result
}
