#' Explain how to calculate survival rate
#' @param fit An object of class survfit
#' @return a flextable or NULL
#' @importFrom flextable flextable as_grouped_data autofit
#' @importFrom ftExtra colformat_md
#' @importFrom purrr map_dfc
#' @importFrom dplyr all_of lag
#' @importFrom rlang .data
#' @export
#' @examples
#' library(survival)
#' data(anderson,package="autoReg")
#' fit=survfit(Surv(time,status)~rx,data=anderson)
#' howto(fit)
#' fit1=survfit(Surv(time,status)~1,data=anderson)
#' howto(fit1)
#' fit2=survfit(Surv(time,status)~1,data=anderson[anderson$rx==1,])
#' howto(fit2)
howto=function(fit){

  if(!("survfit" %in% class(fit))) {
    cat("\nhowto function support class 'survfit' only!\n")
    return(invisible(NULL))
  }

  res=summary(fit)

  strata=FALSE
  if(!is.null(res$strata)){
    temp=table(res$strata)
    tn=names(temp)
    no=as.integer(temp)
    names(no)=tn
    group=c()
    for(i in seq_along(no)){
      group=c(group,rep(names(no)[i],no[i]+1))
    }
    count=length(no)
    strata=TRUE
  } else{
    no=res$n
    count=1
  }
  vars=c("time","n.risk","n.event","n.censor","surv")
  suppressMessages(temp<-map_dfc(vars,~res[[.]]))
  names(temp)=vars

  df=list()
  df$n.risk=res$n
  df$time<-df$n.event<-df$n.censor<-rep(0,count)
  df$surv<-rep(1,count)
  df=as.data.frame(df) %>% dplyr::select(all_of(vars))

  if(strata){

    start=1
    for(i in seq_along(no)){
      if(i==1) {
        result=df[i,]
      } else{
        result=rbind(result,df[i,])
      }
      result=rbind(result,temp[start:(start+no[i]-1),])
      start=start+no[i]
    }
  } else{
    result=rbind(df,temp)
  }
  result$surv=round(result$surv,4)
  result$lag=lag(result$surv)
  result$lag[result$time==0]=NA

  explain=c()
  for(i in 1:nrow(result)){
    if(is.na(result$lag[i])) {
      explain=c(explain,"")
    } else{
      explain=c(explain,paste0("$\\times(",result$n.risk[i]-result$n.event[i],"\\div",
                               result$n.risk[i],")$"))
    }
  }

  result$explain=paste0(sprintf("%.04f",result$lag),explain)
  result$explain[result$explain=="NA"]=""
  result=result %>% dplyr::select(.data$time,.data$n.risk,.data$n.event,.data$n.censor,.data$explain,.data$surv )

  colnames(result)[5]=c(" ")
  if(strata){
    result$strata=group
    ft= flextable(as_grouped_data(result,groups="strata"))
  } else{
    ft=flextable(result)
  }

  ft %>% colformat_md() %>% autofit()
}
