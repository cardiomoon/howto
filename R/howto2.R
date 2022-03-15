#'Explain how to perform log-rank test
#'
#' @param fit An object of class survfit
#' @param digits Integer indicating the number of decimal places
#' @return A flextable or NULL
#' @importFrom flextable flextable as_grouped_data autofit align add_footer_row add_header_row
#'    colformat_double
#' @importFrom ftExtra colformat_md
#' @importFrom dplyr select filter `%>%` mutate
#' @importFrom stats pchisq
#' @importFrom tibble tibble
#' @importFrom rlang `:=` .data
#' @export
#' @examples
#' library(survival)
#' data(anderson,package="autoReg")
#' fit=survfit(Surv(time,status)~rx,data=anderson)
#' survdiff(Surv(time,status)~rx,data=anderson)
#' howto2(fit)
howto2=function(fit,digits=2){

  if(!("survfit" %in% class(fit))) {
    cat("\nhowto2 function support class 'survfit' only!\n")
    return(invisible(NULL))
  } else if(is.null(fit$strata)) {
    cat("\nhowto2 function support class 'survfit' with strata!\n")
    return(invisible(NULL))
  } else if(length(fit$strata)!=2){
    cat("\nhowto2 function support class 'survfit' with two strata !\n")
    return(invisible(NULL))
  }
  vars=c("time","n.risk","n.event","n.censor")

  df=map_dfc(vars,function(x){
    tibble::tibble(
      {{x}} := fit[[x]]
    )
  })

  no=fit$strata
  group=c()
  for(i in seq_along(no)){
    group=c(group,rep(names(no)[i],no[i]))
  }
  df$strata=group
  df

  time=sort(unique(df$time))
  time
  dflist=split(df,df$strata)
  dflist

  no=fit$n

  no
  suppressMessages(result<-map_dfc(1:length(dflist),function(z){

    x=dflist[[z]]
    y=no[z]
    strata=names(fit$strata)[z]
    result=data.frame(time=0,n.risk=y,n.event=0,n.censor=0,strata=strata)
    j=1
    x
    time
    result
    for(i in 1:nrow(x)){

      if(x$time[i]>time[j]){
        while(x$time[i]>time[j]){
          result=rbind(result,result[nrow(result),])
          result$time[nrow(result)]=time[j]
          result$n.risk[nrow(result)]=result$n.risk[nrow(result)]-
            result$n.event[nrow(result)]-
            result$n.censor[nrow(result)]
          result$n.event[nrow(result)]=0
          result$n.censor[nrow(result)]=0
          j=j+1
        }

      }
      result=rbind(result,x[i,])
      j=j+1
      result
      if(i==nrow(x)){
        while(j<=length(time)){
          result=rbind(result,result[nrow(result),])
          result$time[nrow(result)]=time[j]
          result$n.risk[nrow(result)]=result$n.risk[nrow(result)]-result$n.event[nrow(result)]-result$n.censor[nrow(result)]
          result$n.event[nrow(result)]=0
          result$n.censor[nrow(result)]=0
          j=j+1
        }
      }
    }
    result[1:3]
  }))

  names(result)=c("t","n1","f1","t2","n2","f2")

  df=result %>% dplyr::select(.data$t,.data$f1,.data$f2,.data$n1,.data$n2) %>%
    filter(.data$t!=0) %>%
    filter(.data$f1+.data$f2!=0)

  df %>% mutate(
    e1f=paste0("$(",.data$n1,"\\div",.data$n1+.data$n2,")\\times",.data$f1+.data$f2,"$"),
    e1=.data$n1/(.data$n1+.data$n2)*(.data$f1+.data$f2),
    e2f=paste0("$(",.data$n2,"\\div",.data$n1+.data$n2,")\\times",.data$f1+.data$f2,"$"),
    e2=.data$n2/(.data$n1+.data$n2)*(.data$f1+.data$f2),
    oe1=.data$f1-.data$e1,
    oe2=.data$f2-.data$e2,
    varOE=ifelse(.data$n1+.data$n2==1,0,(.data$n1*.data$n2*(.data$f1+.data$f2)*(.data$n1+.data$n2-.data$f1-.data$f2))/((.data$n1+.data$n2)^2*(.data$n1+.data$n2-1)))
  ) ->df

  df
  df1=data.frame(t="Total",f1=round(sum(df$f1)),f2=round(sum(df$f2)),n1="",n2="",e1f=round(sum(df$e1),digits),e1="",
                 e2f=round(sum(df$e2),digits),e2="",oe1=sum(df$oe1),oe2=sum(df$oe2),varOE=sum(df$varOE))
  df=rbind(df,df1)

  x2=(df1$oe1)^2/df1$varOE
  p=pchisq(x2,df=1,lower.tail=FALSE)
  names(df)[10:11]=c("f1-e1f","f2-e2f")
  ft<-df %>% select(-.data$e1,-.data$e2)%>%flextable() %>%
    add_header_row(values=c("","# Failures","# at risk","# Expected","Obs-Exp","Var(O-E)"),
                   colwidths=c(1,2,2,2,2,1)) %>%
    colformat_md() %>%
    colformat_double(j=8:10,digits=digits) %>%
    align(align="center",part="header") %>%
    align(j=1:5,align="center") %>%
    align(j=6:9,align="right") %>%
    hline(i=nrow(df)-1) %>%
    add_footer_row(values=paste0("Log-rank statistic=",format(round(x2,digits)),
                                " on 1 degrees of freedom, p= ",format.pval(p,digits=4)),colwidths = 10) %>%
    autofit()

  attr(ft,"O-E")=df1$oe1
  attr(ft,"Var(O-E)")=df1$varOE
  attr(ft,"x2")=x2
  attr(ft,"p")=p
  ft
}
