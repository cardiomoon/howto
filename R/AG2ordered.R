#'Change (start,stop) or Anderson-Gill style data into Ordered failure time and risk Set
#'
#'@param data A data.frame
#'@param id Character name of id variable
#'@param status Character Name of status variable
#'@param start Character Name of start variable
#'@param stop Character Name of stop variable
#'@importFrom dplyr pull arrange distinct count
#'@return A data.frame
#'@export
#'@examples
#'data(cancer,package="survival")
#'AG2ordered(bladder2[1:51,],status="event")
AG2ordered=function(data,id="id",status="status",start="start",stop="stop"){
  ftime=data %>% filter(.data[[status]]==1) %>% select(.data[[stop]]) %>%
    distinct() %>% arrange(.data[[stop]]) %>% pull()
  ftime
  n<-m<-q<-subid<-c()
  for(i in seq_along(ftime)){
      temp=data %>% filter(.data[[stop]]>=ftime[i]) %>% distinct(.data[[id]]) %>%
        count() %>% pull()
      n=c(n,temp)
      mid=data %>% filter(.data[[status]]==1 & .data[[stop]]==ftime[i]) %>% pull(.data[[id]])
      mid
      if(i<length(ftime)){
      qid=data %>%
        filter(.data[[status]]==0 & .data[[stop]]>=ftime[i] & .data[[stop]]<ftime[i+1] ) %>%
        pull(.data[[id]])
      } else{
        qid=data %>% filter(.data[[status]]==0 & .data[[stop]]>=ftime[i]) %>% pull(.data[[id]])
      }
      qid
      m=c(m,length(mid))
      q=c(q,length(qid))
      subid=c(subid,paste0(sort(unique(c(mid,qid))),collapse=","))
  }
  data.frame(t=ftime,n=n,m=m,q=q,id=subid)
}
