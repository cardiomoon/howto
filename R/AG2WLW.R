#' Change (start,stop) or Anderson-Gill style data into WLW style
#'
#'@param data A data.frame
#'@param id Character name of id variable
#'@param status Character Name of status variable
#'@param start Character Name of start variable
#'@param stop Character Name of stop variable
#'@param interval Character Name of interval variable
#'@return A data.frame
#'@export
#'@examples
#'data(cancer,package="survival")
#'result=AG2WLW(bladder2,status="event",interval="enum")
#'identical(result,bladder)
AG2WLW=function(data,id="id",status="status",start="start",stop="stop",interval="interval"){
  maxinterval=max(table(data$id))
  ids=sort(unique(data[[id]]))
  no=length(ids)
  for(i in 1:no){
    temp=data[data[[id]]==ids[i],]
    while(nrow(temp)<maxinterval){
      temp=temp %>% arrange(temp[[interval]])
      temp=rbind(temp,temp[nrow(temp),])
      temp[[status]][nrow(temp)]=0
    }
    temp[[interval]]=1:maxinterval
    if(i==1) {
      result=temp
    } else{
      result=rbind(result,temp)
    }
  }
  result[[start]]=NULL
  row.names(result)=1:nrow(result)
  result
}
