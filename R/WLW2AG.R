#'  Change WLW style data into (start,stop) or Anderson-Gill style data
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
#'result=WLW2AG(bladder,status="event",interval="enum")
#'identical(result,bladder2)
WLW2AG=function(data,id="id",status="status",start="start",stop="stop",interval="interval"){

  maxinterval=max(table(data$id))
  ids=sort(unique(data[[id]]))
  no=length(ids)
  data[[start]]=as.integer(0)

  for(i in 1:no){

    temp=data[data[[id]]==ids[i],]
    j=1
    while(j <= (nrow(temp)-1)){

      temp[[stop]][j]==temp[[stop]][j+1]
      if(temp[[stop]][j]==temp[[stop]][j+1]){
        temp=temp[1:j,]

        break
      } else{
        temp[[start]][j+1]=temp[[stop]][j]
      }
      j=j+1

    }

    if(i==1) {
      result=temp
    } else{
      result=rbind(result,temp)
    }
  }
  xnames=c(start,stop,status,interval)
  colnames(result)
  mynames=c(setdiff(colnames(result),xnames),xnames)
  result=result %>% dplyr::select(all_of(mynames))
  row.names(result)=1:nrow(result)
  result$enum=as.numeric(result$enum)
  result

}
