#' Hide some cells and draw outline in flextable
#' @param ft A flextable
#' @param i rows selected
#' @param j columns selected
#' @param color color of border
#' @param width width of border
#' @importFrom flextable hline vline color
#' @importFrom officer fp_border
#' @return a flextable
#' @export
#' @examples
#' library(survival)
#' data(anderson,package="autoReg")
#' fit=survfit(Surv(time,status)~rx,data=anderson)
#' ft=howto(fit)
#' hide(ft,i=5:7,j=6:7)
hide=function(ft,i,j,color="red",width=2){

  myborder=fp_border(color=color,width=width)
  ft %>% color(i=i,j=j,color="white") %>%
    hline(i=min(i)-1,j=j,border=myborder) %>%
    hline(i=max(i),j=j,border=myborder) %>%
    vline(i=i,j=min(j)-1,border=myborder,part="body") %>%
    vline(i=i,j=max(j),border=myborder,part="body")
}
