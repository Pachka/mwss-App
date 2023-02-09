timeinputinday <- function(timeinput){
 output <- (as.numeric(strftime(
   timeinput, "%M"
  )) / 60 + as.numeric(strftime(
   timeinput, "%H"
  ))) / 24

 output <- ifelse(output == 0, 1, output)

 return(output)

}
