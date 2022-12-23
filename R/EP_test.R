
#' Calculate Effective precipitation
#'
#' This function calculates the effective precipitation as in
#'  Byun HR, Wilhite DA (1999) Objective quantification of drought severity and duration.
#'  J Climatol 12:2747–2756 https://doi.org/10.1175/1520-0442(1999)012<2747:OQODSA>2.0.CO;2
#'
#' @param ndays A numeric vector with precipitation values. This will be the number of previous days to
#' account for the calculation of the effective precipitation of a m day.
#' @param data A numeric vector with precipitation values
#' @return A numeric vector
#' @export
EP_calc <- function(ndays = 365, data){
  if(!is.numeric(ndays)&length(ndays)>1) stop("ndays must be a unique numeric element")
  if(!is.vector(data)&!is.numeric(data)) stop("data must be a numeric vector")
  df <- vector()
  df_int <- vector()
  for(k in ndays:length(data)){
    for(j in 1:ndays){
      df_int[j] <- data[k-j+1]/j
    }
    df[k] <- sum(df_int)
  }
  return(df)
}





isZero = rbinom(n = 100, size = 1, prob = 0.6)
P = ifelse(isZero==1, 0, rlnorm(sum(isZero==0), meanlog = 0, sdlog = 1))
EP_calc(30,P)

