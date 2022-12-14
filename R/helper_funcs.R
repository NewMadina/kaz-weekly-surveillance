#' @description Extract PCODE and DCODE from spatial information 
#' @param x WHO PCODE
#' @returns Tibble of PCODE and DCODE
extract_pcode <- function(x){
  
  y <- x |> 
    substr(1,11) |> 
    substr(3,11)
  
  PCODE <- substr(y, 4, 6)
  
  DCODE <- substr(y, 7, 9)
  
  tibble(
    "PCODE" = PCODE, 
    "DCODE" = DCODE
  )
  
}
