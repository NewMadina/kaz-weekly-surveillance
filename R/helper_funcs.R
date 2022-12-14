#' @description Extract vaccination data
#' @param x location of cumulative vaccination file 
#' @returns Tibble of vaccination information 
extract_vacc <- function(x){
  
  type_f4 <- grepl("f4",x)
  
  out <- read_excel(x, sheet = "Кесте-1", skip = 9)[,-c(1,17:26)] |> 
    set_names(
      c(
        "prov", 
        lapply(c("<1", "1", "6", "16"), function(x){
          paste0(x, c("_num_begin", "_num_died", "_trans_in", "_trans_out", "_num_end"))
        }) |> 
          unlist()
      )
    ) |> 
    filter(!is.na(prov)) |> 
    mutate(year = str_split(x, "_")[[1]][2] |> str_split("\\.") %>% {.[[1]][1]} |> as.numeric())
  
  if(type_f4){
    file <- read_excel(x, sheet = "Кесте-6", skip = 9) |> 
      set_names(
        c(
          "prov", 
          lapply(c("bopv1", "bopv2", "bopv3", "bopv4"), function(x){
            paste0(x, c("_total", "_right_age", "_pcov", "_adv"))
          }
          
          )
        ) |> 
          unlist()
      )
    
    out <- left_join(out, file, by = "prov")
    
    file <- read_excel(x, sheet = "Кесте-9", skip = 9)[,c(1,2:5,14:17)] |> 
      set_names(
        c(
          "prov",
          lapply(c("mmr1", "mmr2"), function(x){
            paste0(x, c("_total", "_right_age", "_pcov", "_adv")) 
          }
          ) |> 
            unlist()
        )
      )
    
    out <- left_join(out, file, by = "prov")
    
    file <- read_excel(x, sheet = "Кесте-11", skip = 9)[,c(1,10:17,50:57)] |>
      set_names(
        c(
          "prov",
          lapply(c("bopv_no_vacc", "mmr_no_vacc"), function(x){
            paste0(x, c("_total", "_temp_ci", "_perm_ci","_hcw", "_stockout", "_refusal", "_out_perm", "_out_temp")) 
          }
          ) |> 
            unlist()
        )
      )
    
    out <- left_join(out, file, by = "prov")
    
    out <- out |> 
      rowwise() |>
      mutate(prov = str_replace(prov, " облысы", ""))

  }else{
    file <- read_excel(x, sheet = "Кесте-6", skip = 9)[-c(2:5,10:13,18:21,26:29)] |> 
      set_names(
        c(
          "prov", 
          lapply(c("bopv1", "bopv2", "bopv3", "bopv4"), function(x){
            paste0(x, c("_total", "_right_age", "_pcov", "_adv"))
          }
          
          )
        ) |> 
          unlist()
      )
    
    out <- left_join(out, file, by = "prov")
    
    file <- read_excel(x, sheet = "Кесте-9", skip = 9)[,c(1,6:9,30:33)] |> 
      set_names(
        c(
          "prov",
          lapply(c("mmr1", "mmr2"), function(x){
            paste0(x, c("_total", "_right_age", "_pcov", "_adv")) 
          }
          ) |> 
            unlist()
        )
      )
    
    out <- left_join(out, file, by = "prov")
    
    
    file <- read_excel(x, sheet = "Кесте-11", skip = 9)[,c(1,10:17,42:49)] |>
      set_names(
        c(
          "prov",
          lapply(c("bopv_no_vacc", "mmr_no_vacc"), function(x){
            paste0(x, c("_total", "_temp_ci", "_perm_ci","_hcw", "_stockout", "_refusal", "_out_perm", "_out_temp")) 
          }
          ) |> 
            unlist()
        )
      )
    
    out <- left_join(out, file, by = "prov")
    
    
    out <- out |> 
      rowwise() |>
      mutate(prov = str_replace(prov, " облысы", ""))
    
  }
  
  out <- out %>%
    mutate_at(-1, as.numeric)
  
}
