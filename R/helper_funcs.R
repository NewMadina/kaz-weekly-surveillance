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

#' @description Extract measles data
#' @param x string: absolute path of measles data
#' @returns tibble with measles data 
extract_measles <- function(x){
  
  return(
    tryCatch(
      {
        
        sheet_num <- excel_sheets(x) |>
          tolower() %>%
          {which(. == "корь")}
        
        test <- read_excel(x, sheet = sheet_num) 
        
        
        #check which row contains our value of interest
        does_not_contain <- T
        row <- 0
        while(does_not_contain){
          row <- row + 1
          does_not_contain <- startsWith(as.character(test[row,]), "Возрастной") |> 
            sum(na.rm = T) %>%
            {. == 0 | is.na(.)}
        }
        
        file <- read_excel(path = x, sheet = sheet_num, skip = row) %>%
          {set_names(.,str_replace_all(names(.), fixed(" "), ""))} |>
          select(c(1, "Возрастнойдиапазонзаболевших", "...6", "...7", "...8", "...9", "...10", "...11", starts_with("Числоподтвержденных"))) %>%
          {.[-c(1,2),]} |>    
          set_names(c("prov", "<1", "1-4", "5-9", "10-14", "15-19", "20-29", ">30", "conf_cases"))
        
        file <- file[-c(which(file$prov == "РК"):nrow(file)),]
        
        y <- str_split(x, "_") %>%
          {
            list(
              "year" = substr(.[[1]][1],nchar(.[[1]][1])-3, nchar(.[[1]][1])), 
              "week" = substr(.[[1]][2],0, 2)
            )
          }
        
        file <- file |> 
          mutate(
            year = as.numeric(y$year), 
            week = as.numeric(y$week)
          )
        
        file <- file %>%
          mutate_at(-1, as.numeric)
        
        return(file)
      },
      error = function(e){return(x)}
    )
  )
}

