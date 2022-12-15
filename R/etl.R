source(here("R", "packages.R"))
source(here("R", "init.R"))
source(here("R", "helper_funcs.R"))

#create list object to carry all necessary data
kaz <- list()

#Load in spatial data 
kaz$adm0 <- read_rds(here("data", "raw", "spatial", "kaz0.rds"))
kaz$adm1 <- read_rds(here("data", "raw", "spatial", "kaz1.rds"))
kaz$adm2 <- read_rds(here("data", "raw", "spatial", "kaz2.rds"))

#Load in translations of spatial regions from english, russian and kazakh
kaz$trans0 <- read_excel(here("data", "raw", "spatial", "kaz_gazetteer_2019.xlsx"), sheet = "kaz_adm0")
kaz$trans1 <- read_excel(here("data", "raw", "spatial", "kaz_gazetteer_2019.xlsx"), sheet = "kaz_adm1")
kaz$trans2 <- read_excel(here("data", "raw", "spatial", "kaz_gazetteer_2019.xlsx"), sheet = "kaz_adm2")

#Load in AFP data and merge in spatial information and trans
kaz$afp <- list.files(here("data", "raw", "polio", "afp"), full.names = T) |> 
  lapply(read_excel) |> 
  bind_rows() |> 
  mutate(
    DCODE = as.character(DCODE), 
    PCODE = as.character(PCODE),
    DCODE = case_when(
      PCODE == "011" & is.na(DCODE) ~ "1107", 
      PCODE == "013" & is.na(DCODE) ~ "1326", 
      T ~ DCODE
    )
  )

#Load in population data
x <- list.files(here("data", "raw", "pop"), pattern = c(".xls"), full.names = T)[14]

y <- read_excel(x, skip = 2)[2:9,-1] 
y <- y[,-c(53,54,55)]
names <- names(y)[!startsWith(names(y), ".")]
y <- y[,c(1,seq(4,52,3))]
names(y) <- names

y <- pivot_longer(y, 2:18) |> 
  set_names(c("type", "name", "value")) |> 
  mutate(year = 2022) |> 
  arrange(name)

kaz$pop <- y
rm(y)
rm(names)
rm(x)

kaz$pop_16 <- kaz$pop |> 
  mutate(
    name = case_when(
      name %in% c("Туркестанская", "г. Шымкент") ~ "Южно-Казахстанская", 
      name == "г.Астана" ~ "г. Астана",
      T ~ name
    )
  ) %>% 
  mutate(value = as.numeric(value)) |>
  group_by(name, type, year) |> 
  summarize(value = sum(value)) |> 
  ungroup()

#load in pcode and dcode 
x <- read_excel(here("data/raw/spatial/sp_code.xlsx")) |> 
  set_names(c("PCODE", "prov", "DCODE", "dist")) |> 
  mutate(DCODE = as.character(DCODE), 
         PCODE = as.character(PCODE)) |>
  mutate(
    DCODE = case_when(
      PCODE == "11" & is.na(DCODE) ~ "1107",
      PCODE == "13" & is.na(DCODE) ~ "1326", 
      PCODE == 15 ~ "1501", 
      PCODE == 16 ~ "1601", 
      T ~ DCODE
    )
  )

kaz$afp <- left_join(kaz$afp, x, by = "DCODE")

y <- filter(kaz$pop_16, type == "дети до 14 лет включительно")

test_pop <- lapply(2013:2021, function(x){
  y |> 
    mutate(value = value*(1/1.015^(2022-x)), 
           year = x)
}) |> 
  bind_rows() |> 
  bind_rows(y)

kaz$pop_extrapolated <- test_pop |> 
  ungroup() |>
  group_by(year) |> 
  group_split() |>
  lapply(function(x){
    lapply(1:12, function(y){
      x |> 
        mutate(month = y, 
               value = value / 12)
    }) |> 
      bind_rows()
  }) |> 
  bind_rows() 

rm(x)
rm(test_pop)
rm(y)

#read in vaccination coverage data
x <- list.files(here("data/raw/vaccination/cumulative/"), full.names = T) |>
  lapply(extract_vacc) |> 
  bind_rows() |> 
  mutate(prov = case_when(
    prov == "Ақмола" ~ "Акмолинская",
    prov == "Ақтөбе" ~ "Актюбинская",
    prov == "Алматы" ~ "Алматинская",
    prov == "Атырау" ~ "Атырауская",
    prov == "Шығыс Қазақстан" ~ "Восточно-Казахстанская",
    prov == "Жамбыл" ~ "Жамбылская",
    prov == "Батыс-Казақстан" ~ "Западно-Казахстанская",
    prov == "Қарағанды" ~ "Карагандинская",
    prov == "Қостанай" ~ "Костанайская",
    prov == "Кызылорда" ~ "Кызылординская",
    prov == "Манғыстау" ~ "Мангистауская",
    prov == "Павлодар" ~ "Павлодарская",
    prov == "Солтұстіқ-Казақстан" ~ "Северо-Казахстанская",
    prov == "Оңтустіқ-Казақстан" ~ "Южно-Казахстанская",
    prov == "Алматы қаласы" ~ "г. Алматы",
    prov == "Астана қаласы" ~ "г. Астана",
    prov == "Туркістан" ~ "Туркестанская",
    prov == "Нұр-Сұлтан қаласы" ~ "г. Астана",
    prov == "Шымкент қаласы" ~ "г. Шымкент",
    T ~ prov
  ))


x <- x |> 
  #mutate(mmr2_pcov = ifelse((prov == "Западно-Казахстанская"), mmr2_right_age/`6_num_end`*100, mmr2_pcov)) |>
  mutate(
    `6_num_end` = `6_num_begin`-`6_num_died`+`6_trans_in`-`6_trans_out`,
    mmr2_pcov = mmr2_right_age/`6_num_end`*100,
    adj_mmr2_pcov = mmr2_right_age/(`6_num_end`+mmr_no_vacc_refusal+mmr_no_vacc_perm_ci+(mmr_no_vacc_temp_ci/12))*100
  ) |>
  mutate(diff_mmr2_pcov = mmr2_pcov - adj_mmr2_pcov) |> 
  mutate(year = factor(year, levels = as.character(2013:2022)),
         diff_mmr2_pcov = diff_mmr2_pcov / 100)

kaz$vacc_cov <- x

rm(x)

#load measles data
x <- list.files(here("data/raw/measles"), full.names = T, recursive = T, pattern = "xls") |>
  lapply(extract_measles)

error_files <- x[lapply(x, is.character) |> unlist()] |> 
  unlist()

measles <- x[lapply(x, is_tibble) |> unlist()] |> 
  bind_rows() |> 
  filter(year >= 2017) |> 
  arrange(year, week, prov)


kaz$measles <- measles 

kaz$measles <- kaz$measles |> 
  mutate(
    prov = case_when(
      prov == "Алматы" ~ "г. Алматы",
      prov == "Астана" ~ "г. Астана",
      prov == "ВКО" ~ "Восточно-Казахстанская",
      prov == "Жамбыльская" ~ "Жамбылская",
      prov == "ЗКО" ~ "Западно-Казахстанская",
      prov == "СКО" ~ "Северо-Казахстанская",
      prov == "ЮКО" ~ "Южно-Казахстанская",
      prov == "Шымкент" ~ "г. Шымкент",
      prov == "Задано-Казахстанская" ~ "Западно-Казахстанская",
      prov == "Туркестанскяа" ~ "Туркестанская",
      prov == "г.Нур-Султан" ~ "г. Астана",
      prov == "г.Шымкент" ~ "г. Шымкент",
      prov == "Нур-Султан" ~ "г. Астана",
      prov == "г.Алматы" ~ "г. Алматы",
      prov == "г. Нур-Султан" ~ "г. Астана",
      prov == "Турскестанская" ~ "Туркестанская", 
      T ~ prov
    )
  ) 

inc_measles <- kaz$measles |> 
  group_by(year, prov) |> 
  arrange(week) |> 
  mutate(
    `<1` = `<1` - lag(`<1`, default = `<1`[1]), 
    `1-4` = `1-4` - lag(`1-4`, default = `1-4`[1]), 
    `5-9` = `5-9` - lag(`5-9`, default = `5-9`[1]), 
    `10-14` = `10-14` - lag(`10-14`, default = `10-14`[1]), 
    `15-19` = `15-19` - lag(`15-19`, default = `15-19`[1]), 
    `20-29` = `20-29` - lag(`20-29`, default = `20-29`[1]), 
    `>30` = `>30` - lag(`>30`, default = `>30`[1]), 
    `conf_cases` = `conf_cases` - lag(`conf_cases`, default = `conf_cases`[1]), 
  )

kaz$inc_measles <- inc_measles

rm(measles)
rm(inc_measles)
rm(x)

write_rds(kaz, file = here("data/int/kaz.rds"))
write_rds(error_files, file = here("data/int/measles_error_files.rds"))

