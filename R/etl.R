source(here("R", "init.R"))
source(here("R", "packages.R"))

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

#load vaccine data







