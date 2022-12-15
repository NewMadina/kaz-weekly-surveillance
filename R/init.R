#load here
library(here)

#basic folder structure
c(
  here("R"), 
  here("output"), 
  here("resources"),
  here("data"), 
  here("data/raw"),
  here("data/raw/polio"),
  here("data/raw/measles"),
  here("data/int"), 
  here("data/int/polio"),
  here("data/int/measles"), 
  here("data/raw/spatial"), 
  here("data/raw/pop"),
  here("data/raw/polio/afp"),
  here('data/raw/polio/vacc_cov')
) |> 
  sapply(dir.create)


original_locale <- Sys.getlocale()
write_rds(original_locale, "resources/original.locale.rds")
rm(original_locale)
