#Figure 1
fig1 <- kaz$pop |> 
  #filtering out all overlapping categories that we may not want to plot
  filter(!type %in% c(
    "Общая численность населения", 
    "дети от 0 до 2-х лет включительно", 
    "дети до 14 лет включительно"
  )) |>
  mutate(type = factor(type, levels = c(
    "дети от 0 до 4-х лет включительно", 
    "дети от 5 до 14 лет", 
    "взрослые от 15 до 29 лет", 
    "взрослые от 30 до 64 лет", 
    "взрослые от 65 и старше"
  ), 
  labels = c(
    "Дети 0-4 года", 
    "Дети 5-14 лет", 
    "Взрослые 15-29 лет", 
    "Взрослые 30-64 лет", 
    "Взрослые 65 лет и старше"
  ))) |>
  mutate(value = as.numeric(value)) |>
  group_by(name, year) |> 
  mutate(value = value/sum(value)) |>
  ggplot() + 
  geom_histogram(stat = "identity", aes(x = type, y = value, fill = type)) + 
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~name) +
  theme_bw() + 
  theme(
    axis.text.x = element_blank(), 
    axis.ticks = element_blank(),
    axis.title.x = element_blank(), 
    legend.position = "bottom",
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  guides(fill=guide_legend(nrow=2, byrow=TRUE)) +
  labs(fill = "Тип", y = "Процент", 
       title = "Возрастное распределение, РК, 2022 г.", 
       caption = "Размер групп возрастает, значит пропорция должна возрастать \nаналогичным образом. Области, в которых этот тренд не соблюдается, \nмогут указывать на увеличение бремени молодого или старшего населения")

ggsave(here("output/fig1.png"),plot = fig1, dpi = 300,
       width = 8, height = 6)


z <- kaz$afp |> 
  mutate(prov = case_when( 
    prov == "Мангыстауская" ~ "Мангистауская", 
    prov == "Жамбылский" ~ "Жамбылская", 
    T ~ prov)) |>
  mutate(month = month(DONSET), 
         year = year(DONSET)) |> 
  group_by(prov, month, year) |> 
  summarize(count = n()) |> 
  ungroup() |>
  complete(month, year, prov) |>
  arrange(prov, year, month) %>% 
  replace_na(list("count" = 0)) |>
  left_join(kaz$pop_extrapolated, 
            by = c("prov" = "name", "year", "month")) %>% 
  mutate(afp_rate = count / value * 10000) |> 
  drop_na() |>
  mutate(date = paste0(year,"-",month,"-01")) %>%
  mutate(date = as_date(date))

order_of_provs <- z |> group_by(prov) |> summarize(avg = mean(afp_rate)) |> arrange(-avg) |> pull(prov)

fig2 <- z |>
  mutate(prov = factor(prov, levels = rev(order_of_provs))) |>
  mutate(afp_rate = case_when(
    afp_rate == 0 ~ "0",
    afp_rate > 0 & afp_rate <= 1 ~ ">0, <= 1",
    afp_rate >1 & afp_rate <= 2 ~ ">1, <= 2", 
    afp_rate >2 &  afp_rate <= 3 ~ ">2, <= 3", 
    afp_rate > 3 ~ "> 3"
  )) |> 
  mutate(afp_rate = factor(afp_rate, 
                           levels = c(
                             "0",
                             ">0, <= 1",
                             ">1, <= 2",
                             ">2, <= 3",
                             "> 3"
                           ))) |>
  ggplot() + 
  geom_tile(aes(x = date, y = prov, fill = afp_rate), 
            color = "white", size = 0.02) + 
  #scale_fill_viridis_d(option = "plasma") +
  scale_fill_brewer(palette = "Reds", direction = 1) +
  #scale_fill_brewer(palette = "Diamond\nclarity", direction = 1) +
  theme_classic() + 
  labs (fill = "Показатель\nзаболеваемости\nОВП", y = "Территории", x = "Дата",
       title = "Динамика заболеваемости ОВП, РК, 2013-2022 гг.", 
       caption = "Несмотря на выявление ОВП в большинстве территорий, существуют несколько территорий с нулевым показателем заболеваемости ОВП в течении последних 2 лет")


ggsave(here("output/fig2.png"), fig2, height = 3, width = 12, dpi = 300)


