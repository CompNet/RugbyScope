
df_2 <- reticulate::py$df %>% as_tibble()

tail(df_2)
df_2$scrapped_data[[1156]]
df_2$scrapped_data[[1157]]

df_2 %>% filter(len == 13)
df_2$scrapped_data[[881]]


have_all_bits <- function(test){
  testnames <- c("Date of birth", "Height", "Weight", "Position(s)")
  test1 <- testnames %in% names(test)
  testvalues <- c("Senior career")
  test2 <- testvalues %in% unique(unlist(test))
  is_all_bits <- all(test1, test2)
  return(is_all_bits)
}

df_2 <- df_2 %>% mutate(
  len = map_dbl(scrapped_data, .f = length),
  all_bits = map_lgl(scrapped_data, .f = have_all_bits)
)

df_2 %>% count(all_bits)
df_2 %>% count(len) %>% print(n = Inf)

df_2 %>% filter(len >= 4, all_bits == FALSE) %>% print(n=Inf)

df_2$scrapped_data[[1140]]
df_2$scrapped_data[[1141]]
df_2$scrapped_data[[1070]]
df_2$scrapped_data[[1041]]
df_2$scrapped_data[[1039]]

df_2$scrapped_data[[923]]
df_2 %>%  mutate(number = as.numeric(number)) %>% print(n=Inf)
df_2 %>%  mutate(number = as.numeric(number)) %>% 
  filter(len >= 4, number >=923) %>% print(n=Inf)


df_2 %>%  mutate(number = as.numeric(number)) %>% 
  count(opponent) |> 
  ggplot(aes(y = reorder(opponent, n), x = n, fill = n)) + 
  geom_bar(stat="identity") + 
  cowplot::theme_cowplot(14) +
  scale_fill_viridis_c() + 
  theme(legend.position = "none")

df_2 %>%  mutate(number = as.numeric(number)) %>% 
  filter(number >=923) %>%
  count(opponent) |> 
  ggplot(aes(y = reorder(opponent, n), x = n, fill = n)) + 
  geom_bar(stat="identity") + 
  cowplot::theme_cowplot(14) +
  scale_fill_viridis_c() + 
  theme(legend.position = "none")
  