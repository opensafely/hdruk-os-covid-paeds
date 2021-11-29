library("tidyverse")

df_os = read_csv(
  here::here("output", "input.csv"),
  col_types = cols(
    patient_id = col_integer(),
    age = col_double())
)

plot_age = df_os %>%
    ggplot(aes(x = age)) + 
  geom_histogram()

ggsave(
  plot= plot_age,
  filename="hist_age.png", path=here::here("output"),
)


