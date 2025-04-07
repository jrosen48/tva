library(tidyverse)
library(googlesheets4)
library(googledrive)

posts <- read_sheet("https://docs.google.com/spreadsheets/d/1dDv3Zib0TC9R5GzjACkN6MzsneCPp6w7BPEJ8Kcjp94/edit?gid=2035831840#gid=2035831840")

folder <- drive_get("https://drive.google.com/drive/folders/1oD6VagXmO0fHFD0GF14P7RbVaXSlAPb_")

csv_files <- drive_ls(path = folder) %>%
  filter(grepl("\\.csv$", name))

# Read all CSVs into a list
csv_data_list <- map2(
  .x = csv_files$id,
  .y = csv_files$name,
  .f = function(file_id, file_name) {
    temp_path <- tempfile(fileext = ".csv")
    drive_download(as_id(file_id), path = temp_path, overwrite = TRUE)
    read_csv(temp_path)
  }
)

names(csv_data_list) <- csv_files$name

combined_df <- imap_dfr(
  .x = csv_data_list,
  .f = ~ mutate(.x, source_file = .y)
)

combined_df <- combined_df |> 
  mutate(date_of_extract = str_sub(source_file, end = 10)) |> 
  select(date_of_extract, everything())

combined_df <- combined_df |> 
  select(date_of_extract, post_text = Title, everything())

posts <- posts |> 
  mutate(date_of_post = str_sub(`Post Date`, end = 10)) |> 
  fill(Week, .direction = "down") |> 
  rename(post_text = `Post Text Caption`) |> 
  select(date_of_post, post_text, Condition)

combined_df <- combined_df |> 
  mutate(post_text = str_replace(post_text, '^"', ''))

combined_df <- combined_df |> 
  mutate(post_text = str_sub(post_text, end = 20))

posts <- posts |> 
  mutate(post_text = str_sub(post_text, end = 20))

combined_df
posts

combined_df <- combined_df |> 
  left_join(posts, by = join_by(post_text))

combined_df |> 
  select(date_of_extract, date_of_post, Condition, post_text, 
    Reactions, Shares, Views, Impressions, `Total clicks`, `Link Clicks`) |>
  gather(key, val, -date_of_extract, -date_of_post, -Condition, -post_text) |> 
  filter(!is.na(date_of_post)) |> 
  mutate(date_of_extract = lubridate::ymd(date_of_extract)) |> 
  mutate(date_of_post = lubridate::ymd(date_of_post)) |> 
  mutate(day_diff = date_of_extract - date_of_post) |> 
  ggplot(aes(y = val, x = day_diff, group = post_text, color = Condition)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  scale_color_brewer(palette = 1, type = "qual") +
  facet_wrap(~key, scales = "free_y")
