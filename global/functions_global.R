
# Full join function for full join more then two dataframes 

func_frames_full_join <- function (frame_name) {
  Reduce(full_join, frame_name)
}


# Correcting names for unit with the same but different id's

func_correct_name <- function () {

  df_total_result2$name <<- as.character(df_total_result2$name)
  df_total_result2 <<-  df_total_result2 %>%
  mutate(name=replace(name, id=="051011716011", "Tomaszów Mazowiecki wiejski"))
  df_total_result2 <<-  df_total_result2 %>%
  mutate(name=replace(name, id=="051011716092", "Tomaszów Mazowiecki miasto"))  
  df_total_result2$name <<- as.factor(df_total_result2$name)
  
  
}

# Handling missing units 

func_handle_missing <- function (missing) {
  if (
    nrow(missing) > 0) {
    message(
      "Data for following units not retrieved: \n", paste0(missing[1:nrow(missing),], "\n"))
  } else {
    print(
      "Data for all units received.")}
}

