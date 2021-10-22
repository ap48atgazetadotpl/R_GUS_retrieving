
# Full join function for full join of retrieved dataframes (transforming dataframes into one dataframe)

func_frames_full_join <- function (frame_name) {
  Reduce(full_join, frame_name)
}

# Handling missing units (after retrieving data from GUS BDL with function retrieve_gus)

func_handle_missing <- function (missing) {

  if (
    nrow(missing) > 0) {
    message(
      "Data for following units not retrieved: \n", paste0(missing[1:nrow(missing),], "\n"))
  } else {
    message(
      "Data for all units received.")}
}


