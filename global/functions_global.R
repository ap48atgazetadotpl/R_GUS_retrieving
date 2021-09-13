
# Full join function for full join more then two dataframes 

func_frames_full_join <- function (frame_name) {
  Reduce(full_join, frame_name)
}
