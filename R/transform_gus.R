#' Integrate datasets retrieved with 'retrieve_gus' function into one dataframe
#'
#' @param save {TRUE or FALSE; default = TRUE - save retrieved data into .rds file into ```transformeddata``` directory}
#'
#' @return Dataframe including data from all topic.rds type datasets stored in ```retrieveddata``` directory.
#' If ```save = TRUE``` the file ```transformeddata.rds``` in ```transformeddata``` directory is created.
#'
#' @examples
#' ## Not run:
#' transform_gus()
#' ## End(Not run)
#'
#' @export

transform_gus <- function (save = TRUE){

     if (dir.exists(paste0(getwd(), "/retrieveddata"))) {

       # Search for datasets in retrieveddata directory
      filesx <- list.files("retrieveddata", pattern = ".rds")

      if (length(filesx) > 0) {

        filesx <- as.matrix(filesx)
        filesep <- list()

            # Feed dataframe with data from retrieved datasets
            for (i in 1:nrow(filesx)) {

              filex <-  as.data.frame(read_rds(as.character(paste0("retrieveddata/",(filesx[i])))))
              filesep[[i]] <- filex
              }

              retrieved_dataframe <- func_frames_full_join(filesep)
              retrieved_dataframe <- replace(retrieved_dataframe, is.na(retrieved_dataframe), 0)

              # Write results to .rds
              if (save) {
              dir.create("transformeddata")
              filename <- "transformeddata/transformed.rds"
              write_rds(retrieved_dataframe, filename)
              }

              return(retrieved_dataframe)
      }

              else {
              message("Directory 'retrieveddata' empty.")
              }


  }
              else {
              message("No directory with retrieved data.")
              }


}
