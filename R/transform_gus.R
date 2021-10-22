#' Integrate datasets retrieved with 'retrieve_gus' function into one dataframe

#' @return Dataframe in "transformed".rds format file in ```retrieveddata``` directory. Dataframe includes data from all topic.rds type datasets stored in ```retrieveddata``` directory.
#' @examples
#' transform_gus()
#'

#' @export

transform_gus <- function (save = TRUE){

     if (dir.exists(paste0(getwd(), "/retrieveddata"))) {

       # Search for datasets in retrieveddata directory
      #setwd(paste0(getwd(), "/retrieveddata"))
      filesx <- list.files("retrieveddata", pattern = ".rds")

      if (length(filesx) > 0) {

        filesx <- as.matrix(filesx)
        filesep <- list()
        message("...preparing integrated dataframe. See result in 'transformeddata' directory.")

            # Feed dataframe with data from retrieved datasets
            for (i in 1:nrow(filesx)) {

              filex <-  as.data.frame(read_rds(as.character(paste0("retrieveddata/",(filesx[i])))))
              filesep[[i]] <- filex
              }

              retrieved_dataframe <- func_frames_full_join(filesep) %>% replace(., is.na(.), 0)

              # Write results to .rds
              if (save) {
              dir.create("transformeddata")
              filename <- "transformeddata/transformed.rds"
              write_rds(retrieved_dataframe, filename)
              }
      }

              else {
              message("Directory 'retrieveddata' empty.")
              }


  }
              else {
              message("No directory with retrieved data.")
              }

              return(retrieved_dataframe)

}
