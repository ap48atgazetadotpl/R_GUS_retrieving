#' Retrieve data for Pilica Catchment NUTS6 units
#'
#' @description {Retrieve data for 89 Pilica Catchment NUTS6 units from Statistics Poland (GUS BDL) database through GUS BDL API}
#'
#' @param topic {string selected from the list of following topics:
#' - forests
#' - inhabitants
#' - companies_Ikw_0_9
#' - companies_Ikw_10_49
#' - companies_Ikw_50_249
#' - companies_Ikw_250_999
#' - companies_Ikw_1000
#' - companies_Ikw_total
#' - farms_mineral_2010
#' - farms_wapn_2010
#' - farms_potas_2010
#' - farms_azot_2010
#' - farms_fosfor_2010
#' - treatment_plants349
#' - treatment_plants348
#' - waterworks
#' - sewers
#' - farms_cattle_cows_2010
#' - farms_cattle_pigs_2010
#' - farms_cattle_chick_2010
#' - farms_cattle_hors_2010
#' - farms_total_ha
#' - farms_to_1ha
#' - farms_over_1ha
#' - farms_1_5_ha
#' - farms_5_10_ha
#' - farms_10_15_ha}
#'
#' @param save {TRUE ora FALSE; default = TRUE - save retrieved data into .rds file into /retrieveddata directory}
#'
#' @return Dataframe including retrieved data for Pilica Catchment NUTS6 units for selected "topic". If ```save = TRUE``` the file ```topic.rds``` in ```retrieveddata``` directory is created.
#' If data for some NUTS6 have been not retrieved successfully, their names appear in message after function execution.
#'
#' @examples
#' ## Not run:
#' retrieve_gus("farms_fosfor_2010")
#' ## End(Not run)

#' @import dplyr
#' @importFrom readr write_rds read_rds
#' @importFrom rjson fromJSON
#' @importFrom RCurl getURL


#' @export

retrieve_gus <- function (topic, save = TRUE) {

  # Initialize variables neccessary for data retrieval

  url  <- list() # Empty list with URL providing access to GUS API with data of selected variable for one statistical unit.
  urls <- list() # Empty list of URLs providing access to GUS API with data of selected variable for all 91 statistical units.
  df_total_result2 <- data.frame() #  Empty dataframe for results

  # Define ```topic``` as index of appropiate variable in ```variables_matrix```
  for (i in 1:nrow(variables_matrix))

    ifelse (variables_matrix$v_name[i] == topic, index <- i, 0)

        # Create list of URLs with variable codes retrieved for selected variable from ```variables_matrix``` ([index,2] for every NUTS6 unit (from ```units_codes``` dataframe)
        for (k in 1:89) {url <- paste0("https://bdl.stat.gov.pl/api/v1/data/by-variable/", variables_matrix[index,2], "?format=json&page-size=30&unit-parent-id=0", units_codes[[k]], "&unit-level=6")
        urls <- rbind(url, urls)
        }

              for (m in 1:89) {# Iterate over 89 elements included in the URLs list

                sampledatagus2 <- fromJSON(getURL(urls[[m]]))
                message(paste0("Retrieving data for unit:", m ))
                #glimpse(sampledatagus2) // option for display progress 1
                #message(units_codes[[m]]) //option for display progress 2


                id <- sampledatagus2$results[[1]]$id
                name <- sampledatagus2$results[[1]]$name

                      for (j in 1:as.integer(variables_matrix[index,3])){ # Iterate over j elements in retrieved json - as number of observations (years) == variables_matrix[index,3]

                        year <- sampledatagus2$results[[1]]$values[[j]]$year
                        val <- sampledatagus2$results[[1]]$values[[j]]$val
                        result2 <- data.frame(id, name, year, val)
                        df_total_result2 <- rbind(df_total_result2,result2)

                      }

              }

  # Test - check if the file with retrieved data is not empty, otherwise check if there are missing data
  if (
    nrow(df_total_result2) == 0) {
    message(
      "No data retrieved. This error is caused by too many request for data to GUS server. Try again after 15 minutes. \n")
  } else {

    # Set approppiate name for values column
    colnames(df_total_result2) <- c("id", "name", "year", topic)

    # Write results to .rds
    if (save) {
      dir.create("retrieveddata")
      filename <- paste0("retrieveddata/", topic, ".rds", sep="")
      write_rds(df_total_result2, filename)
    }


    # Test - identify and print NUTS6 units (gmina) out of 89 selected, which data has been not retrieved
    retrieved <- data.frame("name" = unique(df_total_result2$name))
    template <- data.frame("name" = unique(key_gen$name))
    missing <- anti_join(template, retrieved)
    func_handle_missing (missing)

  }

    return(df_total_result2)

 }



