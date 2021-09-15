
library(RPostgres)
library(dplyr)
library(chunked)

#install.packages("lubridate")
#library(lubridate)

#install.packages("stringr")
#library(stringr)



#establish connection with postgres on server 

function_connect_to_postgresdbase_onserver <- function(){
  
  con <<-  dbConnect(RPostgres::Postgres(), user='postgres', password='Kefl@vik2018!', dbname='life', host='127.0.0.1', port = '5432')

  }


#chunking range and columns to be considered

function_def_chunking_observations <- function(){
  
  index <<- 1
  chunkSize <<- 1
  col_names <<- c(
                    "id",
                    "name",
                    "year",
                    "inhab",
                    "water",
                    "sew",
                    "comp",
                    "treat348",
                    "treat349",
                    "Strategia_data",
                    "Studium_Data",
                     "Rolnictwo",              
                     "Leśnictwo",  
                     "Rolnictwo_ekologiczne",
                     "Kopalnictwo",                
                     "Turystyka_agroturystyka",  
                     "Turystyka_rekreacja_wodna",  
                     "Prof_MSP",                   
                     "Prof_DF",
                     "Rolnictwo_p",
                     "Leśnictwo_p",                
                     "Rolnictwo_ekologiczne_p",  
                     "Kopalnictwo_p",
                     "Turystyka_agroturystyka_p",  
                     "Turystyka_rekreacja_wodna_p",
                     "Prof_MSP_p",
                     "Prof_DF_p",                  
                     "chic",
                     "cow",  
                     "hors",                       
                     "pigs",
                     "wapn",
                     "potas",                      
                     "azot", 
                     "fosfor",
                     "mineral",                    
                     "comp9",
                     "comp49",
                     "comp249",                    
                     "comp999",
                     "comp1000",
                     "forests",                    
                     "powiat",
                     "region"                     
    )
}  


#chunking - reading variables_dataframe  

function_chunking <- function(){
  
  write.csv(variables_dataframe[,c(-1)], "variables_dataframe.csv")
  vd_file   <<- file(description = "variables_dataframe.csv", open = "r")
  dataChunk <<- read.table(vd_file, nrows=chunkSize, header=T, fill=TRUE, sep=",", quote = '"', col.names = col_names )
  
}

#feeding postrges table with chunked data through established 'con' connection 

function_write_chunked_into_postgres_table <- function(){
  
  dbWriteTable(con, value = dataChunk, row.names = FALSE, name = "variables_dataframe", append = TRUE)

}

#check if table has been sucesfully created in postgres db

function_from_postgres_into_table <- function(){
  
  query <- "SELECT * FROM variables_dataframe;"
  results <<- dbSendQuery(con, query)
  variables_dataframe_from_post <<- dbFetch(results, n = -1)
  dbClearResult(results)
  glimpse(variables_dataframe_from_post)
  
}


###

function_connect_to_postgresdbase_onserver()

function_def_chunking_observations()

function_chunking()  

function_write_chunked_into_postgres_table()   

function_from_postgres_into_table()

 

