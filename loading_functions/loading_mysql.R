##############################################################################
###Functions to execute#######################################################
##############################################################################

#install.packages("rtweet")
library(rtweet)

#install.packages("dplyr")
library(dplyr)

#install.packages("RMySQL")
library(RMySQL)

#install.packages("chunked")
library(chunked)

#install.packages("lubridate")
library(lubridate)

#install.packages("stringr")
library(stringr)

function_variable_declarations_res_query()    

function_variable_declaration_dftotal()

function_retrieve_tweets()

function_variable_declaration_dftotal2()

function_limit_columns_retrieved_tweets()

function_save_to_csv()

function_connect_to_dbase_onserver()

function_def_chunking_variables()

function_chunking()  

mydb =  dbConnect(MySQL(), user='root2', password='Kefl@vik2018!', dbname='twitter', host='127.0.0.1')

function_write_chunked_into_mysql_table()   

function_from_server_into_table()

function_transform()

#function_dtm() # - for data mining purposes - another app 

function_variable_declarations_term_period_occur()

function_select_records(date_search_start, date_search_stop)  

function_count_term_occurancies(k, term)

function_transform_into_numeric()

function_count_term_occurancies_day()

##############################################################################
###PART I#####################################################################
#declaration of all variables neccessary for retrieving data from twitter api#
#construction of a query######################################################
##############################################################################

function_variable_declarations_res_query <- function(){
  
  #declaration of search period scope
  #retrieving data once in week for given period, not earlier than six days
  date1 <<- "2021-09-11" #VARIABLE
  date2 <<- "2021-09-15" #VARIALBE
  
  #declaration of research vector terms 
  twitter_handles <<- c("800v",  
                        "BEV charging", 
                        "electric vehicle", 
                        "electric vehicles", 
                        "BEV charging", 
                        "BEV chargers", 
                        "BEV battery", 
                        "BEV batteries", 
                        "electric boat", 
                        "electric boats",
                        "electric van", 
                        "electric vans", 
                        "electric truck", 
                        "electric trucks", 
                        "electric ferry",
                        "electric ferries", 
                        "electric ship",
                        "electric ships",
                        "electric vessel",
                        "electric vessels",
                        "electric crane"
  )                       #VARIABLE
  
  #dataframe for res query - definition (list) 
  df <- data.frame(twitter_handles)
  #transformation of vector of handles into characters (list)
  res_query <- as.character(df$twitter_handles)
  #unlist res query 
  res_query <<- unlist(strsplit(res_query,","))
  
}


##############################################################################
###PART II####################################################################
#retrieving tweets according to res_query#####################################
##############################################################################

#functionII.1 - declaration of empty dataframe to store incrementing observations 

function_variable_declaration_dftotal <- function(){
  
  
  #df_total as global variable 
  df_total <<-  data.frame()
}

#functionII.2 - loop for collecting tweets through rtweet into df_total dataframe, 
#according to terms given in r esquery with incremental storage into dataframe; 
#superassignment operator to enable local as global variable  

function_retrieve_tweets <- function(){
  
  for (i in 1:length(res_query)){
    # vector output
    result <- search_tweets(res_query[i],n=18000, since = date1, until = date2, include_rts = FALSE)
    # add vector to a dataframe
    dfresult <- data.frame(result)
    df_total <<- rbind(df_total,dfresult)
    #view(df_total) #to comment in production 
  }
  
}

# function II.3 - adjusting table structure to future needs - new dataframe df_total2; using <<- 
# to access local (df_total2) as global from within the function
# chars remain as integer range is not enough in R; resigned from converting 
# to double on purpose; in conseqence they are text fields further


#function II.4 - declaration of empty dataframe to store incrementing observations 

function_variable_declaration_dftotal2 <- function(){
  
  #df_total2 as global variable 
  df_total2 <<-  data.frame()  #
}


#function II.5 - limiting number of colums from retrieved data as not all 
#are neccessary

function_limit_columns_retrieved_tweets <- function(){
  ##https://www.datamentor.io/r-programming/environment-scope/ 
  df_total2 <<- (df_total[,c(1:6,8:10,32)])
  #view(df_total2)
}


##############################################################################
###PART III###################################################################
#save retrieved data to csv###################################################
##############################################################################

# function for saving retrieved data into csv

function_save_to_csv <- function(){
  #retrieved tweets in table df_total2 to csv as df_total2_twitter.csv
  write.csv(df_total2, "df_total1_twitter.csv")
  
}

##############################################################################
###PART IV####################################################################
#connection to mysql on server################################################
##############################################################################


#function connection to mysql on server, dbase: twitter 

function_connect_to_dbase_onserver <- function(){
  
  #install.packages("RMySQL")
  #library(RMySQL)
  #defining access parameters; dbase (twitter) should be created in advance 
  #mydb =  dbConnect(MySQL(), user='root2', password='Kefl@vik2018!', dbname='twitter', host='127.0.0.1')
  #defining filename as feeding source 
  con <<- file(description = "df_total1_twitter.csv", open = "r")
  
}


##############################################################################
###PART V#####################################################################
#creation of table: tweeter.tweets############################################
#feeding from csv df_totalX_twitter.csv into table tweeter.tweets on server###
##############################################################################

#function V.1. chunking range and columns to be considered

function_def_chunking_variables <- function(){
  
  index <<- 1
  chunkSize <<- 100000
  col_names <<- c(
    "id", 
    "user_id",
    "status_id",
    "created_at",
    "screen_name",
    "text",
    "source",
    "reply_to_status_id",
    "reply_to_user_id",
    "reply_to_screen_name",
    "lang"
  )
}  


#function V.2. chunking - reading table 

function_chunking <- function(){
  
  dataChunk <<- read.table(con, nrows=chunkSize, header=T, fill=TRUE, sep=",", quote = '"', col.names = col_names)
  
}

#function V.3. - feeding chunked data into twitter.tweets table through mydb connection

function_write_chunked_into_mysql_table <- function(){
  
  dbWriteTable(mydb, value = dataChunk, row.names = FALSE, name = "tweets", append = TRUE)
  #close.connection()
  
}


#####################################################################
###PART VI###########################################################
###From mysql server twitter.tweets into table in memory for#########
###futher operations and to prevent changes on server################
#####################################################################

#function VI.1. from mysql in server into R table "tweets_results" 
#and reference as "tweets_results" for further work

function_from_server_into_table <- function(){
  
  query <- "SELECT * FROM tweets;"
  results <<- dbSendQuery(mydb, query)
  tweets_results <<- dbFetch(results, n = -1)
  dbClearResult(results)
  glimpse(tweets_results)
  
}


#function VI.2. transformations of variables into needed formats 

function_transform <- function(){
  
  #transform "created_at" column into "date" format 
  
  library(lubridate)
  tweets_results <<- tweets_results %>% mutate(created_at = ymd_hms(created_at)) 
  
  #transform characters "lang" column from chars into factors for classification 
  tweets_results <<- tweets_results %>% mutate(lang = factor(lang))
  
  #for further preprocessing retrieval of day,month,year and creation of new columns 
  tweets_results <<- tweets_results %>% 
    mutate(
      created_at_year = year(created_at),
      created_at_month = month(created_at), 
      created_at_day = day(created_at))
  
  glimpse(tweets_results)
  
}


##################################################################
######Part VIII###################################################
######Statistics for term occurances##############################
##################################################################

#function VIII.1.#################################################
#defining term "XXXXX" to calculate occurance in given period#####
#from table tweets_results: search term definition################

function_variable_declarations_term_period_occur <- function(){
  
  #declaration of term to calculate its occurance 
  term <<- "800" #VARIABLE
  
  #defining period of term search - scope of search 
  date_search_start <<- as.Date("2021-01-02") ##VARIABLE
  date_search_stop <<- as.Date("2021-01-06") ##VARIABLE 
  
}

#function VIII.2 ##################################################
#from table "tweets_results" select records from given period -#### 
#into new table "tweets_results_selected" #########################


function_select_records <- function(date_search_start, date_search_stop) {
  
  tweets_results_selected <<- 
    tweets_results %>% 
    select(created_at, 
           text, 
           created_at_year, 
           created_at_month,  
           created_at_day
    ) 
  
  tweets_results_selected <<- tweets_results_selected %>% 
    filter(date_search_start < created_at & created_at < date_search_stop)   
  
}


#function VIII.3 ##################################################
#retrieve counts "1" if term is present in specific tweet##########
###################################################################

function_count_term_occurancies <- function(k, term){
  
  k <- nrow(tweets_results_selected) 
  
  {
    #occurancies - "1" in each tweet in new column "occurance" if term is present  
    tweets_results_selected <<- tweets_results_selected %>% 
      mutate(occurance = ifelse(str_detect(tweets_results_selected$text[1:k], term), "1", "0")) 
  }
  
}  


#function VIII.4. str_detect is boolean - transform into numeric######
######################################################################

function_transform_into_numeric <- function(){
  
  tweets_results_selected$occurance <<- as.numeric(as.character(tweets_results_selected$occurance))
  
  glimpse(tweets_results_selected)
  
}

#function VIII.5 count term occurancies in all tweets per day 
#############################################################

function_count_term_occurancies_day <- function(){
  
  #grouping by "days" to retrieve term occurancies from tweets from each day  
  tweets_results_selected_summarised <<- 
    tweets_results_selected %>% 
    group_by(created_at_day, created_at_month, created_at_year) %>% 
    summarise(occurance = sum(occurance)) %>% 
    arrange(created_at_year, created_at_month, created_at_day)
  
  glimpse(tweets_results_selected_summarised)
}


