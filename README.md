# ETL for Life 

ETL tool for Life Pilica Catchment Project.  

## Components

### Extract 

The process is developed automatically, with following steps:

 - receiving selected data from Statistics Poland (Główny Urząd Statystyczny, https://bdl.stat.gov.pl/BDL/start) througout API (https://api.stat.gov.pl/Home/BdlApi?lang=en) using functions presented in "/retrieving_functions" directory. For each variable separate receiving function (in separate file) has been prepared,
 - each receiving function execution conducts following steps:
  - connect to GUS BDL database throughout API, 
  - retrieve data according given criteria for catalogue of units definied in "units.rds" file with codes,
  - check with function ```func_handle_missing``` if received data are complete in regard of of definied' units catalogue, and print message with check' result, including names of missing units (if any), 
  - if missing units are detected, the aproppiate action is to be undertaken on "Transform" stage,
  - save received data into apropiate file into "/Data/[filename].rds".

### Transform

The process is developed automatically and if necessary, manually. 

 - if missing units have been detected on previous stage, the appropiate .rds file(s) with automatically retrieved data is/are to be supplemented to avoid missing values:
   - the missing data are to be retrieved manually from Statistics Poland webportal and writen into file: "/Data/app.xlsx",
   - in the next step, automatically retrieved data have to be merged with manually retrieved supplementary data. 
   - when merged, complete dataset is written  to .rds in "/Data/..." directory, ("create_variables")
 - after merge, the separate dataframes - each for variable - are created ("create_variables"),
 - integration of separated dataframes into one dataframe: "variables_dataframe" ("create_variables_dataframe")  


### Load

 - Load dataframe as table into 'life' database (postgres).  

### Derivative actions: aggregations

- generating aggregations ("create_aggregations") of variables from "variables dataframe" for future use.

### Derivative actions: generating plots 

- feeding the graphical presentations with values from created variables_dataframe using functions presented in dir: /plotting_functions.

## Usage

Extract:

```{r}
#/retrieving_functions/forests.R
```

Transform:

```{r}
#transforming_functions/create_variables.R
#transforming_functions/create_variables_dataframe.R
#transforming_functions/create_aggregations.R

```

Load:

```{r}
#loading_functions/loading_postgres.R
```

Derivative actions:

```{r}
#/plotting_functions
```


## Data source used for ETL feeding 

Główny Urząd Statystyczny (https://bdl.stat.gov.pl/BDL/start)


## License

[MIT](https://choosealicense.com/licenses/mit/)

