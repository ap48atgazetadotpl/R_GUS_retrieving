# ETL for Life 

ETL tool for *LIFE PL Pilica Basin CTRL* Project.  

## Motivation 

One of the tasks included in the project "Implementation of River Basin Management Plan in the Vistula basin on the example of Pilica river catchment" (co-financed by EU's fund LIFE programme) is the monitoring of socio-economic situation in 93 Polish gmina's (NUTS 6 statistical units), being a project' important stakeholders.
The monitoring processes are devoted to periodical analysis of selected indicators, presented by Statistics Poland (SP, Główny Urząd Statystyczny, GUS) on official webportal (Bank Danych Lokalnych).
As the manual retrieving of statistical data for 93 statistical units directly from Statistics Poland webportal is a time-consuming and process, the opportunity of use of SP's API (https://api.stat.gov.pl/Home/BdlApi?lang=en) has been taken, to facilitate the retrieving process and to make it repeatable.  
In result an R-based tool has been prepared, allowing automatically retrieve, transform and store data for 10 (out of 12) indicators, for future analyses. 

## Components

### Extract (```retrieving_functions```)

The process of data retrieving runs automatically, with following steps:

 - receiving selected data from Statistics Poland (Główny Urząd Statystyczny, https://bdl.stat.gov.pl/BDL/start) througout API (https://api.stat.gov.pl/Home/BdlApi?lang=en) using functions presented in "/retrieving_functions" directory. For each variable separate receiving function (in separate file) has been prepared,
 - each receiving function execution conducts following steps:
  - connect client to GUS BDL database server throughout API, 
  - retrieve data according given criteria for catalogue of statistical units definied in "units.rds" file with codes,
  - check with function ```func_handle_missing``` if received data are complete in regard of of definied' statistical units catalogue, and print message with check' result, including names of missing units (if any), 
  - if missing units are detected, the aproppiate action is to be undertaken (see: "Transform" stage),
  - save received data into apropiate file into ```/Data/[variable_name].rds```.

### Transform (```transforming_functions```)

The process is developed automatically and if necessary, manually. 

 - if missing units have been detected on previous stage i.e. data for some statistical units have been not retrieved, the appropiate .rds file(s) with automatically retrieved data is/are to be supplemented to avoid missing values:
   - the missing data are to be retrieved manually from Statistics Poland webportal and writen into file: "/Data/app.xlsx",
   - in the next step, automatically retrieved data have to be merged with manually retrieved supplementary data,
   - when merged, complete dataset is written  to ```/Data/[variable_name].rds``` with function ```create_variables```.
 - after merge, the appropiate dataframes - each for one variable - are created with function ```create_variables```,
 - last step is the integration of separated dataframes into one dataframe: ```variables_dataframe``` with function ```create_variables_dataframe```. 

### Load (```loading_functions```)

 - Load ```variables_dataframe``` as table into 'life' database (postgres).  

### Derivative actions: aggregations

- generating aggregations ```create_aggregations``` of variables from ```variables_dataframe``` for future use.

### Derivative actions: generating plots 

- feeding the graphical presentations with data from ```variables_dataframe``` using functions presented in dir: /plotting_functions.

### Derivative actions: bookdown report

- data included in ```variables_dataframe``` as well as generated aggregations and plots serves as the basic for *Report* to be find as separate repository.

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

