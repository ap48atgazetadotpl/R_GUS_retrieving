# ETL Life 

ETL tool for Life.  

## Components

Extract:

 - automatically receiving selected data from Statistics Poland (Główny Urząd Statystyczny, https://bdl.stat.gov.pl/BDL/start) througout API (https://api.stat.gov.pl/Home/BdlApi?lang=en) using functions presented in dir: /retrieving_functions,
 - each receiving function conduct steps as:
  - connect to GUS BDL database throughout API, 
  - retrieve data according given criteria,
  - save received data into approppiate .rds file into /Data,
  - check if received and saved data are complete in regard of statistical units, 
  - if missing units are manually detected, the approppiate action is undertaken on "Transform" stage.

Transform:

 - if missing units have been detected on previous stage, the appropiate .rds file(s) are supplemented manually to avoid missing values using data retrieved manually and stored in file: /Data/app.xlsx,
 - writing complete data to "R" .rds format into appropiate files dir: /Data.
 - transforming data into variables using function /global/variables_global.R

Load:

- integration of variables into one dataframe ("variables_dataframe") using function: /global/variables_global.R
- feeding the graphical presentations with values from created dataframe using functions presented in dir: /plotting_functions.


## Usage

Extract:

```{r}
#/retrieving_functions
```

Transform:

```{r}
#global/variables_global.R
```

Load:

```{r}
#global/variables_global.R
```


## Data source used for ETL feeding 

Główny Urząd Statystyczny, https://bdl.stat.gov.pl/BDL/start)


## License

[MIT](https://choosealicense.com/licenses/mit/)

