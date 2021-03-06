# ETL for Life 

ETL tool for Life Pilica Plus Catchment Project.  

## Motivation

The tool has been prepared to automatize the processes of retrieval of statistical data from Statistics Poland (Główny Urząd Statystyczny, GUS; https://stat.gov.pl/) throughout GUS' API (https://api.stat.gov.pl/Home/BdlApi?lang=en) for 89 Polish localities  (*gmina*, NUTS6 statistical units), to be utilised for socio-economic analyses in the project "Life Pilica + " (IP LIFE PL Pilica Basin CTRL: Implementation of River Basin Management Plan in the Vistula basin on the example of Pilica river catchment Project of the LIFE programme (the EU's funding instrument)).

In addition, the tool enables simple transformation of retrieved data into one dataframe, convenient for further analyses. 
  
## Usage 

### Data retrieving  

Process of data retrieval from Statistics Poland (Bank Danych Lokalnych) database starts with execution of ```retrieve_gus ("topic", save)``` function.

The data are retrieved in yearly intervals. Time range of retrieved data differs among variables, with the lower end on 1995 and higher end on 2021.   

The catalogue of ```topics``` includes 27 following variables: 

 - forests - percentage of forests on statistical unit' area; yearly data for period: 2002 - 2019
 - inhabitants - number of inhabitants; yearly data for period: 1995 - 2020
 - companies_Ikw_0_ 9- number of entities of national economy with 0-9 employees; yearly data for period: 2012 - 2021 as for first quarter each year (Q1)
 - companies_Ikw_10_49 - number of entities of national economy with 10-49 employees; yearly data for period: 2012 - 2021 as for first quarter each year (Q1)
- companies_Ikw_50_249 - number of entities of national economy with 50-249 employees; yearly data for period: 2012 - 2021 as for first quarter each year (Q1)
- companies_Ikw_250_999 -  number of entities of national economy with 250-999 employees; yearly data for period: 2012 - 2021 as for first quarter each year (Q1)
- companies_Ikw_1000 -  number of entities of national economy with 1000 employees and over; yearly data for period: 2012 - 2021 as for first quarter each year (Q1)
- companies_Ikw_total - total number of entities of national economy; yearly data for period: 2012 - 2021 as for first quarter each year (Q1)
- farms_mineral_2010 - usage of pure supplements - minerals - usage in dt (100 kg); data for 2010, according to Powszechny Spis Rolny 2010 (General Agricultural Census 2010)
- farms_wapn_2010 - usage of pure supplements - calcium - usage in dt (100 kg); data for 2010, according to Powszechny Spis Rolny 2010 (General Agricultural Census 2010)
- farms_potas_2010 - usage of pure supplements - minerals - potasium in dt (100 kg); data for 2010, according to Powszechny Spis Rolny 2010 (General Agricultural Census 2010)
- farms_azot_2010 - usage of pure supplements - nitrogen - usage in dt (100 kg); data for 2010, according to Powszechny Spis Rolny 2010 (General Agricultural Census 2010)
- farms_fosfor_2010 - usage of pure supplements - phosphates - usage in dt (100 kg); data for 2010, according to Powszechny Spis Rolny 2010 (General Agricultural Census 2010)
- treatment_plants348- number of water biological treatments; yearly data for period: 1995 - 2020
- treatment_plants349 - number of water treatments with increased purification of nutrients; yearly data for period: 1995 - 2020
- waterworks - percentage of total population connected to waterworks, yearly data for period: 2002 - 2019
- sewers- percentage of total population connected to waterworks, yearly data for period: 2002 - 2019
- farms_cattle_cows_2010 - number of farms breeding cows; data for 2010, according to Powszechny Spis Rolny 2010 (General Agricultural Census 2010)
- farms_cattle_pigs_2010 - number of farms breeding pigs; data for 2010, according to Powszechny Spis Rolny 2010 (General Agricultural Census 2010)
- farms_cattle_chick_2010 - number of farms breeding chicks; data for 2010, according to Powszechny Spis Rolny 2010 (General Agricultural Census 2010)
- farms_cattle_hors_2010 - number of farms breeding horses; data for 2010, according to Powszechny Spis Rolny 2010 (General Agricultural Census 2010)
- farms_total_ha - number of farms; data for 2010, according to Powszechny Spis Rolny 2010 (General Agricultural Census 2010)
- farms_to_1ha - number of farms with area up to 1ha; data for 2010, according to Powszechny Spis Rolny 2010 (General Agricultural Census 2010)
- farms_1_5_ha - number of farms with area 1ha to 5ha; data for 2010, according to Powszechny Spis Rolny 2010 (General Agricultural Census 2010)
- farms_5_10_ha - number of farms with area 5ha to 10ha; data for 2010, according to Powszechny Spis Rolny 2010 (General Agricultural Census 2010)
- farms_10_15_ha - number of farms with area 10ha to 15ha; data for 2010, according to Powszechny Spis Rolny 2010 (General Agricultural Census 2010)
- farms_over_1ha - number of farms with area over 1ha; data for 2010, according to Powszechny Spis Rolny 2010 (General Agricultural Census 2010).

During function execution data are retrieved for 89 *gminas*, with following steps:

  - client connection to GUS BDL (Główny Urząd Statystyczny, https://bdl.stat.gov.pl/BDL/start) database throughout API enabled by Statistics Poland (https://api.stat.gov.pl/Home/BdlApi?lang=en), 
  - retrieve data for selected "topic" for 89 statistical units,  
  - internal test if data has been retrieved for all 89 *gminas* 
  - prints test result (message) with names of *gminas* for which data has not been retrieved (if any),
  - write retrieved data into ```"topic".rds``` file in customised directory ('retrieveddata'), created automatically in working directory when ```save``` parameter was set to ```TRUE``` (default setting). 
  
```Topic.rds``` file includes following variables: 

 - ```id``` - unique id number of *gmina* (according to GUS BDL, NUTS6; local statistical units),
 - ```name``` - name of *gmina* (NUTS6; local statistical units),
 - ```year``` - year of observation,
 - ```[topic]``` - retrieved values for selected "topic".  
  
If data for some *gminas* has not been retrieved, it is recommended to repeat function execution after 15 minutes, as the GUS API limits requests number for specific period of time (15 minutes). In case if the limit is exhausted, the data for some *gminas* are not retrieved. 

### Data transformation  

Process of data transformation is developed with ```transform_gus (save)``` function.

Prerequisite for the function usage, is at least one datafile with retrieved data stored in the ```'retrievedata'``` directory. Otherwise, the warning message is displayed. 

Execution of ```transform_gus (save)``` function: 

- creates one dataframe (```transformedata.rds```) from all dataframes stored in the ```'retrievedata'``` directory and stores the new dataframe in newly created ```'transformedata'``` directory.  

```Transformeddata.rds``` file includes following variables: 

 - ```id``` - unique id number of *gmina* (according to GUS BDL, NUTS6; local statistical units),
 - ```name``` - name of *gmina* (NUTS6; local statistical units),
 - ```year``` - year of observation,
 - ```[topic#1]``` - retrieved values for first "topic", for which data has been previously retrieved,    
 - ```[topic#n]``` - retrieved values for n "topic", for which data has been previously retrieved.

## Data source used for datatables feeding 

Główny Urząd Statystyczny, https://bdl.stat.gov.pl/BDL/start

GUS API documentation: Główny Urząd Statystyczny, https://api.stat.gov.pl/Home/BdlApi?lang=en

## License

[GPL-2.0](https://choosealicense.com/licenses/gpl-2.0/)

