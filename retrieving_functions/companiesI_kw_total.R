library(rjson)
library(RCurl)

codes <- list(51011506022,
              51011506032,
              51011506073,
              51011506082,
              51011506113,
              51011707012,
              51011707023,
              51011707032,
              51011707043,
              51011707052,
              51011707062,
              51011707072,
              51011707082,
              51011710012,
              51011710022,
              51011710032,
              51011710042,
              51011710052,
              51011710062,
              51011710072,
              51011710082,
              51011710093,
              51011710102,
            #  51011710113,
              51011712062,
              51011712072,
              51011712102,
              51011712113,
              51011712132,
              51011712142,
              51011913062,
              51011716011,
              51011716022,
              51011716032,
              51011716042,
              51011716052,
              51011716062,
              51011716072,
              51011716082,
              51011716092,
              51011716102,
              51011762011,
              11212008012,
              11212008032,
              11212312073,
              71422701013,
              71422701022,
              71422701032,
              71422701052,
              71427306012,
              71427306022,
              71427306042,
              71427306073,
              71427306083,
              71427306113,
              71422707022,
              71422707042,
              71422707062,
              71422723022,
              71422723032,
              71422723042,
              71422723063,
              71422723072,
              12414604063,
              12414604092,
              12414609032,
              12415016021,
              12415016032,
              12415016042,
              12415016063,
              12415016073,
              12415016083,
              12415016092,
              12415016102,
              52615302072,
              52615204082,
              52615204112,
              52615204192,
              52615205012,
              52615205022,
              52615205033,
             # 52615205043,
              52615205052,
              52615205062,
              52615205072,
              52615205083,
              52615210022,
              52615313012,
              52615313022,
              52615313032,
              52615313052,
              52615313063,
              71422701063
             )

urls <- list()

#79130 - kanalizacja
#79133 - wodociąg 
#199206 - ludnosc 
#274234 - farmy ogolem ha 
#274238 - do 1 ha
#274242 - od 1 ha 
#274246 - 1 - 5 ha 
#274250  5 - 10 ha
#274254 10 - 15 ha 
#274103 - cattle total
#274107 - cows
#274111 - trzoda chlewna
#274119 - horses
#274123 - drób
#P2613 companies
#https://bdl.stat.gov.pl/api/v1/data/by-variable/74106?format=json&page-size=30&unit-parent-id=052615205052&unit-level=6
#262070 - I kw ogolem
#262072 - I kw 0-9
#262077 - I kw 10 -49
#262079 - I kw 50 - 249 
#262078 - I kw 250 - 999
#262080 - I kw ponad 1000
#74106  - II kw ogolem
#74104  - II kw 0-9
#74102  - II kw 10 -49
#74100  - II kw 50 - 249 
#74098  - II kw 250 - 999
#74096  - II kw ponad 1000
#262073   - III kw ogolem
#262076   - III kw 0-9
#262071   - III kw 10 -49
#262075   - III kw 50 - 249 
#262074   - III kw 250 - 999
#262081   - III kw ponad 1000
#74107    - IV kw ogolem
#74105    - IV kw  0-9
#74103    - IV kw 10 -49
#74101    - IV kw 50 - 249 
#74099    - IV kw 250 - 999
#74097    - IV kw ponad 1000

links <<-  list()


for (k in 1:91) {urls <- paste0("https://bdl.stat.gov.pl/api/v1/data/by-variable/262070?format=json&page-size=30&unit-parent-id=0", codes[[k]], "&unit-level=6")
                links <<- rbind(links, urls)
}

df_total_result2 <- data.frame()


retrieve <- function(link) 

  {
  
  
  for (m in 1:91) 
    
      {
    
  link <- URLencode(links[[m]])
  sampledatagus2 <- fromJSON(getURL(links[[m]]))
  glimpse(sampledatagus2)
  
  
  id <- sampledatagus2$results[[1]]$id
  name <- sampledatagus2$results[[1]]$name
  
  for (j in 1:10){
 
    year <- sampledatagus2$results[[1]]$values[[j]]$year
    val <- sampledatagus2$results[[1]]$values[[j]]$val
  
  
  result2 <- data.frame(id, name, year, val)
  dfresult2 <- data.frame(result2)
  df_total_result2 <<- rbind(df_total_result2,dfresult2)
  

  
  }  

}

}

retrieve(links)

df_total_result2$name <- as.character(df_total_result2$name)
df_total_result2 <-  df_total_result2 %>%
  mutate(name=replace(name, id=="051011716011", "Tomaszów Mazowiecki wiejski"))
df_total_result2 <-  df_total_result2 %>%
  mutate(name=replace(name, id=="051011716092", "Tomaszów Mazowiecki miasto"))  
df_total_result2$name <- as.factor(df_total_result2$name)

write_rds(df_total_result2, "Data/companies_Ikw_total.rds")


#filter(df_total_result2, df_total_result2$id == "051011506022")

