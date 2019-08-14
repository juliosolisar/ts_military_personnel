# Military personnel around the World
Time series graphs with dygraphs on military personnel (data from World Bank)
```r
library(dplyr)
library(tidyr)
library(readxl)
library(dygraphs)
library(lubridate)
library(xts)


regions <- read_excel("/Users/juliosolisarce/Downloads/CLASS.xls", skip=4) 
regions <- regions %>%
    select(Region, Code) %>%
    rename("Country Code" = Code)

army <- read_excel("/Users/juliosolisarce/Downloads/military_personnel.xlsx", skip=4)
army_panel <- army %>%
    select_if(grepl("\\d", names(army)) | grepl("Country", names(army))) %>%
    #this let's me call all the var with year as name as well as all the variables with "Country" in its name
    left_join(regions, "Country Code") %>%
    gather(year, total_personnel, 3:61) %>%
    mutate(code=as.factor(`Country Code`)) %>%
    group_by(code) %>%
    mutate(difference=(total_personnel-lag(total_personnel))/1000) %>%
    ungroup() %>%
    select(`Country Name`, year, difference, Region) %>%
    mutate(date = as.Date(year, "%Y")) %>%
    filter(Region!="NA") 

#looping through regions
region_names <- as.list(unique(army_panel$Region))
lapply(region_names, function(region_name){
    region_name <- army_panel %>%
        filter(Region==region_name) %>%
        spread(`Country Name`, difference) %>%
        select(-year, -Region)
    don=xts(x=region_name[,-1], order.by=region_name$date)
    graph <- dygraph(don, main = "Change in military personnel (1960-2018)- World Bank") %>%
        dyAxis("y", label = "Military personnel- difference (t - t-1) / 1000", valueRange = c(-1500, 5000)) %>%
        dyAxis("x", label = "Year", valueRange = c(1960, 2020))
    graph
    })

```
