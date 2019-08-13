# Military personnel around the World
Time series graphs with dygraphs on military personnel (data from World Bank)
```r
library(dplyr)
library(tidyr)
library(readxl)
library(dygraphs)


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
    mutate(difference=total_personnel-lag(total_personnel))

```
