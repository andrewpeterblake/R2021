library(tidyverse)

fls <- list.files("trendz","^tas.*csv$")
DD  <- list()
for (i in 1:length(fls)) {
  print(i)
  DD[[i]] <- read_csv(paste0("trendz/", fls[i]), col_types="diccc") %>% 
    rename(Temperature = `Temperature - (Celsius)`) %>% 
    rename_with( ~ "Month", starts_with("Statistics")) %>% 
    mutate(Month=gsub(" Average","",Month), Month=factor(Month,levels=month.abb)) 
}

DD <- bind_rows(DD)

saveRDS(DD, file="WeatherData.RDS")
