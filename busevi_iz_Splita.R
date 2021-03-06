# setup #
library(lubridate)
library(dplyr)

# read & wrangle #
buss <- readxl::read_excel("Desktop/busevi.xlsx")
names(buss) <- c("polazak", "dolazak", "peron", "prijevoznik")


correct_trajanje <- function(x.df) {
  
  x.df.trajanje <- mutate_at(x.df, 
                             vars(polazak, dolazak),
                             as.POSIXct, format="%H:%M") %>% 
    mutate(trajanje.put = dolazak - polazak,
           minus = !(trajanje.put > 0))
  
  x.df.trajanje$dolazak[x.df.trajanje$minus] <- x.df.trajanje$dolazak[x.df.trajanje$minus] + lubridate::ddays(1)
  
  mutate(x.df.trajanje, trajanje.put = dolazak - polazak) %>% 
    select(-minus) %>% 
    select(polazak, trajanje.put, everything())
  
}

correct_trajanje(buss) %>% 
  mutate(trajanje.put = round(trajanje.put, 1)) %>% 
  # mutate(trajanje.put.minute = trajanje.put / dminutes()) %>% 
  # select(trajanje.put.minute, everything()) %>% 
  arrange(trajanje.put) %>% 
  View()
