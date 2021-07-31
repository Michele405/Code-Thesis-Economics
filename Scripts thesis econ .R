library(data.table)
library(dplyr)
it16h <- as.data.table(it16h)
it16h$region_c <- recode(it16h$region_c, "[3]ITC4 - Lombardia" = "North","[1]ITC1 - Piemonte" = "North","[2]ITC2 - Valle d'Aosta" = "North","[7]ITC3 - Liguria" = "North","[5]ITH3 - Veneto" = "North","[6]ITH4 - Friuli" = "North","[4]ITH1 and ITH2 - Trentino" = "North","[8]ITH5 - Emilia Romagna" = "North" , "[10]ITI2 - Umbria" = "South","[11]ITI3 - Marche" = "South","[12]ITI4 - Lazio" = "South","[13]ITF1 - Abruzzo" = "South","[14]ITF2 - Molise" = "South","[15]ITF3 - Campania" = "South","[16]ITF4 - Puglia" = "South","[17]ITF5 - Basilicata" = "South","[18]ITF6 - Calabria" = "South","[19]ITG1 - Sicilia" = "South","[20]ITG2 - Sardegna" = "South","[9]ITI1 - Toscana" = "South")

it16p %>% ggplot(it16p, mapping = aes(x=it16p$pitotal, colour = edmom_c)) + geom_step(stat = "ecdf") + scale_x_continuous(limits = c(0, 200000)) + theme(legend.position = "bottom") + xlab("Income")

ds$region_c <- recode(ds$region_c, "[13]Wien" = "Wien","[11]Burgenland" = "Others","[12]Niederösterreich" = "Others","[21]Kärnten" = "Others","[22]Steiermark" = "Others","[31]Oberösterreich" = "Others","[32]Salzburg" = "Others","[33]Tirol" = "Others","[34]Vorarlberg" = "Others") 

# Script to divide household consumption expenditure for number of people within each household
ds <- read.LIS('it16h')            
library(dineq)        
library(data.table)      
ds <- as.data.table(ds)   
library(dplyr)  
library(tidyverse)  
ds <- ds %>% mutate(hcexpd = hcexp / nhhmem)
ds1 <- read.LIS('it16p')
ds2 <- left_join(ds, ds1, by = "hid")
edmom_decomp <- mld_decomp(x=ds2$hcexpd,z=ds2$edmom_c, weights=ds$ppopwgt)  
edmom_decomp["mld_decomp"]

# Script for the regression-based decomposition method
ds <- read.LIS('it16p')            
library(dineq)        
library(data.table)      
ds <- as.data.table(ds)   
library(dplyr)  
library(tidyverse)  
inequality_decomp <- dineq_rb(pitotal~edmom_c+eddad_c+sex+immigr,weights="ppopwgt", data=ds)
inequality_decomp["decomposition_inequality"]




