x<-10:1
y<--4:5
q<-c("hockey", "footbal", "baseball", "baseball", "curling", "rugby", "lacrosse", "basketball","basketball","hockey")
theDF <- data.frame(First=x, Second=y, Third= q, stringsAsFactors = FALSE)
"http://www.jaredlander.com/data/Tomato%20%First.csv"
tomato <- read.table(file="http://www.jaredlander.com/data/Tomato%20First.csv", header=TRUE, sep= ",")
head(tomato)
class(tomato$Tomato)
tomato <- read.table(file="http://www.jaredlander.com/data/Tomato%20First.csv", header=TRUE, sep= ",", stringsAsFactors = FALSE)
download.file("http://www.jaredlander.com/data/ExcelExample.xlsx", destfile = "ExcelExample.xlsx", mode = "wb")
require(readxl)
tomatoXL <- read_excel("ExcelExample.xlsx", sheet = 1)
head(tomatoXL)
library(openxlsx)
tomatoXL1 <- read.xlsx("ExcelExample.xlsx", sheet=1)
library(RODBC)
require(XML)
IATA <- readHTMLTable("http://www.seg-social.es/wps/portal/wss/internet/Trabajadores/PrestacionesPensionesTrabajadores/10963/28393/28396/28472", which=1, header= TRUE, StringsasFactors = FALSE)
IATA


###################################
require(plyr)
require(RCurl)menu <- "https://menupages.com/restaurants/ny-new-york"

doc<-htmlParse(menu)
html <- getURL(doc)
table1 </ 

  
  
######################search in a list of several pages

library(rvest)

wines <- lapply(paste0('http://www.winemag.com/?s=washington%20merlot&drink_type=wine&page=', 1:39),
                function(url){
                  url %>% read_html() %>% 
                    html_nodes(".review-listing .title") %>% 
                    html_text()
                })

#####################