
########################################################################################################simple unique table
##for neat html data
require(XML)
library(RCurl)
url_TRENITALIA <- getURL("https://www.trenitalia.com/it/treni_regionali/piemonte/agenzie_di_viaggiopiemonte.html")
AGENCY_tables <- readHTMLTable(url_TRENITALIA, which = 1, header = TRUE, stringsAsFactors = FALSE)
head(AGENCY_tables)

#####
##https://www.trenitalia.com/it/treni_regionali/abruzzo/agenzie_di_viaggioabruzzo.html
##https://www.trenitalia.com/it/treni_regionali/alto_adige/agenzie_alto_adige.html
##https://www.trenitalia.com/it/treni_regionali/basilicata/agenzie_di_viaggiobasilicata.html
##https://www.trenitalia.com/it/treni_regionali/calabria/agenzie_di_viaggiocalabria.html
##https://www.trenitalia.com/it/treni_regionali/campania/agenzie_di_viaggiocampania.html
##https://www.trenitalia.com/it/treni_regionali/emilia_romagna/agenzie_di_viaggioemiliaromagna.html
##https://www.trenitalia.com/it/treni_regionali/friuli_venezia_giulia/agenzie_friuli_veneziagiulia.html
##https://www.trenitalia.com/it/treni_regionali/lazio/agenzie_lazio.html
##https://www.trenitalia.com/it/treni_regionali/liguria/agenzie_di_viaggioliguria.html
##https://www.trenitalia.com/it/treni_regionali/lombardia/agenzie_viaggio_lombardia.html
##https://www.trenitalia.com/it/treni_regionali/marche/agenzie_di_viaggiomarche.html
##https://www.trenitalia.com/it/treni_regionali/molise/agenzie_di_viaggiomolise.html
##https://www.trenitalia.com/it/treni_regionali/piemonte/agenzie_di_viaggiopiemonte.html
##https://www.trenitalia.com/it/treni_regionali/puglia/agenzie_puglia.html
##https://www.trenitalia.com/it/treni_regionali/sardegna/agenzie_di_viaggiosardegna.html
##https://www.trenitalia.com/it/treni_regionali/sicilia/agenzie_di_viaggiosicilia.html
##https://www.trenitalia.com/it/treni_regionali/toscana/agenzie_toscana.html
##https://www.trenitalia.com/it/treni_regionali/trentino/agenzie_trentino.html
##https://www.trenitalia.com/it/treni_regionali/umbria/agenzie_di_viaggioumbria.html
##https://www.trenitalia.com/it/treni_regionali/valle_d_aosta/agenzie_di_viaggiovalledaosta.html
##https://www.trenitalia.com/it/treni_regionali/veneto/agenzie_di_viaggioveneto.html


###### URLs - excluding lombardia, where the number of columns do not mathc
url_app <- c("abruzzo/agenzie_di_viaggioabruzzo.html", "alto_adige/agenzie_alto_adige.html", "basilicata/agenzie_di_viaggiobasilicata.html", 
"calabria/agenzie_di_viaggiocalabria.html", "campania/agenzie_di_viaggiocampania.html", "emilia_romagna/agenzie_di_viaggioemiliaromagna.html", 
"friuli_venezia_giulia/agenzie_friuli_veneziagiulia.html", "lazio/agenzie_lazio.html", "liguria/agenzie_di_viaggioliguria.html", 
"marche/agenzie_di_viaggiomarche.html", "molise/agenzie_di_viaggiomolise.html", 
"piemonte/agenzie_di_viaggiopiemonte.html", "puglia/agenzie_puglia.html", "sardegna/agenzie_di_viaggiosardegna.html", 
"sicilia/agenzie_di_viaggiosicilia.html", "toscana/agenzie_toscana.html", "trentino/agenzie_trentino.html", 
"umbria/agenzie_di_viaggioumbria.html", "valle_d_aosta/agenzie_di_viaggiovalledaosta.html", "veneto/agenzie_di_viaggioveneto.html")

url_TRENIT <- paste0("https://www.trenitalia.com/it/treni_regionali/", url_app)
TRENITALIA <- getURL(url_TRENIT)
len <- length(TRENITALIA)
###### Reading data
tbl<-readHTMLTable(TRENITALIA[1])[[1]]
for (i in 2:len)
  {tbl<-rbind(tbl,readHTMLTable(TRENITALIA[i])[[1]])}

#####adding lombardia
lombardia_url<- "https://www.trenitalia.com/it/treni_regionali/lombardia/agenzie_viaggio_lombardia.html"
lombardia_url2<- getURL(lombardia_url)
lombardia_tb<- readHTMLTable(lombardia_url2, which = 1)
lombardia_tb[c("V5", "V6")]<-NA

tbl_final <- rbind(tbl, lombardia_tb)

#####adding France, with different columns
https://www.trenitalia.com/it/informazioni/punti_vendita_allestero/francia.html


###### Formatting data
colnames(tbl_final)<-c("Location","Name","Address","Telephone","Opening","Time")
summary(tbl_final)


##################export to excel
require(openxlsx)
write.xlsx(tbl_final, "Trenitalia agencies.xlsx")


####################################################################################################import csv will all data
dt<- read.csv("C:/Users/angeles.jimenez/Documents/R/all_accounts.csv", header = TRUE, sep = ",")
account_pattern<- read.csv("C:/Users/angeles.jimenez/Documents/R/account_pattern.csv", header = TRUE, sep = ",")

colnames(account_pattern) <- c("Central_account_number", "Name")
head(dt)
head(tbl_final)
head(account_pattern)
####################################################################################################fuzzy matching
#The Levenshtein distance between two strings is the number of single character deletions, insertions, 
#or substitutions required to transform one string into the other. This is also known as the edit distance
#https://stackoverflow.com/questions/26405895/how-can-i-match-fuzzy-match-strings-from-two-datasets/43617594#43617594

install.packages("fuzzyjoin")
library(fuzzyjoin)
library(dplyr)
a <- as.character(dt[,2])
b <- as.character(tbl_final[,2])
c <- as.character(account_pattern[,2])

##################################this one takes too much memory ad shows error, should work for smaller tasks
stringdist_join(account_pattern, tbl_final, 
                by = "Name", 
                mode = "left", 
                ignore_case = TRUE, 
                method = "jw", 
                max_dist = 99, 
                distance_col = "dist") %>% 
  group_by(Name.x) %>% 
  top_n(1, -dist)


##################################this one takes a lot of memory too, but works
equal: lapply(c, agrep, b, max.distance=0.01)



#-example
y<-character()
y<-c("UVET VIAGGI TURISMO", "ITALY COPR IMPLANTS", "WELCOME TRAVEL GROUP SPA", "BCD TRAVEL ITALY", "CISALPINA TOURS SPA", "EASY MARKET","BLUETEAM TRAVEL NETWORK SRL", "COSTA CROCIERE", "FESPIT SRL", "GATTINONI TRAVEL NETWORK / BTC", "ROBINTUR SPA", "ALPITOUR SPA", "GLAMOUR SRL")
colnames(y) <- c("Name")
x<-c("UVET VIAGGI ", "ITALY COPR IMPLANT", "WElcom TRAVEL GROUP ", "BCD TRAVEL ", "CISALPINA TOURS ", "whatever", "eso", "EASY MARKET","BLUETEAM TRAVEL netw", "COSTA Crociere", "Goli", "ROBINTUR ", "ALPITOUR ", "GLAMOUR ")
colnames(y) <- c("Name")
install.packages("fuzzyjoin")
library(fuzzyjoin)
library(dplyr)

stringdist_join(y, x, 
                mode = "left",
                ignore_case = TRUE, 
                method = "jw", 
                max_dist = 99, 
                distance_col = "dist") %>%
  group_by(Name.x) %>%
  top_n(1, -dist)

