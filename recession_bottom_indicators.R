# Use of the below government statistics to determine when Sheffield housing market reaches a new equilibrium point
# unemployment stops decreasing
# house prices stop decreasing
# UK GDP stops falling
# CPI stabilizes 

# load libraries    
library(tidyverse)
library(rvest)
library(RSelenium)
library(V8)
library(htmlunit)
library(cronR)
library(emayili)
library(magrittr)
library(lubridate)
library(gridExtra)
library(ggthemes)

# load data tibble
load(file = "~/projects/bottom/data.RData")

# define urls as objects
  #employment rate - quarterly - percent in employment 16-64 - Mar-May latest on 21.07.20
url1 <- "https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/timeseries/lf24/lms"
  #GDP - quarterly - millions £ - April on 21.07.20
url2 <- "https://www.ons.gov.uk/economy/grossdomesticproductgdp/timeseries/abmi/qna"
  #consumer price index - monthly - percentage change - ALL ITEMS - June on 21.07.20
url3 <- "https://www.ons.gov.uk/economy/inflationandpriceindices/timeseries/l55o/mm23"
  #sheffield annual mean house prices - thousands £ - inclues 2 month lag - includes May 2020 on 21.07.20
url4 <- "https://www.plumplot.co.uk/Sheffield-house-prices.html"
## it might be better to use the following for house prices? https://www.ons.gov.uk/economy/inflationandpriceindices/bulletins/housepriceindex/march2020

# read urls as html objects
h1 <- read_html(url1)
h2 <- read_html(url2)
h3 <- read_html(url3)
h4 <- read_html(url4)

# extract nodes from html objects 
p1 <- h1 %>% html_nodes("table") %>% html_text()
p2 <- h2 %>% html_nodes("table") %>% html_text()
p3 <- h3 %>% html_nodes("table") %>% html_text()
p4 <- h4 %>% html_nodes(".nogap") %>% html_text()

# wrangle html text strings into numeric objects
p1.1 <- p1 %>% str_replace_all("\n\t\t\t\t\t\t\t\t\t\t", "") %>% str_replace_all("\n\t\t\t\t\t\t\t\t\t", "")
employment <- as.numeric(str_extract(p1.1[2],"\\d\\d.\\d$"))
p2.1 <- p2 %>% str_replace_all("\n\t\t\t\t\t\t\t\t\t\t", "") %>% str_replace_all("\n\t\t\t\t\t\t\t\t\t", "") %>% str_replace_all("Q\\d", "")
GDP <- as.numeric(str_extract(p2.1[2],"\\d*$"))
p3.1 <- p3 %>% str_replace_all("\n\t\t\t\t\t\t\t\t\t\t", "") %>% str_replace_all("\n\t\t\t\t\t\t\t\t\t", "")
CPI <- ifelse(str_detect(p3.1[2], "-\\d{1,2}.\\d$"), as.numeric(str_extract(p3.1[2],"-\\d{1,2}.\\d$")), as.numeric(str_extract(p3.1[2],"\\d{1,2}.\\d$")))
ShefMeanHouse <- parse_number(p4[1])
EngWalesMeanHouse <- parse_number(p4[2])

# add a row with the scraped data to data file
data <- add_row(data,"%_in_employment" = employment, "GDP" = GDP, "%_CPI_changes" = CPI, "Mean_Sheffield_House_Prices" = ShefMeanHouse, "Mean_Eng/Wal_House_Prices" = EngWalesMeanHouse,"scape_date" = today())

# association check - Sheffield House Prices with the other variables
data %>% summarize(cor(data$Mean_Sheffield_House_Prices, data$GDP), cor(data$Mean_Sheffield_House_Prices, data$`%_in_employment`), cor(data$Mean_Sheffield_House_Prices,data$`%_CPI_changes`), cor(data$Mean_Sheffield_House_Prices, data$`Mean_Eng/Wal_House_Prices`))

# data plots with trendlines
g1 <- data %>% ggplot(aes(x=`scape_date`, y=`Mean_Sheffield_House_Prices`)) +
       geom_point() + geom_smooth(method = "lm", se = FALSE) +
       scale_x_date() + theme_economist() + xlab("Scrape Date of Annual Mean Sheff House Price") + ylab("£ in thousands")
g2 <- data %>% ggplot(aes(x=`scape_date`, y=`Mean_Eng/Wal_House_Prices`)) +
       geom_point() + geom_smooth(method = "lm", se = FALSE) +
       scale_x_date() + theme_economist() + xlab("Scrape Date of Annual Mean Eng/Wales House Price") + ylab("£ in thousands")
g3 <- data %>% ggplot(aes(x=`scape_date`, y=`%_in_employment`)) +
       geom_point() + geom_smooth(method = "lm", se = FALSE) +
       scale_x_date() + theme_economist() + xlab("Scrape Date of Quarterly UK % in Employment") + ylab("Percent")
g4 <- data %>% ggplot(aes(x=`scape_date`, y=`%_CPI_changes`)) +
       geom_point() + geom_smooth(method = "lm", se = FALSE) +
       scale_x_date() + theme_economist() + xlab("CPI price changes monthly - inflation/deflation") + ylab("Percent change in CPI")
g5 <- data %>% ggplot(aes(x=`scape_date`, y=GDP)) +
       geom_point() + geom_smooth(method = "lm", se = FALSE) +
       scale_x_date() + theme_economist() + xlab("Scrape Date of Quarterly GDP") + ylab("£ in millions")
g6 <- grid.arrange(g1, g2, ncol = 2)
g7 <- grid.arrange(g3, g4, g5, nrow = 3)

# save both plots
ggsave("~/projects/bottom/House_prices.png", plot = g6)
ggsave("~/projects/bottom/GDP_CPI_employment.png", plot = g7)

# set up of email alert
email_eric <- envelope()
email_eric <- email_eric %>% from("oedipusatcolonussheffield@gmail.com") %>% to("eric210bohun@gmail.com")
email_eric <- email_eric %>% subject("Economic indicators")
email_eric <- email_eric %>% text("Check out what's happening to the economy - the R log will give a correleation measure between house prices and the indicators")
email_eric <- email_eric %>% attachment("~/projects/bottom/GDP_CPI_employment.png")
email_eric <- email_eric %>% attachment("~/projects/bottom/House_prices.png")
smtp <- server(host = "smtp.gmail.com",
               port = 465,
               username = "oedipusatcolonussheffield@gmail.com",
               password = "XXXXXXXXXX")
smtp(email_eric, verbose = TRUE)

# save newly acquired data in the data object
save(data, file = "~/projects/bottom/data.RData")
