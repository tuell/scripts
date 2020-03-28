#------------------------------------------#
#       Load required packages             #
#------------------------------------------#

require(tidyverse)
require(lubridate)
require(rvest)
require(googlesheets4) # install dev version via github, otherwise writing sheets not available

# for instant geocoding - not used currently
# require(tidygeocoder)

#------------------------------------------#
#       Connect to Google Sheet            #
#------------------------------------------#

# Authenticate via Googlesheets-Api-Key
sheets_deauth() # delete previous auth
sheets_auth(email="EMAIL",path="/path/to/key.json")

sheet_id <- "GOOGLE-SHEET-ID"


#------------------------------------------#
#    Get URLs to all pages in register     #
#------------------------------------------#

# starting page of register
firstpage <- "https://www.divi.de/register/intensivregister"

# find starting no. of last page
total_no <- read_html(firstpage) %>%
    html_node("a[title='Ende']") %>%
    html_attr("href") %>%
    str_replace(".*start\\=", "") %>%
    as.integer() 

# sequence in steps of 20 up to total no. (pages are 20 entries long by default)    
pages <- seq(20,total_no, 20)

# construct urls from sequence
urls <- pages %>%
    str_c("https://www.divi.de/register/intensivregister?start=", .)

# pop in first page at the top
urls <- append(c(firstpage), urls)

#------------------------------------------#
#      function to scrape each url         #
#------------------------------------------#

# helper functoin for time stamps 
ts <- stamp("2020-01-31 15:55")

# scraping function
scrape_page <- function(url){

    # extract table containing hospitals
    table = read_html(url) %>%
        html_node("#dataList")

    # extract single rows, remove row containing legend
    row = html_nodes(table, "tr")
    row = row[2:length(row)]

    # names: first cell in row, before linebreak
    name = html_node(row, "td:nth-child(1)") %>% 
        str_replace("\\<br.*", "") %>% 
        str_replace_all("\\<.*?\\>", "") %>% 
        str_trim()

    # states: third cell in row    
    state = html_node(row, "td:nth-child(3)") %>% html_text() %>% str_trim()
    
    # date and time: seventh cell in row, split up via regex
    date = html_node(row, "td:nth-child(7)") %>% html_text() %>% str_trim() %>% str_replace(regex("(20\\d\\d).*", dotall = T), "\\1")
    time = html_node(row, "td:nth-child(7)") %>% html_text() %>% str_trim() %>% str_extract("\\d\\d\\:\\d\\d")
    
    date_time = str_c(date, " ", time)
    
    timestamp = ts(dmy_hm(date_time))
    
    # street: first cell in row, second-to-last "small"-tag
    street = html_node(row, "td:nth-child(1)") %>%
        html_node("small:nth-last-of-type(2)") %>%
        html_text() %>%
        str_trim()

    loc = html_node(row, "td:nth-child(1)") %>%
        html_node("small:last-of-type") %>%
        html_text() %>%
        str_replace("\\d{4,5}", "") %>%
        str_trim()
    
    web = html_node(row, "td:nth-child(2)") %>%
        html_node("a") %>%
        html_attr("href")
    
    # status of beds is displayed via colored "span"-elements with specific class names

    # icu low care: 4th cell in row
    icu_low = html_node(row, "td:nth-child(4)") %>%
        html_node("span") %>%
        html_attr("class") %>%
        str_replace("hr-icon-", "") %>%
        str_replace_all(c("green" = "verfügbar", "yellow" = "begrenzt", "red" = "ausgelastet", "unavailable" = "nicht verfügbar"))
    
    # icu high care: 5th cell in row
    icu_high = html_node(row, "td:nth-child(5)") %>%
        html_node("span") %>%
        html_attr("class") %>%
        str_replace("hr-icon-", "") %>%
        str_replace_all(c("green" = "verfügbar", "yellow" = "begrenzt", "red" = "ausgelastet", "unavailable" = "nicht verfügbar"))
    
    # ecmo: 6th cell in row
    ecmo = html_node(row, "td:nth-child(6)") %>%
        html_node("span") %>%
        html_attr("class") %>%
        str_replace("hr-icon-", "") %>%
        str_replace_all(c("green" = "verfügbar", "yellow" = "begrenzt", "red" = "ausgelastet", "unavailable" = "nicht verfügbar"))
    

    # combine all results in one tibble    
    return_tbl = tibble(name = name,
                        web = web,
                        state = state,
                        adress = str_c(street, ", ", loc),
                        timestamp = timestamp,
                        icu_low = icu_low,
                        icu_high = icu_high,
                        ecmo = ecmo)
    
    return(return_tbl)
}



#------------------------------------------#
#         apply scraping function          #
#------------------------------------------#

# use lapply to create list of tibbles for each url, then bind together in one big tbl
new_tbl <- lapply(urls, scrape_page) %>% bind_rows


#------------------------------------------#
#       add geo location data              #
#------------------------------------------#

# load adress file
adresses <- read_sheet(sheet_id, sheet="adresses")

# select only lat and long for merging
join_adresses <- adresses %>%
    select(-adress)

new_tbl <- left_join(new_tbl, join_adresses, by = "name")

# possibly geocode missing adresses right here. 
# Not used currently because of trouble installing tidygeocoder package on server

# # merge result tibble with lat long into temporary tibble
# temp_tbl <- left_join(new_tbl, join_adresses, by = "name")
# 
# # in case of new hospitals in list:
# # find hospitals missing latlong and then geocode adress
# missing_adresses <- temp_tbl %>%
#     filter(is.na(lat)) %>%
#     select(name, adress) %>%
#     tidygeocoder::geocode(adress, method = "osm")
# 
# # if there were any missing adresses: append them to adress-list and overwrite csv file
# # then perform new join with complete adress tbl
# # else use temp join tbl as full tibble
# if(nrow(missing_adresses) > 0){
#     adresses <- bind_rows(adresses, missing_adresses) %>% distinct()
#     write_sheet(adresses, ss=sheet_id, sheet="adresses")
#     join_adresses <- adresses %>% select(-adress)
#     new_tbl <- left_join(full_tbl, join_adresses, by = "name")
# } else {
#     full_tbl <- temp_tbl
# }


#------------------------------------------#
#               save data                  #
#------------------------------------------#

# save locally to csv
write.csv(new_tbl, format(Sys.time(), "/home/till_hafermann_hr_de/rscripts/divi/divi_%Y%m%d_%H%M.csv"), fileEncoding = "UTF-8", row.names = F)


# write and append to google sheet

sheet_tbl <- new_tbl %>%
    mutate(scraped = ts(now(tzone = "CET")))


write_sheet(sheet_tbl, ss = sheet_id, sheet = "current")
sheets_append(sheet_tbl, ss=sheet_id, sheet = "archive")


