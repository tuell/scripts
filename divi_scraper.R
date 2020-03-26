#------------------------------------------#
#       Load required packages             #
#------------------------------------------#

library(tidyverse)
library(rvest)


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

    # street: first cell in row, second-to-last "small"-tag
    street = html_node(row, "td:nth-child(1)") %>%
        html_node("small:nth-last-of-type(2)") %>%
        html_text() %>%
        str_trim()

    # location (plz and placename): first cell in row, last "small"-tag
    plz = html_node(row, "td:nth-child(1)") %>%
        html_node("small:last-of-type") %>%
        html_text() %>%
        str_trim() %>%
        str_extract("\\d{4,5}")
    
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
                        street = street,
                        plz = plz,
                        location = loc,
                        date = date,
                        time = time,
                        icu_low = icu_low,
                        icu_high = icu_high,
                        ecmo = ecmo)
    
    return(return_tbl)
}



#------------------------------------------#
#         apply scraping function          #
#------------------------------------------#

# use lapply to create list of tibbles for each url, then bind together in one big tbl
full_tbl <- lapply(urls, scrape_page) %>% bind_rows

#------------------------------------------#
#            save as csv file              #
#------------------------------------------#

write.csv(full_tbl, format(Sys.time(), "divi_%Y%m%d_%H%M.csv"), fileEncoding = "UTF-8", row.names = F)
