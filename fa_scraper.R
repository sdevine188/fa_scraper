library(tidyverse)
library(rvest)
# library(keyboardSimulator)

# https://blog.rstudio.com/2014/11/24/rvest-easy-web-scraping-with-r/

# css selectors
# https://www.w3schools.com/cssref/css_selectors.asp

# setwd
setwd("C:/Users/Stephen/Desktop/R/fa_scraper")

# manually login and get current_issue_url by clicking "Current Issue" tab in header 
# (note the url will have "check_logged_in=1" annotation)
current_issue_url <- "https://www.foreignaffairs.com/issues/2022/101/4?check_logged_in=1"

# create get_article_text()
get_article_text <- function(current_issue_url) {
        
        # get current_issue_html
        current_issue_html <- read_html(x = "https://www.foreignaffairs.com/issues/2022/101/4?check_logged_in=1")
        
        # get article_links
        article_links <- current_issue_html %>% html_nodes(css = "[href]") %>% html_attr("href") %>% 
                tibble(url = .) %>%
                filter(str_detect(string = url, pattern = regex("^/articles", ignore_case = TRUE))) %>%
                mutate(url = str_c("https://www.foreignaffairs.com/", url),
                       article_number = row_number())
        
        # create get_article_text_for_current_url()
        get_article_text_for_current_url <- function(current_url, current_article_number) {
                
                print(current_url)
                
                # get current_article_html
                current_article_html <- read_html(x = current_url)
                
                # get current_article_text
                current_article_text <- current_article_html %>% 
                        html_nodes(css = "div [class = 'article-dropcap ls-0  f-serif'] p") %>% 
                        html_text() %>% 
                        reduce(.f = str_c) %>%
                        as_tibble() %>% 
                        rename(text = value)
                
                # get current_article_title/subtitle
                current_article_title <- current_article_html %>% 
                        html_nodes(css = "h1[class = 'f-serif ls-0 article-title pt-2']") %>% html_text()
                current_article_subtitle <- current_article_html %>% 
                        html_nodes(css = "h2[class = 'f-serif ls-0 article-subtitle ']") %>% html_text()

                # get current_article_author
                current_article_author <- current_article_html %>% 
                        html_nodes(css = "a[class = 'article-byline-author b4-l']") %>% html_text()
                
                # get header info to current_article_text
                current_article_text <- str_c("{{Pause=.3}} Foreign Affairs {{Pause=.3}} Article number ",
                        as.character(current_article_number), " of ", 
                        as.character(article_links %>% nrow()), " {{Pause=.3}} ",
                        current_article_title, " {{Pause=.3}} ",
                        current_article_subtitle, 
                        " {{Pause=.3}} By: ", current_article_author, " {{Pause=.3}} ", 
                        current_article_text %>% pull(text), " {{Pause=.3}} {{split}}")
                current_article_text <- tibble(text = current_article_text)
                
                # print(article_text)
                
                # split article if it's over excel's 32,767 character limit (no articles should be > 64000 characters)
                # yes there will be an abrupt cutoff mid-word at 32000 characters,
                # but not worth trying to snip at the end of a sentence etc
                if(nchar(current_article_text %>% pull(text)) > 32000) {
                        
                        # get current_article_text_part_1 and part_2
                        current_article_text_part_1 <- str_sub(string = current_article_text %>% pull(text), 
                                                               start = 1, end = 32000)
                        current_article_text_part_2 <- str_sub(string = current_article_text %>% pull(text), 
                                                               start = 32001, end = -1)
                        
                        # replace current_artcle_text with part_1 and part_2
                        current_article_text <- tibble(text = c(current_article_text_part_1,
                                                                current_article_text_part_2))
                        
                }
               
                # return current_article_text
                return(current_article_text)
        }
        
        # loop through article_links calling get_article_text_for_current_url
        all_article_text_tbl <- map2(.x = article_links %>% pull(url),
                                     .y = article_links %>% pull(article_number),
                                     .f = ~ get_article_text_for_current_url(current_url = .x, current_article_number = .y)) %>%
                bind_rows()
        
        # return all_article_text_tbl
        return(all_article_text_tbl)
}

# run get_article_text
output_tbl <- get_article_text(current_issue_url)

# get rid of weird characters
output_tbl <- output_tbl %>% mutate(text = str_replace_all(string = text, pattern = """, replacement = "'"),
                                    text = str_replace_all(string = text, pattern = """, replacement = "'"),
                                    text = str_replace_all(string = text, pattern = "Â", replacement = ""),
                                    text = str_replace_all(string = text, pattern = "-", replacement = "-"),
                                    text = str_replace_all(string = text, pattern = "'", replacement = "'"))

# save output_tbl
output_tbl %>% write_csv(file = "fa_article_text.csv")



