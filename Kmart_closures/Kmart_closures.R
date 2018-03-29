## Setting up the project. The following code chunk installs and loads all the packages in the "pkgs" vector.

pkgs <- c("acs","data.table","dplyr", "DT", "fuzzyjoin", "ggplot2", "janitor", "magrittr", "readr", "rvest", "scales", "stringdist", "tibble")

check <- sapply(pkgs, require, warn.conflicts = TRUE, character.only = TRUE)
if(any(!check)){
    pkgs.missing <- pkgs[!check]
    install.packages(pkgs.missing, repos = "http://cran.us.r-project.org")
    check <- sapply(pkgs.missing, require, warn.conflicts = TRUE, character.only = TRUE)
}
rm(pkgs, check)


#1. Scrape store addresses and store them as a vector called "all_links"
#   The function uses the rvest package to scrape whatever url you give it,
#   and retrieves the specific HTML objects you target, in this case <a> and href.

all_links <- read_html("http://www.kmart.com/stores.html") %>%
  html_nodes("a") %>%
  html_attr("href")
all_links 


#2. Grab only the relevant rows from "all_links", vector objects 33 through 84, 
#   and create a new vector called "store_links"

store_links <- all_links[33:84]
store_links 


#3. Create a function named "make_store_urls
#   The function uses paste0() to append each object in the "store_links" vector to the end of "http://www.kmart.com"
#   The outpout is saved into a new vector named "clean_urls"

make_store_urls <- function(links) {
  full_url <- paste0("http://www.kmart.com",links)
  return(full_url)
}
clean_urls <- make_store_urls(store_links)
clean_urls


#4.   Create a function named "get_addresses"
#   The function uses the rvest package to scrape whatever URLs you give it,
#   in this case the URLs stored in the "clean_urls" vector,
#   and retrieves the specific HTML objects you target, in this case text stored
#   as "streetAddress", "addressLocality", "addressRegion", "postalCode", and "telephone"

#   The stores are compiled together into a data frame called "all_store_addresses_raw"


get_addresses <- function(url_to_scrape) {
  address <- url_to_scrape %>%
    read_html() %>%
    html_nodes(xpath = '//*[@itemprop="streetAddress"]') %>%
    html_text(trim = TRUE)
 
  city <- url_to_scrape %>%
    read_html() %>%
    html_nodes(xpath = '//*[@itemprop="addressLocality"]') %>%
    html_text(trim = TRUE)
  
  state <- url_to_scrape %>%
    read_html() %>%
    html_nodes(xpath = '//*[@itemprop="addressRegion"]') %>%
    html_text(trim = TRUE)
 
  zip <- url_to_scrape %>%
    read_html() %>%
    html_nodes(xpath = '//*[@itemprop="postalCode"]') %>%
    html_text(trim = TRUE)
  
   phone <- url_to_scrape %>%
    read_html() %>%
    html_nodes(xpath = '//*[@itemprop="telephone"]') %>%
    html_text(trim = TRUE)

  store_data <- data.frame(address, city, state, zip, phone)
  return(store_data)
}

all_store_addresses_raw <- rbindlist(lapply(clean_urls, get_addresses))
all_store_addresses_raw[] <- lapply(all_store_addresses_raw, as.character)
all_store_addresses_raw


#5. First, copy "all_store_addresses_raw" into a new data frame called "all_store_addresses"
#   Add a new column to the data frame "all_store_addresses" called "status"
#   For now, we'll assume all the stores have a status of "not closing,"
#   but we'll overwrite this later once we now which stores are set to close

#   We'll also remove the "phone" column from our data frame, since we don't need it.
#   The easiest way to do this is to set the column to NULL

all_store_addresses <- all_store_addresses_raw 

all_store_addresses$status <- "not closing"
all_store_addresses$phone <- NULL

all_store_addresses


#6. Read in "010418-store-closing-list.csv" from github
#   and name the data frame "closing_stores_raw"

closing_stores_raw <- read_csv(file = "https://raw.githubusercontent.com/davidingold/R_training/master/Kmart_closures/Data/010418-store-closing-list.csv")
closing_stores_raw

#7. The janitor package has a function called clean_names() that makes it
#   easier to work with column names. We'll run that on "closing_stores_raw"
#   and save the results as "closing_stores"

closing_stores_clean <- clean_names(closing_stores_raw)
closing_stores_clean

#8. Lets rename one of the columns in "closing_stores_clean"
#   and grab only the columns we want using select() 
#   We'll write this to a new data frme called "closing_stores"

closing_stores <- closing_stores_clean %>% select(address = street_address, city, state, zip)
closing_stores


#9. Add a new column to the data frame "closing_stores" called "status"
#   We know all the stores on this list are set to close, so we'll set
#   their status to "closing" and overwrite the existing data frame

closing_stores$status <- "closing"
closing_stores

# 10. So I just noticed that the "state" column in "closing_stores" uses 
#     state abbreviations, while the scraped data in "all_store_addresses" uses
#     full state names. Luckily, R Studio has built in state matching abilities.
#     We can use the match() function to replace all the abbreviations in the "state"
#     column with full names, and then overwrite "closing_stores$state"
    
closing_stores$state <- state.name[match(closing_stores$state,state.abb)]
closing_stores

# 11. This next bit of code can be confusing. But I'll try my best to walk you through it,
#     The function rbind() adds together two data frames that have the same columns.
#     For example, below we add the rows from "closing stores" to the bottom
#     of all_stores_addresses and saves it as a new data frame called "test_data_frame"

test_data_frame <- rbind(all_store_addresses, closing_stores)
test_data_frame 

# 12. But I suspect there are a number of repeated stores,
#     since Kmart probably hasn't updated their website yet to remove
#     stores that are set for removal. But maybe they have, either way
#     we can check to see if any stores are duplicated in the new
#     data frame we just created

#     The code below groups all the stores that have the same city, state and zip,
#     and then creates a new data frame where there are two or more stores

multiple_stores <- test_data_frame %>%
  group_by(city, state, zip) %>%
  tally() %>% filter(n >= 2)

View(multiple_stores)

# There are 59 repeats. I can create a new vector that is just the zip codes
# of these repeats. Then I can filter out stores that are in these zip codes.

repeated_zips <- multiple_stores$zip
test_data_frame %>% filter(zip %in% repeated_zips) %>% arrange(zip)

# It looks like there are numerous duplicates, but their addresses fields
# don't perfectly match. We also can't merge on ZIP codes alone, because there
# are some cases where multiple stores exist in the same ZIP code.
# 
# Instead, we can use a package called fuzzyjoin that will try and match
# the "all_store_addresses" and "closing_stores" data frames the best it can,
# by comparing the two data frames and looking for similar text. We'll use "full"
# as the mode, because we wan't to keep all the rows, even if they don't have a
# match in both data frames. This is ok though, because not all stores are closing.

test <- stringdist_left_join(x = all_store_addresses,
                y = closing_stores,
                by = "address",
                distance_col = NULL)

stringdist

all_stores_and_closing <- rbind(all_store_addresses[!all_store_addresses$zip %in% closing_stores$zip,], closing_stores)

## Calling in U.S. Census data

api.key.install("fd68cfbf9e76890c847a2550d660865147db3685")
# Backup key: fdf8d8199abdff788753f0b3faf8a92b50751fbe

raw_acs_data <- acs.fetch(endyear = 2016,
          span = 5,
          geo.make(zip.code = "*"),
          table.number = "B19013",
          dataset = "acs",
          col.names = "auto")

zips_income <- data.frame(raw_acs_data@estimate) %>%
  rownames_to_column("zip") %>%
  mutate(zip = str_replace(string = zip,
                           pattern = "ZCTA5 ",
                           replacement = "")) %>%
  dplyr::rename(income = B19013_001)

stores_with_income <- merge(x = all_stores_and_closing,
                            y = zips_income,
                            by = "zip",
                            all.x = TRUE)

stores_with_income_clean <- stores_with_income %>%
  na.omit() %>%
  filter(income >= 0)


## Charting my findings
### Box and whisker

ggplot(data = stores_with_income_clean, aes(x = closing, y = income), alpha = 0.5) +
  geom_boxplot() + geom_jitter(width = 0.1) +
  scale_y_continuous(labels = comma) +
  labs(title = "Household income near Kmart stores", x = "Store status", y = "Household income")


### Violin plot

ggplot(data = stores_with_income_clean, aes(x = closing, y = income), alpha = 0.5) +
  geom_violin() + geom_jitter(width = 0.1) +
  scale_y_continuous(labels = comma, breaks = seq(0, 140000, 10000)) +
  labs(title = "Household income near Kmart stores", x = "Store status", y = "Household income")

### Histogram

ggplot() + 
  geom_histogram(data = stores_with_income_clean %>% filter(closing == "not closing"),
                 aes(x = income, fill = "Not closing"), binwidth = 1000) +
  geom_histogram(data = stores_with_income_clean %>% filter(closing == "closing"),
                 aes(x = income, fill = "Closing"), binwidth = 1000) +
  scale_x_continuous(labels = comma) +
  labs(title = "Household income near Kmart stores",
       x = "Household income", y = "Number of stores", fill = "Store status")

ggplot() + 
  geom_histogram(data = stores_with_income_clean %>% filter(closing == "not closing"),
                 aes(x = income, fill = "Not closing"), binwidth = 5000) +
  geom_histogram(data = stores_with_income_clean %>% filter(closing == "closing"),
                 aes(x = income, fill = "Closing"), binwidth = 5000) +
  scale_x_continuous(labels = comma) +
  labs(title = "Household income near Kmart stores",
       x = "Household income", y = "Number of stores", fill = "Store status") +
  facet_wrap(~state)



ggplot() + 
  geom_histogram(data = stores_with_income_clean %>%
                   filter(closing == "not closing" &
                            state %in% c("Colorado", "Georgia", "New York", "West Virginia"),
                 aes(x = income, fill = "Not closing"), binwidth = 5000)) +
  geom_histogram(data = stores_with_income_clean %>%
                   filter(closing == "not closing" &
                            state %in% c("Colorado", "Georgia", "New York", "West Virginia"),
                 aes(x = income, fill = "Closing"), binwidth = 5000)) +
  labs(title = "Household income near New York Kmart stores",
       x = "Household income", y = "Number of stores", fill = "Store status") +
  facet_wrap(~state)


