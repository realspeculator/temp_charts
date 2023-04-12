library(tidyverse)
library(rvest)

ticker <- "CVRX" # Change this

base_url <- paste0("https://stockanalysis.com/stocks/", ticker,"/financials/ratios/?")
is_url <- paste0("https://stockanalysis.com/stocks/", ticker,"/financials/") # income statement url

base_html <- read_html(base_url)
is_html <- read_html(is_url)

raw_text <- base_html %>%
  html_nodes('td.svelte-1k942df') %>%
  html_text()

raw_is <- is_html %>%
  html_nodes('td.svelte-1k942df') %>%
  html_text()

ratio_df <- matrix(raw_text, ncol = 12, byrow = T) %>%
  as.data.frame() %>%
  as_tibble() %>%
  `colnames<-`(c("Category", "2023", "2022", "2021", "2020", "2019", "2018", "2017",
                 "_2016", "_2015", "_2014", "_2013")) %>%
  mutate(Category = str_trim(Category)) %>%
  gather(2:ncol(.), key = "year", value = value) %>%
  mutate(value = parse_number(value)) %>%
  mutate(year = parse_number(year)) 

is_df <- matrix(raw_is, ncol = 11, byrow = T) %>%
  as.data.frame() %>%
  as_tibble() %>%
  `colnames<-`(c("Category", "2023", "2022", "2021", "2020", "2019", "2018", "2017",
                 "_2016", "_2015", "_2014", "_2013")) %>%
  mutate(Category = str_trim(Category)) %>%
  gather(2:ncol(.), key = "year", value = value) %>%
  mutate(value = parse_number(value)) %>%
  mutate(year = parse_number(year)) 

eps <- is_df %>%
  filter(str_detect(Category,pattern = "EPS \\(Diluted\\)"))

revenue <- is_df %>%
  filter(str_detect(Category,pattern = "^Revenue$"))

mkt <- ratio_df %>%
  filter(str_detect(Category, "Market Capitalization"))

coeff <- 10


p <- revenue %>%
  full_join(mkt) %>%
  spread(Category, value = value) %>%
  ggplot() +
  geom_line(aes(x = year, y = `Market Capitalization`, color = "Market Cap (in millions USD)")) +
  geom_line(aes(x = year, y = Revenue*13, color = "Revenue (in millions USD)")) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(sec.axis = sec_axis(~./13, name="Revenue", breaks = scales::pretty_breaks(n = 8)),
                     breaks = scales::pretty_breaks(n = 10)) +
  labs(x = "Year", y = "Market Cap", color = "",
       title = paste0("Ticker: ", ticker)) +
  scale_color_manual(values = c("orange2", "gray30")) +
  theme(
    axis.title.y = element_text(color = "gray30"),
    axis.title.y.right = element_text(color = "orange3")
  ) +
  ggthemes::theme_fivethirtyeight()
  

fname <- paste0("~/Desktop/temp/", ticker, ".png")
ggsave(filename = fname, dpi = 500)
