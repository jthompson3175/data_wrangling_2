Untitled
================
Julia Thompson
10/10/2019

## Using HTML

Read in the NSDUH
data

``` r
url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

drug_use_xml = read_html(url)

table_list = (drug_use_xml %>% html_nodes(css = "table"))
tabl_marj = 
  table_list[[1]] %>% 
  html_table() %>% 
  slice(-1) %>% 
  as_tibble()
```

Another example- go back to this later

``` r
nyc_cost = 
  read_html("https://www.bestplaces.net/cost_of_living/city/new_york/new_york") %>%
  html_nodes(css = "table") %>%
  .[[1]] %>%
  html_table(header = TRUE)
```

Harry Potter data

``` r
hpsaga_html = 
  read_html("https://www.imdb.com/list/ls000630791/")
```

``` r
hp_movie_names =
  hpsaga_html %>% 
  html_nodes(".lister-item-header a") %>% 
  html_text()

hp_movie_runtime =
  hpsaga_html %>% 
  html_nodes(".runtime") %>% 
  html_text()

hp_movie_money = 
  hpsaga_html %>%
  html_nodes(".text-small:nth-child(7) span:nth-child(5)") %>%
  html_text()

hp_df = 
  tibble(
    title = hp_movie_names,
    rev = hp_movie_money,
    time = hp_movie_runtime
  )
```

Napoleon Dynamite

Note: this is just for one page of reviews, not all of
them

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_nodes(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text()

review_text = 
  dynamite_html %>%
  html_nodes(".review-text-content span") %>%
  html_text()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

## Using an API

``` r
nyc_water = 
  GET("https://data.cityofnewyork.us/resource/waf7-5gvc.csv") %>% 
  content("parsed")
```

    ## Parsed with column specification:
    ## cols(
    ##   year = col_double(),
    ##   new_york_city_population = col_double(),
    ##   nyc_consumption_million_gallons_per_day = col_double(),
    ##   per_capita_gallons_per_person_per_day = col_double()
    ## )

``` r
nyc_water = 
  GET("https://data.cityofnewyork.us/resource/waf7-5gvc.json") %>% 
  content("text") %>%
  jsonlite::fromJSON() %>%
  as_tibble()
```

Link for datasets in data.gov that have an API:
<https://catalog.data.gov/dataset?q=-aapi+api+OR++res_format%3Aapi#topic=developers_navigation>

``` r
poke = 
  GET("http://pokeapi.co/api/v2/pokemon/1") %>%
  content()

poke$name
```

    ## [1] "bulbasaur"

``` r
poke$abilities
```

    ## [[1]]
    ## [[1]]$ability
    ## [[1]]$ability$name
    ## [1] "chlorophyll"
    ## 
    ## [[1]]$ability$url
    ## [1] "https://pokeapi.co/api/v2/ability/34/"
    ## 
    ## 
    ## [[1]]$is_hidden
    ## [1] TRUE
    ## 
    ## [[1]]$slot
    ## [1] 3
    ## 
    ## 
    ## [[2]]
    ## [[2]]$ability
    ## [[2]]$ability$name
    ## [1] "overgrow"
    ## 
    ## [[2]]$ability$url
    ## [1] "https://pokeapi.co/api/v2/ability/65/"
    ## 
    ## 
    ## [[2]]$is_hidden
    ## [1] FALSE
    ## 
    ## [[2]]$slot
    ## [1] 1
