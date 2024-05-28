see if I can read from betfred's website


this doesn't work


``` r
library(tidyverse)
library(rvest)

my_url <- "https://www.betfred.com/sports/football"
html <- read_html(my_url)
html %>% html_nodes(css = "div._yynop3:nth-child(1)")
```

```
## {xml_nodeset (0)}
```

