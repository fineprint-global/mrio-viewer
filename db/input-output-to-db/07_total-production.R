##########################################################################
### 7. X.rds - add total production as environmental use
##########################################################################

data <- read_file_function(sprintf(file_format, year, file_names$X[1]))
# the order of products and countries is here:
# country 1 with total production for products from 1:130 (1 to 130)
# country 2 with total production for products from 1:130 (1.1 to 130.1)
# ...
# country 192 with total production for products from 1:130 (1.191 to 130.191)

insert_data <- data %>% 
  enframe(name = NULL) %>% # "product.country"
#  as_tibble() %>% 
  dplyr::mutate(year = year) %>%
  dplyr::mutate(from_region = rep(region$id, each = 130)) %>% # each for 130 products, 4 elem * 192 countries
  dplyr::mutate(from_product = rep(product$id, times = 192)) # 4 elements * 192 countries * 192 countries
  