library(factoextra)
library(tidyverse)

# Let's start by reading in the data 
prop_data_raw <- read_delim("output/cop_property_data.csv", delim = "|")


# feature engineering
prop_data_feat <- prop_data_raw %>% 
  filter(province %in% c("western-cape", "kwazulu-natal")) %>% 
  mutate(
    bedrooms = gsub(".*([0-9]+).*", "\\1", type),
    bedrooms = as.double(bedrooms),
    bedrooms = case_when(
      !is.na(bedrooms) ~ bedrooms,
      grepl("([Bb]achelor)|([Ss]tudio)", type) ~ 0.5,
      T ~ NA_real_
    ),
    size = gsub("([0-9]+).*", "\\1", floor_size),
    size = as.double(size),
    deposit_prop = deposit/price
  ) %>% 
  filter(!is.na(bedrooms)) 

# outlier
prop_data_outliers <- prop_data_feat %>% 
  mutate(
    # log price and deposit
    across(
      price:deposit,
      ~log(.x)
    ),
    across(
      price:deposit,
      ~!between(
        .x,
        quantile(.x, 0.25) - (1.5 * IQR(.x)),
        quantile(.x, 0.75) + (1.5 * IQR(.x))
      ),
      .names = "outlier_{.col}"
    )
  )


# visualise & further remove outliers
prop_data_outliers %>% 
  ggplot(
    aes(x = bedrooms, y = price, color = outlier_price)
  ) +
  geom_jitter(alpha = 0.8) +
  scale_y_continuous(trans = "log") +
  theme_bw()

# deposit outlier display
prop_data_outliers %>% 
  filter(!outlier_deposit) %>% 
  ggplot(
    aes(x = bedrooms, y = deposit, color = outlier_deposit)
  ) +
  geom_jitter(alpha = 0.8) +
  scale_y_continuous(trans = "log") +
  theme_bw()


prop_data <- prop_data_outliers %>% 
filter(
  !outlier_price,
  !outlier_deposit,
  bedrooms != 0, 
  bedrooms <= 4
) %>% 
  select(
    listing_number,
    province,
    city,
    type,
    price,
    deposit,
    days_ad_active,
    bedrooms,
    size
  )

# Okay, now we've run out of dimensions, let's do a PCA

# estimate the pca
res.pca <- prop_data %>% 
  select(where(is.numeric)) %>% 
  prcomp(., scale = TRUE)


# Scree plot
fviz_eig(res.pca)

res.ind <- get_pca_ind(res.pca)
res.ind$coord

property_pca <- bind_cols(prop_data, res.ind$coord)

property_pca %>% 
  filter(city %in% c("durban", "cape-town")) %>%
  ggplot(
    aes(x = Dim.1, y = Dim.2, color = city)
  ) +
  geom_point(alpha = 0.6, size = 3) +
  theme_bw()


