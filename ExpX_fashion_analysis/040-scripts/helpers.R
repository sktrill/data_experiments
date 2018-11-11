# purpose : logical exclusion function
`%notin%` <- function(x,y) !(x %in% y) 

# input   : df of all user segment data (segments), df of order history for user and product category 
# output  : list of product lovers
# purpose : user_id list generation function for marketing campaigns
getUserList <- function(customers_by_category, segments, product_category) {
  
  lovers <- customers_by_category %>% filter(category == product_category) %>% 
    arrange(desc(avg_order))
  lovers <- left_join(lovers, segments, by = "user_id")
  lovers$cohort <- as.Date(lovers$cohort)
  
  # remove all NA values (where gender = NA)
  sum(is.na(lovers))
  sum(is.na(lovers$gender))
  lovers <- na.omit(lovers)
  
  # let us now set our associative rules
  
  # 1. filter for all high end users that are driving the bottom line, pick the top 10
  # these folks buy the product consistently but may also buy other things
  filter_high_end <- lovers %>% group_by(category) %>%
    arrange(desc(revenue), .by_group = TRUE)
  filter_high_end <- mutate(filter_high_end, rank_rev = dense_rank(desc(revenue)))
  filter_high_end <- filter_high_end %>% filter(rank_rev <= 10)
  
  # 2. filter for all customers that are in the top 25% in revenue for the category 
  # and purchase more than median number of items
  # and spend more than 30% of their total Everland spending on the category
  filter_cash_cows <- lovers %>% group_by(category) %>%
    arrange(revenue, .by_group = TRUE)
  filter_cash_cows <- mutate(filter_cash_cows, rank_rev = percent_rank(revenue))
  filter_cash_cows <- filter_cash_cows %>% 
    filter(rank_rev >= 0.75 & items > mean(filter_cash_cows$items) & pct_revenue > 0.3)
  
  # 3. filter for new customers (last 6 months) that spent 50% of their spending on this one category
  filter_new_lovers <- lovers %>% filter(cohort >= "2017-06-01" & pct_revenue > 0.5)
  
  # 4. filter for customers that bought a product from that category in the last 30 days
  filter_recent_lovers <- lovers %>% filter(last_category == product_category & since_last <= 30)
  
  # join all types
  lovers_list <- bind_rows(filter_high_end, filter_cash_cows, filter_new_lovers, filter_recent_lovers, .id = "id")
  stars <- intersect(filter_high_end$user_id, filter_cash_cows$user_id)
  lovers_list$id[lovers_list$user_id %in% stars] <- 0
  
  # remove duplicate user_ids
  lovers_list <- lovers_list[!duplicated(lovers_list$user_id),]
  
  # clean up user categories
  lovers_list$id[lovers_list$id == 0] <- "1 - Stars"
  lovers_list$id[lovers_list$id == 1] <- "2 - High enders"
  lovers_list$id[lovers_list$id == 2] <- "3 - Cash cows"
  lovers_list$id[lovers_list$id == 3] <- "4 - New lovers"
  lovers_list$id[lovers_list$id == 4] <- "5 - Recent lovers"
  
  return (lovers_list)
}


# input   : product lovers list
# output  : summary table to print
# purpose : print summaries of user_id list for marketing campaigns
printSummary <- function(product_lover) {
  
  # arrange by id
  slice <- product_lover %>% 
    group_by(id, gender) %>% 
    arrange(id, .by_group = TRUE)
  
  data_cut <- summarise(slice, 
                        revenue = sum(revenue),
                        avg_order = round(sum(revenue) / sum(orders),2),
                        items = sum(items)
  )
  
  return (data_cut)
  
}
