## To Open and view a data ##
library(readxl) # To be able to view excel files #
restaurant_data <- read_excel("C:/Users/chakr/Downloads/DA/restaurant_data.xlsx") # To import the dataset #
View(restaurant_data) # To be able to view the data #

## To view the number of rows in a dataset ##
nrow(restaurant_data)

## To view the number of columns in a dataset ##
ncol(restaurant_data)

## To see the no. of categories in a column and the no. of contents in it. ##
table(restaurant_data$City)

## To see the maximum no. of restaurants in the city column ##
rest_count<-table(restaurant_data$City)

## To see the maximum no. of restaurants in a city with the city name ##
restaurant_data %>% count(City, sort = TRUE)

restaurant_data %>% count(`Restaurant Name, City, sort = TRUE)
restaurant_data %>% count(`Restaurant Name, City, sort = TRUE)

restaurant_data %>% count(Restaurant_Name,City, sort = TRUE)

## How to use filter ##
restaurant_data %>% filter(Has_Table_booking=="No")-> no_rest_bookng
restaurant_data %>% filter(Has_Table_booking=="Yes")-> yes_rest_bookng

## How to find out ratio ##
no_rest_bookng %>% count(Has_Table_booking, sort = TRUE) #First count the no. of Nos #
# Now give a name to it #
no_rest_bookng %>% count(Has_Table_booking, sort = TRUE)-> no_bookng_count
View(no_bookng_count)
#Now do the same with yes #
yes_rest_bookng %>% count(Has_Table_booking, sort = TRUE)
yes_rest_bookng %>% count(Has_Table_booking, sort = TRUE)-> yes_bookng_count

# now choose the column where the count is stored while dividing #
table_booking_counts <- as.data.frame(table(restaurant_data$Has_Table_booking))
ggplot(data = table_booking_counts, aes(x ="", y = Freq, fill = factor(var1, labels = c("No", "Yes")))) + geom_bar(stat = "identity", width = 1) + coord_polar(theta = "y") + ggtitle("Ratio of Restaurants with Table Booking System")+ labs(x = NULL, y = NULL, fill= "Table Booking System") + guides(fill= guide_legend(reverse = TRUE))+theme_void()

## How to do a percentage ##
restaurant_data %>% filter(Has_Online_delivery=="Yes")-> yes_onln_deli
View(yes_onln_deli)
yes_onln_deli %>% count(Has_Online_delivery, sort = TRUE)-> yes_onln_deli_count
View(yes_onln_deli_count)
sum(onln_deli_count$n, na.rm = TRUE) # onln_deli_count already had the yes and no counts #
sum(onln_deli_count$n, na.rm = TRUE)-> total_onln_deli
View(total_onln_deli)
percentage_yes_onln_deli<- yes_onln_deli_count$n/total_onln_deli*100
View(percentage_yes_onln_deli)

## How to find the difference between votes of "yes" and "no" ##
sum(yes_onln_deli$Votes, na.rm = TRUE)-> total_vote_yes_deli
restaurant_data %>% filter(Has_Online_delivery=="No")-> no_onln_deli

sum(no_onln_deli$Votes, na.rm = TRUE)-> total_vote_no_deli
difference_in_votes<- total_vote_no_deli - total_vote_yes_deli

View(difference_in_votes)

## To get maximum count from a column ##
max(str_count(restaurant_data$Cuisines, '\\w+'), na.rm = TRUE)

## To find and omit missing value datas ## 
sum(is.na(restaurant_data))
restaurant_data <- na.omit(restaurant_data)
## To find the number of cuisines a restaurant serves and which Cuisines they are ##
df_cuisines <- restaurant_data %>% separate_rows(Cuisines, sep = ",") %>% distinct(Restaurant_Name, Cuisines) %>% summarise(n_unique_cuisines = n ())
top_restaurant <- df_cuisines%>%filter(n_unique_cuisines == max(n_unique_cuisines))
df_cuisines <- restaurant_data %>% separate_rows(Cuisines, sep = ",") %>% distinct(Restaurant_Name, Cuisines) %>%group_by(Restaurant_Name)%>% summarise(n_unique_cuisines = n ())
top_restaurant <- df_cuisines%>%filter(n_unique_cuisines == max(n_unique_cuisines))

## Top 10 cuisines with served across cities ##
city_cuisine_count<- restaurant_data %>% group_by(City, Cuisines) %>% summarise(count = n()) %>% ungroup()

sorted_counts <- city_cuisine_count%>% arrange(City,desc(count))

top_10_cuisines <- sorted_counts%>% group_by(City)%>% top_n(10, count) %>% arrange(City, desc(count)) %>% pivot_wider(names_from = Cuisines, values_from = count, values_fill = 0)

View(top_10_cuisines)

print(top_10_cuisines)


## How to find the top ten cities with most number of restaurants ##
top_ten_cities  <- restaurant_data %>% group_by(City) %>% summarise(n_restaurants = n()) %>% arrange(desc(n_restaurants))%>% top_n(10,n_restaurants)
View(top_ten_cities)

## How to find minimum number of restaurant cities ##
min_num_rest_cities <-restaurant_data %>% group_by(City) %>% summarise(n_restaurants = n()) %>% arrange(n_restaurants)%>% top_n(10,n_restaurants)
View(min_num_rest_cities)

## To visualize the minimum and maximum data ##
ggplot(data = top_ten_cities, aes( x = reorder(City, n_restaurants), y = n_restaurants)) + geom_bar(stat = "identity", fill = "blue") +ggtitle("Top 10 cities with most number of restaurants") + xlab("City Names") + ylab("Number of restaurants")

## To merge two dataframes into one ##
merge_df <- merge(restaurant_data, Country_Code, by.x = "Country_Code", by.y = "Country Code", all.x = TRUE)
View(merge_df) # In the above line it is necessary to have a common column to join the two dataframes together. #

## To find the countries with most number of franchises ##
franchise_counts <-merge_df%>% group_by(Country)%>% summarise(n_franchises = n()) %>% arrange(desc(n_franchises))
top_country<- franchise_counts$Country

# To find out restaurant name #
franchise_rest <- merge_df %>% filter(Country == "India") %>% group_by(Restaurant_Name) %>% summarise(n_franchises = n())%>% arrange(desc(n_franchises)) %>% top_n(1,n_franchises)
top_franchise_rest <- franchise_rest$Restaurant_Name
View(top_franchise_rest)


## Cost distribution in different cities ##
ggplot(data = merge_df, aes(x = City , y = Average_Cost_for_two )) + geom_boxplot(fill = "orange") + ggtitle("Average Cost for Two in different Cities") + xlab("City Names") + ylab("Average Cost for two")

## Factors affecting the Aggregate rating of restaurants ## 

# Average Cost for two factor #
ggplot(data = merge_df, aes(x = Average_Cost_for_two , y = Aggregate_rating )) + geom_boxplot(fill = "orange") + ggtitle("Average Cost for Two factor affecting the Aggregate rating") + xlab("Average Cost for two") + ylab("Aggregate rating")

# Cuisines factor #
ggplot(data = merge_df, aes(x = Cuisines, y = Aggregate_rating )) + geom_boxplot(fill = "orange") + ggtitle(" Cuisine factor affecting the Aggregate rating") + xlab(" No. of Cuisines") + ylab("Aggregate rating")

# Online delivery factor #
ggplot(data = merge_df, aes(x = Cuisines, y = Aggregate_rating )) + geom_boxplot(fill = "orange") + ggtitle(" Cuisine factor affecting the Aggregate rating") + xlab(" No. of Cuisines") + ylab("Aggregate rating")

# Table booking facility factor #
ggplot(data = merge_df, aes(x = Has_Table_booking, y = Aggregate_rating )) + geom_boxplot(fill = "orange") + ggtitle(" Table booking facility factor affecting the Aggregate rating") + xlab("Has Table Booking facility") + ylab("Aggregate rating")

# Votes factor #
ggplot(data = merge_df, aes(x = Has_Table_booking, y = Aggregate_rating )) + geom_boxplot(fill = "orange") + ggtitle(" Table booking facility factor affecting the Aggregate rating") + xlab("Has Table Booking facility") + ylab("Aggregate rating")
