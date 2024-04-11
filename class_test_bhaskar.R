london_crime=read.csv("london-crime-data.csv")
#1 converting variables into chr to use paste()
str(london_crime)
View(london_crime)
london_crime$day=as.character(london_crime$day)
str(london_crime)
london_crime$month=as.character(london_crime$month)
str(london_crime$month)
london_crime$year=as.character(london_crime$year)
str(london_crime$year)
#using paste()
london_crime$Date <- as.Date(paste(london_crime$year, london_crime$month, london_crime$day, sep = "-"), format = "%Y-%m-%d")
View(london_crime)
#2
# renaming the variables by names()
names(london_crime)[names(london_crime) == "borough"] <- "Borough"
names(london_crime)[names(london_crime) == "major_category"] <- "MajorCategory"
names(london_crime)[names(london_crime) == "minor_category"] <- "SubCategory"
names(london_crime)[names(london_crime) == "value"] <- "Value"
names(london_crime)[names(london_crime) == "Date"] <- "CrimeDate"
# extracting the df by subset()
london_crime1=subset(london_crime,select=c("Borough","MajorCategory","SubCategory","Value","CrimeDate"))
View(london_crime1)
#3 I already changed the type to Date in question 1, below shows the str of crime date
str(london_crime1$CrimeDate)
#4
install.packages("vcd")
# Loading the necessary library for categorical data visualization
library(vcd)

# Aggregating crime data by Borough
crime_values_by_borough <- aggregate(Value ~ Borough, data=london_crime1, FUN=sum)

windows(16,10)

# Sorting the data for better visualization
crime_values_by_borough_sorted <- crime_values_by_borough[order(-crime_values_by_borough$Value),]

# Using barplot to visualize the aggregated crime values by borough
barplot(crime_values_by_borough_sorted$Value, names.arg=crime_values_by_borough_sorted$Borough, main="Aggregated Crime Values by Borough", xlab="Borough", ylab="Aggregated Crime Values", las=2, col="skyblue")

# Identify the borough with the highest and lowest level of crime
highest_crime_borough <- crime_values_by_borough_sorted$Borough[1]
lowest_crime_borough <- crime_values_by_borough_sorted$Borough[nrow(crime_values_by_borough_sorted)]
# below code shows that which  borough has the highest level of crime highest and lowest crime
highest_crime_borough
lowest_crime_borough
#5
# Aggregate crime values by MajorCategory
crime_values_by_category <- aggregate(Value ~ MajorCategory, data=london_crime1, FUN=sum)

# Converting the aggregated data into a named vector for the pie chart
crime_values <- setNames(crime_values_by_category$Value, crime_values_by_category$MajorCategory)

# Determining the highest and lowest categories of crime
highest_crime_category <- names(which.max(crime_values))
lowest_crime_category <- names(which.min(crime_values))
# below code shows the major category of highest level of crime and lowest level of crime
highest_crime_category
lowest_crime_category 

windows(16,10)

# Creating a pie chart to display the data
pie(crime_values, main="Major Categories of Crime in London", col=rainbow(length(crime_values)), labels=names(crime_values))
#6

# Create a mapping of Borough to Region
borough_to_region <- list(
  "Barking and Dagenham" = "East",
  "Barnet" = "North",
  "Bexley" = "East",
  "Brent" = "West",
  "Bromley" = "South",
  "Camden" = "North",
  "Croydon" = "South",
  "Ealing" = "West",
  "Enfield" = "North",
  "Greenwich" = "East",
  "Hackney" = "North",
  "Hammersmith and Fulham" = "West",
  "Haringey" = "North",
  "Harrow" = "West",
  "Havering" = "East",
  "Hillingdon" = "West",
  "Hounslow" = "West",
  "Islington" = "Central",
  "Kensington and Chelsea" = "Central",
  "Kingston upon Thames" = "East", 
  "Lambeth" = "Central",
  "Lewisham" = "Central",
  "Merton" = "South",
  "Newham" = "East",
  "Redbridge" = "East",
  "Richmond upon Thames" = "West",
  "Southwark" = "Central",
  "Sutton" = "South",
  "Tower Hamlets" = "Central",
  "Waltham Forest" = "Central",
  "Wandsworth" = "East", 
  "Westminster" = "Central"
)

# Assigning the Region to each Borough in the dataframe
london_crime1$Region <- sapply(london_crime1$Borough, function(x) ifelse(is.null(borough_to_region[[x]]), NA, borough_to_region[[x]]))

# Automatically replace NA regions with a default value if needed
# For example, assigning 'Unknown' to any NA regions
london_crime1$Region[is.na(london_crime1$Region)] <- 'Unknown'
View(london_crime1)
#7
# Aggregating the crime values by Region
crime_values_by_region <- aggregate(Value ~ Region, data=london_crime1, FUN=sum)

# Converting the regions to a factor to ensure they are treated as categorical data
crime_values_by_region$Region <- factor(crime_values_by_region$Region)

# Sorting the data for better visualization
crime_values_by_region_sorted <- crime_values_by_region[order(crime_values_by_region$Value), ]

# Bar plot
windows(16,10)
barplot(crime_values_by_region_sorted$Value, names.arg=crime_values_by_region_sorted$Region, 
        main="Reported Crimes by Region in London", ylab="Number of Reported Crimes", las=2, col="blue")

# Identifying the highest and lowest crime regions directly on the plot
highest_crime_value <- max(crime_values_by_region_sorted$Value)
lowest_crime_value <- min(crime_values_by_region_sorted$Value)

highest_crime_region <- crime_values_by_region_sorted$Region[which.max(crime_values_by_region_sorted$Value)]
lowest_crime_region <- crime_values_by_region_sorted$Region[which.min(crime_values_by_region_sorted$Value)]
#highest number od crimes as per each region
highest_crime_region 
# lowest number of crime as per region if may be unkonown due to NA values are replaced by unknown
lowest_crime_region
#8
# Extracting the subset of data for the region with the highest number of crimes
highest_crime_data <- subset(london_crime1, Region == highest_crime_region)
highest_crime_data

#aggregating this data by MajorCategory to find the most prevalent type of crime
highest_crime_categories <- aggregate(Value ~ MajorCategory, data=highest_crime_data, FUN=sum)
highest_crime_categories
# Sorting to find the major crime category with the highest count
highest_crime_categories <- highest_crime_categories[order(-highest_crime_categories$Value), ]
highest_crime_categories
# Extracting the subset of data for the region with the lowest number of crimes
lowest_crime_data <- subset(london_crime1, Region == lowest_crime_region)
lowest_crime_data
# Aggregating this data by MajorCategory
lowest_crime_categories <- aggregate(Value ~ MajorCategory, data=lowest_crime_data, FUN=sum)
lowest_crime_categories
# Sorting to find the major crime category with the lowest count
lowest_crime_categories <- lowest_crime_categories[order(-lowest_crime_categories$Value), ]
lowest_crime_categories
#9


# Setting the parameters to plot two charts side by side
par(mfrow=c(1, 2), mar=c(5, 4, 4, 2) + 0.3) 

# Finding the maximum value to set the same y-axis scale for both plots
max_value <- max(c(highest_crime_categories$Value, lowest_crime_categories$Value))

# Plotting for the region with the highest number of crimes
barplot(highest_crime_categories$Value, names.arg=highest_crime_categories$MajorCategory, 
        main="Most Prevalent Crime Categories\n(Highest Crime Region)", ylim=c(0, max_value),
        ylab="Number of Crimes", las=2, col="red")

# Assuming lowest_crime_categories is your data frame containing the aggregated crime data for the lowest crime region.

# Checking for NAs in your data which might cause issues with plotting
lowest_crime_categories <- na.omit(lowest_crime_categories)

# If there are too many categories, the labels might overlap and cause issues. You might consider shortening them or only displaying the top N.
# Here's a approach to limit to the top N if needed
top_n <- 10 # Adjust this number based on your needs
lowest_crime_categories_top_n <- head(lowest_crime_categories[order(-lowest_crime_categories$Value), ], n = top_n)

# Adjusting the names.arg to ensure it fits within the margins and doesn't overlap
names_arg_adjusted <- abbreviate(lowest_crime_categories_top_n$MajorCategory, minlength=5)

# Now, plotting the adjusted data
barplot(lowest_crime_categories_top_n$Value, names.arg=names_arg_adjusted, 
        main="Most Prevalent Crime Categories\n(Lowest Crime Region)", 
        ylim=c(0, max(lowest_crime_categories_top_n$Value)),
        ylab="Number of Crimes", las=2, col="blue")

# Resetting to default parameters after plotting
par(mfrow=c(1, 1), mar=c(5, 4, 4, 2))

#10
# saving the file to working directory
write.csv(london_crime1, "london-crimemodified.csv", row.names = FALSE)









