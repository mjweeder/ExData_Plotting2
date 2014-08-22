## Data Exploration by Ron Collins
## assignment #2, plot number 6

# Compare emissions from motor vehicle sources in Baltimore City (fips == "24510")with emissions from motor vehicle # sources in Los Angeles County, California (fips == "06037"). Which city has seen greater changes over time in 
# motor vehicle     
# emissions?


## Download, unzip, and then read files for processing.
# Create temp file
temp <- tempfile()

# Download zipp file from internet into temp file
download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip", temp)

proj2_unzip <- unzip(temp)

# read data for emission of mp2 pollution data
NEI <- readRDS(proj2_unzip[2], "summarySCC_PM25.rds")


# read data for classification of mp2 pollution data
SCC <- readRDS(proj2_unzip[1], "Source_Classification_Code.rds")


# Create logical vector for locations for Los Angeles and Baltimore city
vect_BC <- NEI$fips == "24510" | NEI$fips == "06037"

# Create new dataframe from  NEI dataframe to contain only those observations from Los Angeles county and Baltimore City
NEI_BC <- NEI[vect_BC,]

# modify NEI$type from factor to character for future subsetting
NEI$type <- as.character(NEI$type)

# Create logical vector for on-road activities for NEI and then create dataframe from this logical vector
vect_type <- (NEI_BC$type == "ON-ROAD")
NEI_temp <- NEI_BC[vect_type,]

# The facet labels are for the zipcode not the text name of the locations.  Consequently, an additional column with the appropriate location name is created and then added to the NEI_emis dataframe for plotting.

# Create vector for location name instead of zip code
location <- character(0)

#Loop to go through NEI_temp dataframe and add Baltimore City, MD for 24510 and Los Angeles County, CA for 06037
for (i in 1:length(NEI_temp[,1])) {
    if (NEI_temp[i,1] == "24510") {
        location[i] <- "Baltimore City, MD"
    }
    if (NEI_temp[i,1] == "06037") {
        location[i] <- "Los Angeles County, CA"
    }
}

# add vector location to dataframe NEI_temp
NEI_emis <- cbind(location,NEI_temp)

# Create plot for motor vehicle sources changed from 1999-2008 in Baltimore City.
# open grafic device
windows()

# convert year from int to character so that the X axis is labeled correctly.
NEI_emis$year <- as.character(NEI_emis$year)
g <- ggplot(NEI_emis, aes(year,Emissions))
p <- g +  facet_grid(~location) + ggtitle("Change in Vehicle emission between 1999 and 2008") + geom_bar(stat = "identity") + ylab("Emissions (tons)")

print(p)

#Save plot as png file
dev.copy(png,'plot6.png')
dev.off()