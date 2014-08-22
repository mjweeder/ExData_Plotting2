## Data Exploration by Ron Collins
## assignment #2, plot number 5

## How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City? 

# function to remove objectives during testing
rm(SCC_coal,SCC_same,coalUse,desiredSCC,i,j,NEI_temp, NEI_outcome, NEI_SCC, vect_type)
rm(NEI_shrink, vect_typeA, NEI_BC,SCC_temp,vect_emis, g,p,vect_BC,vect_scc, NEI_emis)


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


# subset for baltimore city
vect_BC <- NEI$fips == "24510" 

# create dataframe with only Baltimore City
NEI_BC <- NEI[vect_BC,]
# Create logical vector CoalUse for SCC Items which contain coal

# SCC _coal numbers are only for point/nonpoint pollution, all others can be removed.
# Create logical vector for types which are point or nonpoint
NEI$type <- as.character(NEI$type)

# Create logical vector for on-road activities for NEI and then create dataframe from this logical vector
vect_type <- (NEI_BC$type == "ON-ROAD")
NEI_temp <- NEI_BC[vect_type,]

# Create logical vector for on-road activities for SCC and then create dataframe from this logical vector
vect_type <- (SCC$Data.Category == "Onroad")
SCC_temp <- SCC[vect_type,]

# Remove the one outlier whose emision is above 1000
vect_emis <- NEI_temp$Emissions <= 500
NEI_emis <- NEI_temp[vect_emis,]


# Create plot for motor vehicle sources changed from 1999-2008 in Baltimore City.

# open grafic device
windows()

# convert NEI_temp$year to character from int so plot x axis label will be correct
NEI_emis$year <- as.character(NEI_emis$year)

g <- ggplot(NEI_emis, aes(year,Emissions))
p <- g +  ggtitle("Change in Vehicle emission between 1999 and 2008") + geom_bar(stat = "identity") + ylab("Emissions (tons)")

print(p)

#Save plot as png file
dev.copy(png,'plot5.png')
dev.off()