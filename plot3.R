## Data Exploration by Ron Collins
## assignment #2, plot number 3

# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these
# four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in
# emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.


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



# create list with logical vectors for subsetting NEI to include only Baltimore City
vect_selection <-  NEI$fips == "24510" 


# Create data frames for each of the types of pollution: NON-POINT, NONPOINT, ON-ROAD, POINT.
# Utilize the year_emis data frame to create 4 logical vectors based on type of polution
NEI$type <- as.factor(NEI$type)
NEI_temp <- NEI[vect_selection,]

# eliminate on outlayer which messes up plot
q <- NEI_temp$Emissions < 500
NEI_BC <- NEI_temp[q,]


# Create barplot of emmission by year for total emission for country
# open grafic device
windows()

g <- ggplot(NEI_BC, aes(year,Emissions))
p <- g +geom_point() + facet_grid(~type) + geom_smooth(method = "lm") + theme(axis.text.x=element_text(color = "black", angle=30)) +  ggtitle("Which Polution Source Decreased for Which Year?")

print(p)

#Save plot as png file
dev.copy(png,'plot3.png')
dev.off()