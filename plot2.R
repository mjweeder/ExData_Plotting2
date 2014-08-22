## Data Exploration by Ron Collins
## assignment #2, plot number 2

# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? 
# Use the base plotting system to make a plot answering this question.

rm(NEI_Emissions_total,emissionVector,vect_sel,YearVector)
rm(NEI_temp,year_emis, yearVector, i, proj2_unzip,NEI, SCC)
rm(NEI1999,proj2_unzip.chr, temp,vect1999,vect2002,vect2005,vect2008)

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

# Create integer vector with years that analysis is conducted for
yearVector <- c("1999", "2002", "2005", "2008")

#create blank vector to hold total emmission for each year
emissionVector <- numeric(0)

year_emis <- data.frame (yearVector=as.character(0), emissionVector = as.numeric(0))
year_emis$yearVector <- as.character(year_emis$yearVector)




# create list with logical vectors for subsetting NEI by years
vect_sel <- list(vect1999 = NEI$year == 1999 & NEI$fips == "24510", vect2002 = NEI$year == 2002 & NEI$fips == "24510" , vect2005 = NEI$year == 2005 & NEI$fips == "24510" , vect2008= NEI$year == 2008 & NEI$fips == "24510" )

# Loop calculates emissions for each year and then assigns year and emission to dataframe
for (i in 1:length(yearVector)){
    # calculate total emissions for 1999, 2002, 2005, 2008 
    NEI_temp <- NEI[vect_sel[[i]],] # create data frame for the specified year
    NEI_Emissions_total <- sum(NEI_temp$Emissions) # sum up the total emissions for specified year
    emissionVector[i] <- NEI_Emissions_total        # assign the caculated total emission to emission vector
    year_emis[i,2] <- emissionVector[i]
    year_emis[i,1] <- yearVector[i]
}


# Create barplot of emmission by year for total emission for country
# open grafic device
windows()
barplot(year_emis$emissionVector, names = year_emis$yearVector,
        xlab = "Year", ylab = "Total Emission (tons)",
        main = " Total PM2.5 emission for 1999, 2002, 2005, and 2008 for zipcode 24510")

#Save plot as png file
dev.copy(png,'plot2.png')
dev.off()