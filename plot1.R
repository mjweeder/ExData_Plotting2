## Data Exploration by Ron Collins
## assignment #2, plot number 1

# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system,
# make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.


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

# Create empty dataframe (year_emis0 with two variables which will be used in graph
year_emis <- data.frame (yearVector=as.integer(0), emissionVector = as.numeric(0))

# create list with four logical vectors for subsetting NEI by years  for use in calculating total emission for each 
# year.
vect_sel <- list(vect1999 <- NEI$year == 1999, vect2002 <- NEI$year == 2002, vect2005 <- NEI$year == 2005,vect2008<- NEI$year == 2008 )             

# Loop which goes through iteration for each year
# The total emission is cacaluated for each year
# the year and total emission for each year is assigned to the Year_emis data frame
for (i in 1:length(yearVector)){
    # calculate total emissions for 1999, 2002, 2005, 2008 
    NEI_temp <- NEI[vect_sel[[i]],] # create data frame for the specified year
    NEI_Emissions_total <- sum(NEI_temp$Emissions) # sum up the total emissions for specified year
    emissionVector[i] <- NEI_Emissions_total        # assign the caculated total emission to emission vector
    year_emis[i,2] <- emissionVector[i]
    year_emis[i,1] <- yearVector[i]
}

# Create histogram of emmission by year
# open grafic device
windows()
# Create the barplot using base graphics
barplot(year_emis$emissionVector, names = year_emis$yearVector,
        xlab = "Year", ylab = "Total Emission (tons)",
        main = " Total PM2.5 emission for each of the years 1999, 2002, 2005, and 2008")

#Save plot as png file
dev.copy(png,'plot1.png')
dev.off()