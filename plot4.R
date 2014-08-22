## Data Exploration by Ron Collins
## assignment #2, plot number 4

## Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

## Read in downloaded zipped RDS files, Unzip file 
rm(SCC_coal,SCC_same,coalUse,desiredSCC,i,j,NEI_temp, NEI_outcome, NEI_SCC, vect_type)


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


# Create logical vector CoalUse for SCC Items which contain coal
SCC$EI.Sector <- as.character(SCC$EI.Sector)
SCC$SCC <- as.character(SCC$SCC)
coalUse <- grepl("Coal", SCC$EI.Sector)

# create logical vector with the SCC numbers for Items using coal
SCC_coal <- SCC[coalUse,]
# Create vector with SCC numbers for Observations with coal
desiredSCC  <- SCC_coal$SCC

# SCC _coal numbers are only for point/nonpoint pollution, all others can be removed.
# Create logical vector for types which are point or nonpoint
NEI$type <- as.character(NEI$type)

vect_type <- (NEI$type == "POINT" | NEI$type == "NONPOINT")
NEI_temp <- NEI[vect_type,]
NEI_temp$SCC <- as.character(NEI_temp$SCC)


# Remove SCC observations that do not contain coal
NEI_SCC <- ((NEI_temp$SCC >= "10100101" & NEI_temp$SCC <= "10500200") | (NEI_temp$SCC >= "2101001000" & NEI_temp$SCC <= "2199003000"))

# create new dataframe with SCC numbers associated with coal
NEI_outcome <- NEI_temp[NEI_SCC,]

#define logical vector SCC_same
SCC_same <- logical(0)

# Outer loop to go through dataframe NEI
for( i in 1:length(NEI_outcome[,1])) {
    SCC_same[i] <- FALSE
    # Inner loop to compare SCC value from Dataframe NEI SCC variable.
    # If the two SCC falues are the same, assign True to logical vector, If not assign false
    for (j in 1:length(desiredSCC)){
        if ( NEI_outcome[i,2] == desiredSCC[j]){
            SCC_same[i] <- TRUE
        }
    }
        
}
NEI_Emis <- NEI_outcome[SCC_same,]

# Open graphics device
windows()

g <- ggplot(NEI_Emis, aes(year,Emissions))
p <- g +  ggtitle("Change in emissions from coal combustion between 1999 and 2008") + geom_bar(stat = "identity") +  ylab("Emissions from coal combustion-related sources (tons)")

print(p)

#Save plot as png file
dev.copy(png,'plot4.png')
dev.off()