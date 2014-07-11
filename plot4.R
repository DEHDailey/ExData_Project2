## Coursera Exploratory Data Analysis (exdata-004) Course Project 2
## plot4.R: Creates an exploratory plot for Question 4: 

## 4. Across the United States, how have emissions from coal combustion-related
## sources changed from 1999–2008?

## Emissions from coal combustion-related sources remained nearly constant
## between 1999 and 2005.  Between 2005 and 2008, they decreased markedly.


## Code to (a) unzip RDS files from original zip file; (b) read RDS files and 
## save their contents as RData files; (c) read RData objects into the 
## workspace.  This code will do the least work necessary to get the data.frames
## into the workspace.  This code is repeated in all R scripts for this
## assignment because we've been asked to keep the R scripts self-contained; in
## a real-world application, I would either write this code into a separate file
## that is source()d by this one, or create all the plots in a single R script.

zip.filename    <- "exdata-data-NEI_data.zip"
rds.filenames   <- c( NEI = "summarySCC_PM25.rds", SCC = "Source_Classification_Code.rds" )
rdata.filenames <- gsub( 'rds$', 'rdata', rds.filenames )
invis <- sapply( names( rds.filenames ), function( nom ) {  ## Loops over the *name* of the vector of rds.filenames
  if( !( rdata.filenames[ nom ] %in% dir() ) ) {  ## Rdata file does not exist; create it from RDS
    if( !( rds.filenames[ nom ] %in% dir() ) ) {  ## RDS file does not exist; extract it from zip
      message( sprintf( "Unzipping %s from %s...", rds.filenames[ nom ], zip.filename ) )
      unzip( zip.filename, rds.filenames[ nom ] )
    }
    message( sprintf( "Reading %s from disk...", rds.filenames[ nom ] ) )
    myData <- readRDS( rds.filenames[ nom ] )
    assign( nom, myData )
    message( sprintf( "Saving %s data as rdata object...", nom ) )
    save( list=nom, file=rdata.filenames[ nom ] )
  }
  message( sprintf( "Loading %s into workspace...", rdata.filenames[ nom ] ) )
  load( rdata.filenames[ nom ], .GlobalEnv )
} )

## Data loaded; now manipulate the data and create the plot.

message( 'Locating and extracting SCCs related to coal combustion...' )
coalSCCs <- as.character( with( SCC, SCC[ grep( 'Fuel Comb.*Coal$', EI.Sector ) ] ) )
coalNEI <- subset( NEI, SCC %in% coalSCCs )

message( 'Summing up emissions by year...' )
SumByYear <- with( coalNEI, aggregate( Emissions, by=list( year ), FUN = sum ) )
colnames( SumByYear ) <- c( 'Year', 'Total.PM25' )
SumByYear$Total.PM25.K <- with( SumByYear, Total.PM25/1e3 )
SumByYear$Total.PM25.M <- with( SumByYear, Total.PM25/1e6 )



library( ggplot2 )
png( 'plot4.png' )
  qplot( Year, Total.PM25.K, data=SumByYear, geom='line',
         ylim=range( c( 0, SumByYear$Total.PM25.K ) ), ylab='Total.PM25 (thousands of tons)',
         main=expression( PM[2.5] * " emmissons from coal combustion, US, 1999-2008" ) )
dev.off()