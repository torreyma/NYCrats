## rats-vs-garbage.R
# Last modified: 2023-04-17 10:54
## A map of DSNY garbage tonnage vs rat complaints
library(sf)
library(ggplot2)
library(data.table)
data.table::setDTthreads(3)

local_data_dir <- file.path(Sys.getenv('HOME'),"DOHMH-local/rats-by-NTA_data") ## This is where I store my large data files on my local system
data_dir <- file.path(Sys.getenv('HOME'),"git-local-repos/torreyma/NYCrats/data") ## This is the data directory of small data in git repo
code_dir <- file.path(Sys.getenv('HOME'),"git-local-repos/torreyma/NYCrats/older-stuff")
#
#
#
#
#### First section: Set up DSNY district geography ############################

DSNY_shape_file  <- "geo_export_002fcdb0-2abc-4a56-89f1-f7b0e7e8baea.shp" 
DSNY_path <- file.path(data_dir,"DSNY-Districts")
DSNY_shape <- st_read(file.path(DSNY_path,DSNY_shape_file), stringsAsFactors = FALSE) ## With path set correctly, load DSNY districts as an st object



allyears_DSNY_file <- file.path(local_data_dir,"DSNY_Monthly_Tonnage_Data.csv")
DSNY_Monthly_Tonnage.dt <- read.csv(allyears_DSNY_file) ## read in DSNY monthly tonnage from all years
## data.tabley way to subset only 2022 rows (%like% is supposed to match string contained in):
DSNY_2020.dt <- DSNY_Monthly_Tonnage.dt[DSNY_Monthly_Tonnage.dt$MONTH %like% "2020", ] 
rm(DSNY_Monthly_Tonnage.dt) ## Delete object we aren't using anymore
invisible(gc()) ## flush memory
write.csv(DSNY_2020.dt, file = file.path(data_dir,"2020-DSNY_Monthly_Tonnage.csv"))
##
## Then, you can probably do some kind of merge, like this:
## MFI_by_NTA <- merge(NYC_NTA_shape, MFI_NTA_2021.dt, by.x="NTACode", by.y="NTA_10")
## DSNY didn't make it super easy, but you can probably match on districtco -- which I think is "district code"
## districtco looks like it's borough code concatenated with disctrict id. So that's easy enough.
## You also have to compress the months together.

##############################################################################
#
#
#
#
#
### Second Section: Add 2020 rat complaint data points section ###############


#### section to create a smaller 2020 rats data file:
	## Since this section ends with saved csv files, you can skip it if you have already done it and are just messing with other parts of the code
	allyears_rats_file <- file.path(local_data_dir,"Rodent_Inspection.csv") ## This is the giant csv file as download from NYC opendata
	ratpoints.dt <- read.csv(allyears_rats_file) ## takes a few minutes to load this giant file
	## data.tabley way to subset only 2020 rows (%like% is supposed to match string contained in):
	subset_ratpoints.dt <- ratpoints.dt[ratpoints.dt$INSPECTION_DATE %like% "2020", ] 
	rm(ratpoints.dt) ## get rid of this giant memory hog
	invisible(gc()) ## flush memory
	subset_ratpoints.dt <- subset_ratpoints.dt[subset_ratpoints.dt$INSPECTION_TYPE %like% "Initial", ] # keep only rows that have 'Initial' INSPECTION_TYPE
	## HERE: you might want to pull out only rows that have 'rat activity' in RESULT -- or make that another data set for mapping -- so you can show complaints vs actual rats (maybe some neighborhoods are just complainers)
	subset_ratpoints.dt <- subset_ratpoints.dt[!with(subset_ratpoints.dt,is.na(LATITUDE)| is.na(LONGITUDE)), ] # keep only rows that don't have NA in lat or long
	ratpoints_complaints.dt <- subset_ratpoints.dt[!with(subset_ratpoints.dt,LATITUDE==0 | LONGITUDE==0), ] # keep only rows that don't have 0 in lat or long
	rm(subset_ratpoints.dt) ## get rid of this large memory hog
	invisible(gc()) ## flush memory
	write.csv(ratpoints_complaints.dt, file = file.path(data_dir,"2020-Rodent_Complaints.csv"))
	ratpoints_ractivity.dt <- ratpoints_complaints.dt[ratpoints_complaints.dt$RESULT %like% "Activity", ] 
	write.csv(ratpoints_ractivity.dt, file = file.path(data_dir,"2020-Rodent_Activity.csv"))
	## Should really save these two as shapefiles too.
	rm(ratpoints_complaints.dt)
	rm(ratpoints_ractivity.dt)
	invisible(gc()) ## flush memory

## Read in rat data csv as spatial -- big file, takes a long time to read, even after the R prompt comes back
rats_complaintsfile <- file.path(data_dir,"2020-Rodent_Complaints.csv")
rats_complaintslayer <- "2020-Rodent_Complaints"
ratcomplaints.sf <- sf::st_read(rats_complaintsfile, rats_complaintslayer, stringsAsFactors = F, quiet=T, options=c("X_POSSIBLE_NAMES=LONGITUDE","Y_POSSIBLE_NAMES=LATITUDE"), crs = 4979)
rats_activityfile <- file.path(data_dir,"2020-Rodent_Activity.csv")
rats_activitylayer <- "2020-Rodent_Activity"
ratactivity.sf <- sf::st_read(rats_activityfile, rats_activitylayer, stringsAsFactors = F, quiet=T, options=c("X_POSSIBLE_NAMES=LONGITUDE","Y_POSSIBLE_NAMES=LATITUDE"), crs = 4979)


##############################################################################
#
#
#
#
#
### Plot section: put all the layers on top of eachother #####################

## pull the MFI column from the attributes of MFI_by_NTA and order it so you can see what the range looks like for the breaks
# MFI_order <- setorder(as.data.table(MFI_by_NTA$B19113_001E)) 
## Plotting with sf::plot
# plot(MFI_by_NTA) ## default of sf's version of plot is to plot every column on a single page. useful!
# plot(MFI_by_NTA["B19113_001E"], col = sf.colors(12, categorical = TRUE), border = NA, axes = TRUE, breaks = br) ## border = NA removes polygon bordors. Could also be a color like 'gray'
## Uncomment if you want to mess with custom breaks:
# br <- c(0, 100, 200, 500, 1000) * 1000 ## set custom breaks value to pass in the plot() command
# Another way to do custom breaks:
# br <- seq(min(fullrange), max(fullrange), by = diff(range(fullrange))/10)
# plot(MFI_by_NTA["B19113_001E"], border = 'gray', axes = TRUE, breaks = br)
# plot(MFI_by_NTA["B19113_001E"], border = "gray", axes = FALSE, breaks = "quantile")


# Trying to plot with ggplot:
ggplot() + 
#	scale_color_brewer(type="seq",palette="Oranges") +
	geom_sf(data = MFI_by_NTA, aes(fill=B19113_001E), show.legend=TRUE, color = NA) +
#	scale_y_continuous(breaks = 34:36) ## but breaks aren't working the way I expect them to
#	+ geom_sf_label(aes(label=B19113_001E)) ## Display MFI value
	## plot rat inspection points
	geom_sf(data = ratcomplaints.sf, size=.01, color="yellow", show.legend=FALSE) +
	geom_sf(data = ratactivity.sf, size=.01, color="red", show.legend=FALSE)
## save plot
# ggsave("ratpoints.png", width=5, height=5)
ggsave("rat-complaint-activity.png")


##############################################################################




