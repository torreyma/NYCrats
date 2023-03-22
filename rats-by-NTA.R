## learning-R/rats-by-zip/rats-by-zip.R
# Last modified: 2023-01-30 11:44
## A map of (some Census geography -- maybe ZCTAs?) showing MFI, with 311 rat complaint points plotted on top
library(sf)
library(ggplot2)

data_dir <- file.path(Sys.getenv('HOME'),"DOHMH-local/rats-by-NTA_data") ##MT: Edited to match my local system
code_dir <- file.path(Sys.getenv('HOME'),"pCloudDrive/workroom/DOHMH/learning-R/rats-by-NTA")

### load GC's Census functions; also loads data.table and censusapi libraries ###
source(file.path(code_dir,"USCB_API_functions-MTcomments.R"))
data.table::setDTthreads(3) ## only affects data.table, not R. GC's file above sets to 1 for the DOHMH R server, but at home I can use 3 (leaving one available for other system functions).
#
#
#
#
#### First section: Pull MFI Census data for NTAs and plot ####################
 
## Pull the variables from the ACS 5-year detailed tables and write them to a csv file, uncomment if you need to review these
# acs.detail_vars <- as.data.table(listCensusMetadata(name = "acs/acs5",vintage = 2021,type = "variables")) ## 2021 is the latest ACS 5-year release
# setorder(acs.detail_vars,name) ## sort by name column to get all the variables grouped together
## From above table, I found that B19113_001E = Median Family Income, the variable I want
# write.csv(acs.detail_vars, file = file.path(data_dir,"acs.detail_vars.csv"), row.names=FALSE) ## save this for later, for no particular reason
# rm(acs.detail_vars)
###flush memory###
# invisible(gc())


## Call extract_by_block from GC's functions file to get MFI for 2021 by tract
MFI_tract_2021.dt <- extract_by_tract(c("B19113_001E"),my.survey="acs/acs5",my.vintage = 2021) ##MT: acs/acs5 is the ACS 5-year survey. B19113 is MFI. 2021 is the latest ACS 5-year data.


########Once you get this block working, you should convert it to a callable function, for the sake of practice
	## This deletes negative value rows from MFI, but that removes the geographies too leaving holes in the map, so no good: 
	# MFI_tract_2021.dt <- MFI_tract_2021.dt[MFI_tract_2021.dt$B19113_001E > 0]
	## instead, set the negative values to NA:
	MFI_tract_2021.dt$B19113_001E <- replace(MFI_tract_2021.dt$B19113_001E, MFI_tract_2021.dt$B19113_001E < 0, NA)

	## merge with lookup table for NTA
	tract_nta_lookup <- merge(MFI_tract_2021.dt, unique(nta_info.dt[,c("USCB_tract", "JN.NTA_10", "JN.NTA_NAME_10"),with=FALSE]), by="USCB_tract")

	### aggregate by NTA using mean (as opposed to sum in Gretchen's original) ###
	MFI_NTA_2021.dt <- tract_nta_lookup[, lapply(.SD, mean, na.rm = TRUE), by = list(JN.NTA_10,JN.NTA_NAME_10), .SDcols = "B19113_001E"]
	## na.rm=TRUE is an option for mean() passed as the third variable to lapply. causes mean() to drop NA values instead of returning NA in the calculations.
	## Note that in this data set there are still whole NTAs that remain NA (parks) even after the mean() cacluation
	## Of course mean is no good for MFI really, but this is just proof of concept. If you really wanted to do this, then look at your R file you did for Furman

	setnames(MFI_NTA_2021.dt,c("JN.NTA_10", "JN.NTA_NAME_10"),c("NTA_10", "NTA_NAME_10"))
	
	setorder(MFI_NTA_2021.dt,NTA_10)




## Call convert_tract_to_NTA_by_mean -- struggling to get this to work as a function, implemented above.
# MFI_NTA_2021.dt <- convert_tract_to_NTA_by_mean(MFI_tract_2021.dt,MFI_tract_2021.dt$"B19113_001E") 

## I'm going to merge with two different column names below, but if you wanted to match the NTCode column name for the merge, you can do it like this:
# temp_name <- names(MFI_NTA_2021.dt) ##MT: used this to get the c() with the names from nta_info.dt. Then \rv on temp_names to get the list in the c() format. yanked and pasted into this setname command:
# setnames(MFI_NTA_2021.dt,c("NTA_10", "NTA_NAME_10", "B19113_001E"),c("NTACode", "NTA_NAME_10", "B19113_001E")) ## Make NTACode name match for merge to NYC_NTA_shape


NYC_NTA_file  <- "nynta2010.shp" 
NYC_NTA_path <- file.path(data_dir,"20230126_nynta2010_22a")
NYC_NTA_shape <- st_read(file.path(NYC_NTA_path,NYC_NTA_file), stringsAsFactors = FALSE) ## With path set correctly, load 2010 NTA geographies shapefile
## Tried with 2020 geo for NTAs, but the NTACode didn't match. Rather than sorting that out, I'm just loading the 2010 geography.

MFI_by_NTA <- merge(NYC_NTA_shape, MFI_NTA_2021.dt, by.x="NTACode", by.y="NTA_10") ## merge on the NTA identifier code in both objects -- note: merge data frame INTO sf object, ie sf object has to be listed first in merge command

##############################################################################
#
#
#
#
#
### Second Section: Add 2022 rat complaint data points section ###############


#### section to create a smaller 2022 rats data file:
	## Since this section ends with saved csv files, you can skip it if you are just messing with other parts of the code
	allyears_rats_file <- file.path(data_dir,"Rodent_Inspection.csv") ## This is the giant csv file as download from NYC opendata
	# library(data.table) ## only needed here if you didn't include the USCB file at top of file, next line too:
	# data.table::setDTthreads(3) ## doesn't really seem to do anything
	ratpoints.dt <- read.csv(allyears_rats_file) ## takes a few minutes to load this giant file
	## data.tabley way to subset only 2022 rows (%like% is supposed to match string contained in):
	subset_ratpoints.dt <- ratpoints.dt[ratpoints.dt$INSPECTION_DATE %like% "2022", ] 
	rm(ratpoints.dt) ## get rid of this giant memory hog
	invisible(gc()) ## flush memory
	subset_ratpoints.dt <- subset_ratpoints.dt[subset_ratpoints.dt$INSPECTION_TYPE %like% "Initial", ] # keep only rows that have 'Initial' INSPECTION_TYPE
	## HERE: you might want to pull out only rows that have 'rat activity' in RESULT -- or make that another data set for mapping -- so you can show complaints vs actual rats (maybe some neighborhoods are just complainers)
	subset_ratpoints.dt <- subset_ratpoints.dt[!with(subset_ratpoints.dt,is.na(LATITUDE)| is.na(LONGITUDE)), ] # keep only rows that don't have NA in lat or long
	ratpoints_complaints.dt <- subset_ratpoints.dt[!with(subset_ratpoints.dt,LATITUDE==0 | LONGITUDE==0), ] # keep only rows that don't have 0 in lat or long
	rm(subset_ratpoints.dt) ## get rid of this large memory hog
	invisible(gc()) ## flush memory
	write.csv(ratpoints_complaints.dt, file = file.path(data_dir,"2022-Rodent_Complaints.csv"))
	ratpoints_ractivity.dt <- ratpoints_complaints.dt[ratpoints_complaints.dt$RESULT %like% "Activity", ] 
	write.csv(ratpoints_ractivity.dt, file = file.path(data_dir,"2022-Rodent_Activity.csv"))
	## Should really save these two as shapefiles too.
	rm(ratpoints_complaints.dt)
	rm(ratpoints_ractivity.dt)
	invisible(gc()) ## flush memory

## Read in rat data csv as spatial -- big file, takes a long time to read, even after the R prompt comes back
rats_complaintsfile <- file.path(data_dir,"2022-Rodent_Complaints.csv")
rats_complaintslayer <- "2022-Rodent_Complaints"
ratcomplaints.sf <- sf::st_read(rats_complaintsfile, rats_complaintslayer, stringsAsFactors = F, quiet=T, options=c("X_POSSIBLE_NAMES=LONGITUDE","Y_POSSIBLE_NAMES=LATITUDE"), crs = 4979)
rats_activityfile <- file.path(data_dir,"2022-Rodent_Activity.csv")
rats_activitylayer <- "2022-Rodent_Activity"
ratactivity.sf <- sf::st_read(rats_activityfile, rats_activitylayer, stringsAsFactors = F, quiet=T, options=c("X_POSSIBLE_NAMES=LONGITUDE","Y_POSSIBLE_NAMES=LATITUDE"), crs = 4979)

## Now maybe calculate rat inspections per dollar MFI?

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




