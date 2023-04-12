

#########################################################
###This is a sample of how to bring in ACS data with R###
#########################################################

###disable scientific notation###
options(scipen = 999)

###load packages###
library(censusapi)
library(data.table)
#library(xlsx)
data.table::setDTthreads(1)


##########################
###pull from census api###
##########################

### more info on censusapi package can be found here: https://www.hrecht.com/censusapi/articles/getting-started.html ###

###MT's API key###
mycensuskey <-"d740d5cb339b9e2783e17bf506bd4514a1ce205d"

##MT: according to the vignette, you can set your key in the environment, and then not have to pass it with each function
## This is totally unecessary with these functions as currently written, so I'm commenting them back out
## Add key to .Renviron
# Sys.setenv(CENSUS_KEY = "d740d5cb339b9e2783e17bf506bd4514a1ce205d")
## Reload .Renviron ##MT: I touched to create this file, I don't think you need to reload it if you don't have anything in it already.
# readRenviron("~/.Renviron")
## Check to see that the expected key is output in your R console
# Sys.getenv("CENSUS_KEY")


###########################################
###ZCTA to MODZCTA look-up from USCB SF1###
###########################################
##MT: edited these lines to pick up from the R object files from my file system. This is in lieu of rGBAT libraries that I would have on the R server.
load(file = "~/pCloudDrive/workroom/DOHMH/learning-R/USCB_API/CB_to_ZCTA_2010.rda") 
load(file = "~/pCloudDrive/workroom/DOHMH/learning-R/USCB_API/CB_to_ZCTA_2020.rda")
load(file = "~/pCloudDrive/workroom/DOHMH/learning-R/USCB_API/CT_to_PUMA_NTA_2010.rda")

zc_info.dt2 <- as.data.table(CB_to_ZCTA_2010) ##MT: GC gave me this line to show what needs to be changed to load without the rGBAT library
# zc_info.dt2 <- as.data.table(rGBAT::CB_to_ZCTA_2010) ##MT: the original version of the line above. rGBAT is a library only available on the R server. All lines with it need to have libraries loaded and be edited

setnames(zc_info.dt2,c("JN.ZCTA_10","JN.MODZCTA_10","JN.COMBOZCTA_10","JN.UHFCODE_10"),c("ZCTA","MODZCTA","COMBOZCTA","UHFCODE")) ##MT: change field names -- first c() is current names, 2nd c() is what to change them to.


###############################################################
###load tract to NTA look-up table downloaded from DCP BYTES###
###############################################################

nta_info.dt <- as.data.table(CT_to_PUMA_NTA_2010)
# nta_info.dt <- as.data.table(rGBAT::CT_to_PUMA_NTA_2010) ##MT: commented out in favor of next line with locally loaded table

# temp_names  <- names(nta_info.dt) ##MT: used this to get the c() with the names from nta_info.dt. Then \rv on temp_names to get the list in the c() format. yanked and pasted into this setname command:
setnames(nta_info.dt,c("JN.PUMA_10", "JN.NTA_10", "JN.NTA_NAME_10", "USCB_tract_10", "JN.PUMA_NAME_10"),c("JN.PUMA_10", "JN.NTA_10", "JN.NTA_NAME_10", "USCB_tract", "JN.PUMA_NAME_10")) ##MT: had to do this to remove the _10 from the USCB_tract name. Need that to match the functions further down.

###create look-up table of borough code to FIPS
FIPS.dt <- data.table(boro_code=as.character(1:5),boro_int=1:5,FIPS=c("061","005","047","081","085"), stringsAsFactors = FALSE)


#
##
###
##
#

###specify vector of variables from survey of interest###
#my.vars <- c("B10051I_004E","B10051I_007E","B10051D_004E","B10051D_007E") ##MT: I believe the idea here is that you edit this to select what columns you want here. Then you run one of the functions below. See other code example (Stephanie) for calling functions.) This was probably commented out by GC to ensure it doesn't already have a value when this file is called by the Stephanie code (or any other .R file) since it gets passed to these functions.

##MT: The other values passed to these functions (my.survey and my.vintage) have defaults set in the function call lines below (pretty sure that's what's going on).


extract_by_PUMA <- function(my.vars,my.survey="acs/acs5",my.vintage = 2018){

	api.data_zcta <- rbindlist(lapply(unique(nta_info.dt$JN.PUMA_10), function(x) as.data.table(getCensus(name = my.survey,
		vintage = my.vintage,
		key = mycensuskey,
		vars = my.vars,
		region = paste0("public use microdata area:",x) 
		))))
		
	setnames(api.data_zcta, c("public_use_microdata_area"), c("PUMA_10"))

	setorder(api.data_zcta,ZCTA)

	return(api.data_zcta)

}

extract_by_ZCTA <- function(my.vars,my.survey="acs/acs5",my.vintage = 2018){

	if(my.vintage > 2018){
		api.data_zcta <-  as.data.table(getCensus(name = my.survey,
		vintage = my.vintage,
		key = mycensuskey,
		vars = my.vars,
		regionin = "state:36",
		region = "zip code tabulation area:*"
		))
		
		api.data_zcta <- api.data_zcta[zip_code_tabulation_area %in% zc_info.dt2$ZCTA]
		
	} else{
		api.data_zcta <- rbindlist(lapply(unique(zc_info.dt2[ZCTA != "99999"]$ZCTA), function(x) as.data.table(getCensus(name = my.survey,
		vintage = my.vintage,
		key = mycensuskey,
		vars = my.vars,
		region = paste0("zip code tabulation area:",x) 
		))))
	}
		
	setnames(api.data_zcta, c("zip_code_tabulation_area"), c("ZCTA"))

	setorder(api.data_zcta,ZCTA)

	return(api.data_zcta)

}


extract_by_MODZCTA <- function(my.vars,my.survey="acs/acs5",my.vintage = 2018){

	api.data_zcta <- extract_by_ZCTA(my.vars,my.survey,my.vintage)

	###merge to look-up table###
	api.data_zcta <- merge(api.data_zcta, unique(zc_info.dt2[,c("ZCTA","MODZCTA"),with=FALSE]), by="ZCTA")

	###aggregate by MODZCTA###
	api.data_modzcta <- api.data_zcta[, lapply(.SD, sum), by = MODZCTA, .SDcols = my.vars]

	return(api.data_modzcta)
}

extract_by_UHF <- function(my.vars,my.survey="acs/acs5",my.vintage = 2018){

	api.data_zcta <- extract_by_ZCTA(my.vars,my.survey,my.vintage)

	###merge to look-up table###
	api.data_zcta <- merge(api.data_zcta, unique(zc_info.dt2[,c("ZCTA","UHFCODE"),with=FALSE]), by="ZCTA")

	###aggregate by UHF###
	api.data_uhf <- api.data_zcta[, lapply(.SD, sum), by = UHFCODE, .SDcols = my.vars]
	
	return(api.data_uhf)
}


extract_by_tract <- function(my.vars,my.survey="acs/acs5",my.vintage = 2018){

	api.data_tract <- rbindlist(lapply(FIPS.dt$FIPS, function(x) as.data.table(getCensus(name = my.survey,
	vintage = my.vintage,
	key = mycensuskey,
	vars = my.vars,
	region = "tract:*",
	regionin = paste0("state:36+county:",x)))))

	###produce 11-digit geographic ID###
	api.data_tract[,USCB_tract := paste0(state,county,tract)]

	setorder(api.data_tract,USCB_tract)
	
	return(api.data_tract)
	
}	
	
extract_by_NTA <- function(my.vars,my.survey="acs/acs5",my.vintage = 2018){

	api.data_ct <- extract_by_tract(my.vars,my.survey,my.vintage)

	###merge to look-up table###
	api.data_ct <- merge(api.data_ct, unique(nta_info.dt[,c("USCB_tract", "JN.NTA_10", "JN.NTA_NAME_10"),with=FALSE]), by="USCB_tract")

	###aggregate by NTA###
	api.data_nta <- api.data_ct[, lapply(.SD, sum), by = list(JN.NTA_10,JN.NTA_NAME_10), .SDcols = my.vars]

	setnames(api.data_nta,c("JN.NTA_10", "JN.NTA_NAME_10"),c("NTA_10", "NTA_NAME_10"))
	
	setorder(api.data_nta,NTA_10)
	
	return(api.data_nta)
}	

convert_tract_to_NTA_by_mean <- function(api.data_ct,my.var){

	api.data_ct <- api.data_ct[my.var > 0]

	### merge to look-up table nta_info.dt ###
	api.data_ct <- merge(api.data_ct, unique(nta_info.dt[,c("USCB_tract", "JN.NTA_10", "JN.NTA_NAME_10"),with=FALSE]), by="USCB_tract")

	###aggregate by NTA###
	api.data_nta <- api.data_ct[, lapply(.SD, mean), by = list(JN.NTA_10,JN.NTA_NAME_10), .SDcols = my.var]

	setnames(api.data_nta,c("JN.NTA_10", "JN.NTA_NAME_10"),c("NTA_10", "NTA_NAME_10"))
	
	setorder(api.data_nta,NTA_10)
	
	return(api.data_nta)
}

extract_by_block <- function(my.vars,my.survey="dec/sf1",my.vintage = 2010){

	api.data_cb <- rbindlist(lapply(FIPS.dt$FIPS, function(x) as.data.table(getCensus(name = my.survey,
	vintage = my.vintage,
	key = mycensuskey,
	vars = my.vars,
	region = "block:*",
	regionin = paste0("state:36+county:",x)))))
	


	api.data_cb[,USCB_block := paste0(state,county,tract,block)]	
	
	setorder(api.data_cb,USCB_block)
	
	return(api.data_cb)
}	
	
	
