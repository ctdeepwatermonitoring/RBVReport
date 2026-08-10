# script purpose: make summary tables for RBV annual report

setwd("C:/Users/LandryJes/Documents/RBVReport/RBV_Metrics_WQX_2025")
library(dplyr)
library(tidyr)
library(tibble)
library(DBI)
library(odbc)
library(keyring)
library(readxl)


### Setting up file directories to keep track of the downloaded files 
data <- "C:/Users/LandryJes/Documents/RBVReport/RBV_Metrics_WQX_2025/Data/"
reference_tables <- "C:/Users/LandryJes/Documents/RBVReport/RBV_Metrics_WQX_2025/Reference_Tables/"
taxa_QC <- "C:/Users/LandryJes/Documents/RBVReport/RBV_Metrics_WQX_2025/taxa_QC/"
results <- "C:/Users/LandryJes/Documents/RBVReport/RBV_Metrics_WQX_2025/Results/"

#### BEFORE RUNNING SCRIPT ####
### Files to be updated for new sample year: Add to Data File Folder ###
# 1. New RBV Samples File (manually edit file sent by Ecoanalyst to match format from previous year, filter to rbv, rearrange columns, ect.)
# 2. RBV site submission form csv downloaded from survey123 form (filtered down to just the year of interest)


### Files to pull from previous years directory (Copy Entire Reference_Tables Folder)
# 1. Copy "summarystats.csv" file from last WQX metrics folder to append this years stats
# 2. Copy of "Master_Taxa_List.csv"
# 3. Copy of "WQX_Taxon_MasterTaxon_Lookup.csv"
# 4. Copy of "RBV_MasterTaxaList_Lookup.csv" 

### pulling most recent AWX stations
con <- dbConnect(odbc(), 
                 Driver = "MySQL ODBC 9.0 ANSI Driver", 
                 Server = "sdc-epafiling", 
                 Database = "awqx", 
                 Trusted_Connection = "True",
                 uid = key_list("sdc-epafiling")[1,2],
                 pwd = key_get("sdc-epafiling", "readonly_user"))

# Pull full list of stations from awX
site_select <- ("SELECT * FROM awqx.stations;")
awx_stations <- dbGetQuery(con, site_select)
# Disconnect from database 
dbDisconnect(con)

# WQX master taxon file
tmpf <- tempfile()
download.file("https://cdx.epa.gov/wqx/download/DomainValues/Taxon_CSV.zip",tmpf)
conn <- unz(tmpf, "Taxon.csv")
taxon <- read.csv(conn, header = TRUE)
unlink(tmpf)

#### STEP 0: get list of monitoring groups, open this csv file, add org code and coordinators name before running the rest of the script ####
## take old groups file from previous year and add if there are any new additions in the output of this new file
## Save the final file as "groups_year.csv" and add to reference_tables file
survey_output <- read.csv(paste0(data,"RBV_Site_Submission_Form_0.csv"))
groups <- unique(survey_output$Monitoring.Organization.)
groups <- as.data.frame(groups)
write.csv(groups, "groups.csv", row.names = FALSE)



### reading in files needed for analysis
master_taxa_list <- read_xlsx(paste0(reference_tables, "Macroinvertebrate_MasterTaxa_Synonyms_071526.xlsx")) # master taxa list
wqx_taxa_list <- read.csv(paste0(reference_tables,"WQXTaxon_MasterTaxon_Lookup.csv"))
new_samples <- read_xlsx(paste0(data, "RBV_2025_Samples.xlsx"))
survey_output <- read.csv(paste0(data, "RBV_Site_Submission_Form_0.csv"))
grp_id <- read.csv(paste0(reference_tables, "groups2025.csv"))
summary <- read.csv(paste0(reference_tables, "summarystats.csv"), check.names= FALSE)

### first reformatting taxa names to WQX format (making a new column so we can keep the "TAXON_NAME" original column in tact)
new_samples$finalID <- new_samples$TAXON_NAME

#Reformatting taxon name to remove sp. and gr. since that is how it is imported to master taxa 
new_samples$finalID <- gsub(" sp.", "", new_samples$finalID, fixed = TRUE)
new_samples$finalID <- gsub(" gr.", " group", new_samples$finalID, fixed = TRUE)



#### STEP 1: compare new samples with master taxa list ####
new_taxa <- new_samples[nchar(new_samples$DEEPTaxaID) > 4 ,] # subsetting anything over 4 characters to find the ones not on DEEP taxa ID list or those with notes attached 

#confirming the not on list taxa 
new_taxa_confirm <- anti_join(new_taxa, master_taxa_list, by = "finalID")

#reformatting new taxa to match master taxa list
new_taxa_reformat <- master_taxa_list[FALSE, ]
new_taxa_reformat$DEEPTaxaID <- as.character(new_taxa_reformat$DEEPTaxaID)
new_taxa_reformat$SUBTRIBE <- as.character(new_taxa_reformat$SUBTRIBE)
new_taxa_reformat <- bind_rows(new_taxa_reformat, new_taxa_confirm)
new_taxa_reformat <- new_taxa_reformat[,1:32]

#Exporting the new taxa files, raw and reformatted 
write.csv(new_taxa_confirm, paste0(taxa_QC, "newtaxa.csv"), row.names = FALSE, na ="")
write.csv(new_taxa_reformat, paste0(taxa_QC, "newtaxa_formatted.csv"), row.names = FALSE, na = "")

#### Review these and create a few new files 
##### This year in the DEEP taxaID column they added parentheses around the taxa name if it is a synonym, so this new taxa list has those not on list and synonym 
# 1. "RBVnewtaxa_formatted2025.csv": create copy of newtaxa_formatted.csv and only keep brand new taxa, we have no record of the final ID or any synonyms, so it will be added to master taxa database
# 2. "NotonList_Merge.csv": create a copy of newtaxa and rename assign all taxa in this file a DEEP Taxa ID by looking through synonyms and creating new taxa
#     change "finalID" to "finalID_supplied" to keep a record of the original Ecoanalyst supplied name, and create a new finalID_mew column with the taxa name we use based on the mastertaxalist
#     this allows for a record of both, the name we want will be submitted to WQX and we will have the name given by Ecoanalyst in supplied column 


# Reading in the new "NotonList_Merge" file with newly edited/ created DEEP TaxaIDs and finalID_new column 
# Merging with our datafile and changing the the final ID and DEEP Taxa ID to the newly checked ones we just made in the merge file
notonlist <- read.csv(paste0(taxa_QC, "NotonList_Merge.csv"))
#removing the duplicates from the list 
notonlist <- unique(notonlist)
new_samples <- merge(new_samples, notonlist, by.x = "finalID", by.y = "finalID_supplied", all.x=TRUE)

# replacing the Final ID with the new ones 
new_samples <- new_samples %>%
  mutate(finalID = coalesce(finalID_new, finalID))

# replacing the DEEP taxaID with the new ones
new_samples$DEEPTaxaID.y <- as.character(new_samples$DEEPTaxaID.y)
new_samples <- new_samples %>%
  mutate(DEEPTaxaID = coalesce(DEEPTaxaID.y, DEEPTaxaID.x))



#### STEP 2: Comparing our data with the taxon in WQX to determine unmatching taxa (likely due to outdated names) ####
data_taxon <- new_samples
colnames(data_taxon)[colnames(data_taxon) == "finalID"] <- "Name"
data_taxon <- anti_join(data_taxon, taxon, by = "Name")
data_taxon <- data_taxon[!duplicated(data_taxon[c("Name")]), ]# names not present in WQX due to name changes
data_taxon <- data_taxon[!grepl("None", data_taxon$Name),] ## removing the none present in sample
write.csv(data_taxon, paste0(taxa_QC, "outdated_names.csv"), na =" ", row.names= FALSE)

### open outdated names file and create a file called "updated_names.csv" with a column for the current name, a column for "WQX Updated Name", check in WQX domain table and ITIS and choose valid name, include ITIS link ###
updatednames <- read.csv(paste0(taxa_QC, "RBVupdated_names2025.csv"))
new_samples <- merge(new_samples, updatednames, by.x = "finalID", by.y = "Name", all.x=TRUE)
new_samples <- new_samples %>%
  mutate(finalID = coalesce(WQX.Updated.Name, finalID))

### at this point all taxa should have an assigned DEEP taxa ID and the 'finalID' column should only contain names that are valid in WQX and ITIS
# all new taxa and updated names should be changed in the Master Taxa File.



#### STEP 3: Open RBV_MasterTaxaList_Lookup.csv and all newly created DEEP TaxaIDs (aka those in the "RBVnewtaxa_formatted2025.csv") with RBV category before continuing ####
rbv_taxa_list <- read.csv(paste0(reference_tables, "RBV_MasterTaxaList_Lookup.csv"))
# This lookup table has duplicate DEEP TaxaIDs because when it was made, they added the synonym in the final ID column
# ideally, this should be reworked so there is one final ID & DEEP Taxa ID and those match the master taxa list  (or even better, a database for the master taxa list that has a column for RBV Category)
# But for now, just removing the duplicated Taxa IDs so I can merge without adding additional samples
rbv_taxa_list <- rbv_taxa_list[!duplicated(rbv_taxa_list$DEEPTaxaID), ]
rbv_taxa_list$DEEPTaxaID <- as.character(rbv_taxa_list$DEEPTaxaID)


### once we have the updated master taxa list, with our new taxa added and outdated names updated, we can pull in that new file and merge to our samples
# though this process was already done above, so not really necessary, but since I assigned temporary DEEP taxa IDs to the ones on the "Not on List" file and made judgement calls to the outdated names, we need to verify all decisions made match the final master taxa list before uploading 
# so it might be worth it to do a final merge with the updated and verifies master taxa list before data is submitted
# merged_list <- merge(new_samples, master_taxa_list, by = "DEEPTaxaID")


# once RBV category is added, merging rbv taxa list with our samples
# first anti join to check that all DEEP Taxa IDs in the sample have a matching rbv category
rbv_list_confirm <- anti_join(new_samples, rbv_taxa_list, by = "DEEPTaxaID")
# add any from this list to the RBV Master Look up Table, then re-run

# merging with our sample dataframe and AWX stations to add the mun name
merged_list <- merge(new_samples, rbv_taxa_list, by = "DEEPTaxaID", all.x = TRUE)
merged_list <- merge(merged_list, awx_stations, by = "staSeq")

##### making rbv final id category: this section will make the presence/absense table for the RBV report ######
rbv_cat_table <- merged_list
rbv_cat_table <- rbv_cat_table[,c("CTDEEP.LabID", "staSeq", "locationName", "munName", "DATE_COL", "CLASS", "SUBCLASS", "ORDER", "SUBORDER", "FAMILY", "GENUS", "finalID.x", "RBVCategory")]

# Checking to see what is being left out of the subset below
rbv_excluded <- subset(rbv_cat_table, 
                       !RBVCategory %in% c("MOST WANTED", "MODERATELY WANTED", "LEAST WANTED") &
                         !FAMILY %in% c("Tipulidae", "Elmidae", "Baetidae", "Athericidae", "Planariidae") &
                         CLASS != "Bivalvia")

# subsetting based on the field datasheet categories
rbv_cat_table <- subset(rbv_cat_table, 
       rbv_cat_table$RBVCategory == "MOST WANTED" | 
       rbv_cat_table$RBVCategory == "MODERATELY WANTED" |
       rbv_cat_table$RBVCategory == "LEAST WANTED" |
         rbv_cat_table$FAMILY == "Tipulidae" | #rbv field sheet other family
         rbv_cat_table$FAMILY == "Elmidae" |
         rbv_cat_table$FAMILY == "Baetidae" |
         rbv_cat_table$FAMILY == "Athericidae" |
         rbv_cat_table$FAMILY == "Planariidae" |
         rbv_cat_table$CLASS == "Bivalvia") #rbv field sheet other class

rbv_cat_table$RBVFinalIDCat <- NA #make empty column
#most wanted
rbv_cat_table$RBVFinalIDCat <- ifelse(rbv_cat_table$RBVCategory == "MOST WANTED", rbv_cat_table$FAMILY, rbv_cat_table$RBVFinalIDCat) #all most wanted are to family with 2 exceptions
rbv_cat_table$RBVFinalIDCat <- ifelse(!is.na(rbv_cat_table$GENUS) & rbv_cat_table$GENUS == "Epeorus", rbv_cat_table$GENUS, rbv_cat_table$RBVFinalIDCat) #if genus = epeorus, replace with epeorus, if not use existing value
rbv_cat_table$RBVFinalIDCat <- ifelse(!is.na(rbv_cat_table$GENUS) & rbv_cat_table$GENUS == "Drunella", rbv_cat_table$GENUS, rbv_cat_table$RBVFinalIDCat) #exception to most wanted
#moderately wanted
rbv_cat_table$RBVFinalIDCat <- ifelse(rbv_cat_table$RBVCategory == "MODERATELY WANTED", rbv_cat_table$FAMILY, rbv_cat_table$RBVFinalIDCat) #most moderates are family
rbv_cat_table$RBVFinalIDCat <- ifelse(!is.na(rbv_cat_table$SUBORDER) & rbv_cat_table$SUBORDER == "Anisoptera", rbv_cat_table$SUBORDER, rbv_cat_table$RBVFinalIDCat) #exception to moderate
rbv_cat_table$RBVFinalIDCat <- ifelse(!is.na(rbv_cat_table$SUBORDER) & rbv_cat_table$SUBORDER == "Zygoptera", rbv_cat_table$SUBORDER, rbv_cat_table$RBVFinalIDCat) #exception to moderate
#least wanted
rbv_cat_table$RBVFinalIDCat <- ifelse(!is.na(rbv_cat_table$ORDER) & rbv_cat_table$ORDER == "Amphipoda", rbv_cat_table$ORDER, rbv_cat_table$RBVFinalIDCat) #least wanted below, taxa id level varies a lot
rbv_cat_table$RBVFinalIDCat <- ifelse(!is.na(rbv_cat_table$ORDER) & rbv_cat_table$ORDER == "Isopoda", rbv_cat_table$ORDER, rbv_cat_table$RBVFinalIDCat)
rbv_cat_table$RBVFinalIDCat <- ifelse(!is.na(rbv_cat_table$SUBCLASS) & rbv_cat_table$SUBCLASS == "Hirudinea", rbv_cat_table$SUBCLASS, rbv_cat_table$RBVFinalIDCat)
rbv_cat_table$RBVFinalIDCat <- ifelse(!is.na(rbv_cat_table$SUBCLASS) & rbv_cat_table$SUBCLASS == "Hirudinida", "Hirudinea", rbv_cat_table$RBVFinalIDCat) #synonym for hirudinea
rbv_cat_table$RBVFinalIDCat <- ifelse(!is.na(rbv_cat_table$SUBCLASS) & rbv_cat_table$SUBCLASS == "Oligochaeta", rbv_cat_table$SUBCLASS, rbv_cat_table$RBVFinalIDCat)
rbv_cat_table$RBVFinalIDCat <- ifelse(!is.na(rbv_cat_table$CLASS) & rbv_cat_table$CLASS == "Gastropoda", rbv_cat_table$CLASS, rbv_cat_table$RBVFinalIDCat)
rbv_cat_table$RBVFinalIDCat <- ifelse(!is.na(rbv_cat_table$FAMILY) & rbv_cat_table$FAMILY == "Chironomidae", rbv_cat_table$FAMILY, rbv_cat_table$RBVFinalIDCat)
rbv_cat_table$RBVFinalIDCat <- ifelse(!is.na(rbv_cat_table$FAMILY) & rbv_cat_table$FAMILY == "Simuliidae", rbv_cat_table$FAMILY, rbv_cat_table$RBVFinalIDCat)
#other subset that are present on the field sheet
rbv_cat_table$RBVFinalIDCat <- ifelse(!is.na(rbv_cat_table$FAMILY) & rbv_cat_table$FAMILY == "Tipulidae", rbv_cat_table$FAMILY, rbv_cat_table$RBVFinalIDCat) #below are the subset of 'other' on rbv field sheet
rbv_cat_table$RBVFinalIDCat <- ifelse(!is.na(rbv_cat_table$FAMILY) & rbv_cat_table$FAMILY == "Elmidae", rbv_cat_table$FAMILY, rbv_cat_table$RBVFinalIDCat) 
rbv_cat_table$RBVFinalIDCat <- ifelse(!is.na(rbv_cat_table$FAMILY) & rbv_cat_table$FAMILY == "Baetidae", rbv_cat_table$FAMILY, rbv_cat_table$RBVFinalIDCat) 
rbv_cat_table$RBVFinalIDCat <- ifelse(!is.na(rbv_cat_table$FAMILY) & rbv_cat_table$FAMILY == "Athericidae", rbv_cat_table$FAMILY, rbv_cat_table$RBVFinalIDCat) 
rbv_cat_table$RBVFinalIDCat <- ifelse(!is.na(rbv_cat_table$FAMILY) & rbv_cat_table$FAMILY == "Planariidae", rbv_cat_table$FAMILY, rbv_cat_table$RBVFinalIDCat) 
rbv_cat_table$RBVFinalIDCat <- ifelse(!is.na(rbv_cat_table$CLASS) & rbv_cat_table$CLASS == "Bivalvia", rbv_cat_table$CLASS, rbv_cat_table$RBVFinalIDCat)


# open the rbv_cat_table dataframe and check for any NAs in the RBV FinalIDCat column, if there are any NAs, manually assign category below
# left with just Plecoptera, since we do not have a family level ID and only order level, I am removing it 
rbv_cat_table <- subset(rbv_cat_table, !is.na(RBVFinalIDCat))
rbv_cat_table <- rbv_cat_table[!duplicated(rbv_cat_table[c("CTDEEP.LabID", "RBVFinalIDCat")]), ] #counting one finalid cat per sample

######presence/absence table for report######
rbv_cat_pivot <- rbv_cat_table
rbv_cat_pivot <- rbv_cat_pivot[,c("CTDEEP.LabID","staSeq","locationName", "munName", "DATE_COL", "RBVFinalIDCat")] 
rbv_cat_pivot$Stream_Mun <- paste(rbv_cat_pivot$locationName, " ", "(", rbv_cat_pivot$munName, ")", sep = "")
rbv_cat_pivot$presence <- "X" #trying something funky
rbvidlist <- rbv_cat_pivot[!duplicated(rbv_cat_pivot[c("RBVFinalIDCat")]), ]
rbvidlist$RBVFinalIDCat #print cat columns
target_cats <- c(
  "Drunella", "Isonychiidae", "Epeorus", #version to compare to site metrics for QA purposes
  "Peltoperlidae", "Perlidae", "Pteronarcyidae", 
  "Perlodidae","Chloroperlidae","Capniidae", 
  "Leuctridae", "Taeniopterygidae", "Nemouridae", 
  "Glossosomatidae", "Apataniidae", "Rhyacophilidae",
  "Brachycentridae", "Lepidostomatidae", "Hydropsychidae",
  "Philopotamidae", "Heptageniidae", "Psephenidae", 
  "Corydalidae", "Anisoptera", "Zygoptera",
  "Amphipoda", "Isopoda", "Hirudinea", "Chironimidae",
  "Simuliidae", "Gastropoda", "Oligochaeta",
  "Tipulidae", "Elmidae", "Baetidae", 
  "Athericidae", "Planariidae", "Bivalvia")
rbv_cat_pivot <- rbv_cat_pivot %>%
  filter(RBVFinalIDCat %in% target_cats) %>%
  mutate(RBVFinalIDCat = factor(RBVFinalIDCat, levels = target_cats)) %>% # Enforces all 35 levels
  pivot_wider(
    names_from = RBVFinalIDCat, 
    values_from = presence,
    names_expand = TRUE 
  )

rbv_cat_pivot <- rbv_cat_pivot[c("CTDEEP.LabID","staSeq","Stream_Mun","DATE_COL", #just to look at, not write
                "Drunella", "Isonychiidae", "Epeorus", #version to compare to site metrics for QA purposes
                "Peltoperlidae", "Perlidae", "Pteronarcyidae", 
                "Perlodidae","Chloroperlidae","Capniidae", 
                "Leuctridae", "Taeniopterygidae", "Nemouridae", 
                "Glossosomatidae", "Apataniidae", "Rhyacophilidae",
                "Brachycentridae", "Lepidostomatidae", "Hydropsychidae",
                "Philopotamidae", "Heptageniidae", "Psephenidae", 
                "Corydalidae", "Anisoptera", "Zygoptera",
                "Amphipoda", "Isopoda", "Hirudinea", "Chironimidae",
                "Simuliidae", "Gastropoda", "Oligochaeta",
                 "Tipulidae", "Elmidae", "Baetidae", 
                "Athericidae", "Planariidae", "Bivalvia")] 
rbv_cat_unformatted <- rbv_cat_pivot #to check

#need to summarize misc small stonefly familes for report
rbv_cat_pivot$MiscSmallStoneflies <- NA
taxa_columns <- rbv_cat_pivot[, 11:16]
rbv_cat_pivot$MiscSmallStoneflies <- ifelse(rowSums(taxa_columns == "X", na.rm = TRUE) > 0, "X", NA) # Check if any of the columns contain "X" using rowSums

#fixing names
#colnames(volmon_grp)[colnames(volmon_grp) == "Field.ID"] <- "CTDEEP.LabID" 
#rbv_cat_pivot <- merge(rbv_cat_pivot, volmon_grp, by = "CTDEEP.LabID") #adding org name to sample row
rbv_cat_pivot <- rbv_cat_pivot[c("Stream_Mun","staSeq","DATE_COL", 
                                 "Drunella", "Isonychiidae", "Epeorus", 
                                 "Peltoperlidae", "Perlidae", "Pteronarcyidae", 
                                 "MiscSmallStoneflies", #six fams replaced with this category for report 
                                 "Glossosomatidae", "Apataniidae", "Rhyacophilidae",
                                 "Brachycentridae", "Lepidostomatidae", "Hydropsychidae",
                                 "Philopotamidae", "Heptageniidae", "Psephenidae", 
                                 "Corydalidae", "Anisoptera", "Zygoptera",
                                 "Amphipoda", "Isopoda", "Hirudinea", "Chironimidae",
                                 "Simuliidae", "Gastropoda", "Oligochaeta",
                                  "Tipulidae", "Elmidae", "Baetidae", 
                                 "Athericidae", "Planariidae", "Bivalvia")]
write.csv(rbv_cat_pivot, paste0(results, "presence_absence_report.csv"), row.names = FALSE, na = " ")

######step 5: Calculating the number of most wanted per site #####
all_sites <- merged_list %>% #reference list of all unique site + date combos
  group_by(CTDEEP.LabID, staSeq, DATE_COL) %>%
  summarize() 

sites_with_most_wanted <- merged_list %>% #count of most wanted per sample
  filter(RBVCategory == "MOST WANTED") %>%
  group_by(CTDEEP.LabID, staSeq, DATE_COL) %>%
  distinct(FAMILY) %>% #there are duplicate family columns ie x and y, probably unnecessary joining
  summarize(RBV_most_wanted_count = n()) 

sites_with_no_most_wanted <- all_sites %>% 
  anti_join(sites_with_most_wanted, by = c("CTDEEP.LabID", "staSeq", "DATE_COL")) %>% #looking for what isnt in most wanted but is in all sites
  mutate(RBV_most_wanted_count = 0)  #makes a new column with value 0 since they had no most wanted

#summary table with most wanted count by site
final_site_metrics <- bind_rows(sites_with_most_wanted, sites_with_no_most_wanted)
final_site_metrics <- merge(final_site_metrics, awx_stations, by = "staSeq")
write.csv(final_site_metrics, paste0(results, "metrics_labid.csv"), row.names = FALSE)
final_site_metrics <- final_site_metrics[c("CTDEEP.LabID", "staSeq", "locationName", "munName", "DATE_COL", "RBV_most_wanted_count")]
colnames(final_site_metrics)[colnames(final_site_metrics) == "munName"] <- "Municipality"
colnames(final_site_metrics)[colnames(final_site_metrics) == "locationName"] <- "WaterbodyName"
write.csv(final_site_metrics, paste0(results, "RBV_Summary_2025.csv"), row.names = FALSE)

#filtered rbv id per site, # of unique taxa should match summary count by site
#e.g. if a site had a count of 4, 4 unique rbv category taxa from that site should be found in this table
#alternate way of viewing presence/absence table
most_wanted_by_site <- merged_list
most_wanted_by_site <- subset(most_wanted_by_site, most_wanted_by_site$RBVCategory == "MOST WANTED")
most_wanted_by_site <- most_wanted_by_site[c("CTDEEP.LabID", "staSeq", "locationName", "DATE_COL", "FAMILY", "finalID.x")]
most_wanted_by_site <- unique(most_wanted_by_site)
most_wanted_by_site <- most_wanted_by_site[!duplicated(most_wanted_by_site[c("staSeq", "FAMILY", "DATE_COL")]), ]
colnames(most_wanted_by_site)[colnames(most_wanted_by_site) == "FAMILY.x"] <- "Family"
colnames(most_wanted_by_site)[colnames(most_wanted_by_site) == "finalID.x"] <- "finalID"
write.csv(most_wanted_by_site, paste0(results, "RBV_MostWantedBySite_2025.csv"), row.names = FALSE)

####checking S123 output with summary table####
colnames(survey_output)[10] <- "staSeq"
survey_check <- merge(survey_output, final_site_metrics, by = "staSeq", all.x = TRUE)



##### putting org name with count by site #####
labid <- read.csv(paste0(results, "metrics_labid.csv"))
colnames(labid)[colnames(labid) == "CTDEEP.LabID"] <- "sampleID"
labid <- merge(survey_output, labid, by = "sampleID")
groups <- read.csv(paste0(reference_tables, "groups2025.csv"))
labid$Monitoring.Organization. <- ifelse(grepl("other", labid$Monitoring.Organization., ignore.case = TRUE), NA, labid$Monitoring.Organization.)
labid <- labid %>%
  mutate(Monitoring.Organization. = coalesce(Monitoring.Organization., Specify.other.))
colnames(groups)[1] <- ("Monitoring.Organization.")
labid <- merge(labid, groups, by = "Monitoring.Organization.", all.x=TRUE)
colnames(labid)[colnames(labid) == "staSeq.x"] <- "staSeq"
labid <- labid[c("locationName", "staSeq", "code", "DATE_COL", "RBV_most_wanted_count")]
write.csv(labid, paste0(results, "summarybysite_code2025.csv"), row.names = FALSE)

#####For Summary Stats file used in report#####
stats <- data.frame(matrix(ncol = 5, nrow = 1))
colnames(stats) <-c("# RBV Samples Submitted", "# Monitoring Stations", "# Streams Monitored", "# Samples w/ 4+ 'Most Wanted' Types", "% of Samples w/ 4+  'Most Wanted' Types")
stats$'# RBV Samples Submitted' <- nrow(labid)
stats$`# Monitoring Stations` <- length(unique(labid$staSeq))
stats$`# Streams Monitored` <- length(unique(labid$locationName))
stats$`# Samples w/ 4+ 'Most Wanted' Types` <- sum(labid$RBV_most_wanted_count >= 4)
stats$`% of Samples w/ 4+  'Most Wanted' Types` <- paste0(round(sum(labid$RBV_most_wanted_count >= 4)/ nrow(labid) *100), "%")
# rearranging dataframe so I can merge
stats_t <- as.data.frame(t(stats))
stats_t <- rownames_to_column(stats_t, var = "Category")
colnames(stats_t)[2] <- "2025"
summarystats <- merge(summary, stats_t, by = "Category", sort = FALSE)
write.csv(summarystats, paste0(results, "summarystats2025.csv"), row.names = FALSE)


###### wqx stuff #################################################################

#stations for wqx
wqx_stations <- left_join(all_sites, awx_stations, by = "staSeq")
colnames(wqx_stations)
wqx_stations <- wqx_stations[c("staSeq", "locationName", "ylat", "xlong")]
write.csv(wqx_stations, paste0(results, "WQX_Upload/upload_stations.csv"), row.names = FALSE)

####Site info for RVB sites, formatted file for the report###
sites <- merge(wqx_stations, awx_stations, by = "staSeq")
sites <- sites[c("locationName.x","staSeq", "munName", "locationDescription","ylat.x", "xlong.x")]
names(sites) <- c("Waterbody Name", "Station ID", "Municipality", "Description", "Latitude", "Longitude")
write.csv(sites, paste0(results, "sitesforreport2025.csv"), row.names = FALSE)

####lazy from old version
new_sites <- read.csv(paste0(results, "RBV_Summary_2025.csv")) #stationid and metrics results
sampleinfo <- read_xlsx(paste0(data, "RBV_2025_Samples.xlsx")) 
wqx_master_lookup <- read.csv(paste0(reference_tables,"WQXTaxon_MasterTaxon_Lookup.csv"))
rbv_cat <- read.csv(paste0(reference_tables,"RBV_MasterTaxaList_Lookup.csv")) #manually added in new spp

#####metrics upload file#####
upload_metrics <- merge(sampleinfo, new_sites, by = "CTDEEP.LabID") #to preserve unique samples
#need to make: activity ID (accession# + _TaxaMostW)
#activity metric comment (= deep field id: / lab id:)
upload_metrics <- upload_metrics[c("staSeq.x", "CTDEEP.LabID", "DATE_COL.x", "RBV_most_wanted_count", "LAB_SAMPLE_ID" )]
upload_metrics$'Activity ID' <- paste((gsub("[^0-9]+", "", upload_metrics$CTDEEP.LabID)), '_TaxaMostW') #pulling out numeric values and adding text
upload_metrics$'Activity Metric Comment' <- paste('DEEP Field ID: ', upload_metrics$CTDEEP.LabID, ' / Taxonomy Lab ID: ', upload_metrics$LAB_SAMPLE_ID)
upload_metrics$'Activity Metric Value' <- upload_metrics$RBV_most_wanted_count #there are duplicated value and score columns
colnames(upload_metrics)[colnames(upload_metrics) == "staSeq.x"] <- 'Monitoring Location ID'
colnames(upload_metrics)[colnames(upload_metrics) == "DATE_COL.x"] <- 'Activity Start Date'
colnames(upload_metrics)[colnames(upload_metrics) == "RBV_most_wanted_count"] <- 'Activity Metric Score'
upload_metrics <- upload_metrics[c("Monitoring Location ID", "Activity ID", "Activity Start Date", 
                                   "Activity Metric Score", "Activity Metric Value", "Activity Metric Comment")]
upload_metrics <- upload_metrics[!duplicated(upload_metrics[c("Activity ID")]), ] #QA check: make sure you still have same number of samples and different metrics for different sample at same site
write.csv(upload_metrics, paste0(results, "WQX_Upload/upload_metrics.csv"), row.names = FALSE)

#####biological upload file#####
# new_samples dataframe should have the corrected and final taxa names in finalID column and no NAs in DEEPTaxaID column based on the work done in Step 1
# original script had section for adding in new taxa names, byt since that is all sorted out in the beginning, this upload_bio file should contain all new taxon and DEEP IDs
# double check this by confirming there are no NAs in the final upload_bo dataframe and the number of obs. is the same as the total RBV samples in the original file
upload_bio <- new_samples
rbv_cat <- rbv_cat[!duplicated(rbv_cat[c("DEEPTaxaID")]), ] #dont need synonyms
upload_bio <- merge(rbv_cat, upload_bio, by = "DEEPTaxaID")
upload_bio <- upload_bio[c("staSeq", "CTDEEP.LabID", "DATE_COL", "finalID.y", "DEEPTaxaID", "LAB_NAME", "RBVCategory" )] #need to add: activity ID, analysis start date (= DATE_COL)
colnames(upload_bio)[colnames(upload_bio) == "staSeq"] <- 'Monitoring Location ID'
upload_bio$'Activity ID' <- paste((gsub("[^0-9]+", "", upload_bio$CTDEEP.LabID)), '_BioResults', sep = "") #this used to be an auto number but i am changing it :)
upload_bio$'Analysis Start Date' <- upload_bio$DATE_COL #these are the same date in older existing data
colnames(upload_bio)[colnames(upload_bio) == "finalID.y"] <- 'Subject Taxonomic Name'
colnames(upload_bio)[colnames(upload_bio) == "LAB_NAME"] <- 'Laboratory Name'
colnames(upload_bio)[colnames(upload_bio) == "RBVCategory"] <- 'Result Comment'
colnames(upload_bio)[colnames(upload_bio) == "DATE_COL"] <- 'Activity Start Date'
upload_bio <- upload_bio[c("Monitoring Location ID", "Activity ID", "Activity Start Date", 
                           "Analysis Start Date", "Subject Taxonomic Name", "Laboratory Name", "Result Comment")]
upload_bio$'Result Comment' <- gsub("LEAST WANTED", "Least Wanted Taxa", upload_bio$'Result Comment')#formatting like the template
upload_bio$'Result Comment' <- gsub("MODERATELY WANTED", "Moderately Wanted Taxa", upload_bio$'Result Comment')
upload_bio$'Result Comment' <- gsub("MOST WANTED", "MOST Wanted Taxa", upload_bio$'Result Comment')
upload_bio$'Result Comment' <- gsub("OTHER", "Not Categorized - Other Taxa", upload_bio$'Result Comment')

write.csv(upload_bio, paste0(results, "WQX_Upload/upload_bio.csv"), row.names = FALSE)