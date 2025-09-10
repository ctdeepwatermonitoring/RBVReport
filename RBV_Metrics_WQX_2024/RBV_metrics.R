#script purpose: make summary tables for RBV annual report

setwd("P:\\RBVReport-main\\RBV_Metrics_WQX_2024")
library(dplyr)
library(tidyr)
library(tibble)

### Files to be updated for new sample year ###
# 1. New RBV Samples File
# 2. Latest csv file of awx stations
# 3. RBV site submission form csv downloaded from survey123 form
# 4. Most recent "TAXON" file downloaded from WQX

master_taxa_list <- read.csv("Master_Taxa_List.csv")
wqx_taxa_list <- read.csv("WQXTaxon_MasterTaxon_Lookup.csv")
new_samples <- read.csv("RBV_2024_Samples.csv")
rbv_taxa_list <- read.csv("RBV_MasterTaxaList_Lookup.csv")
awx_stations <- read.csv("awx_stations_webservice2024.csv")
taxon <- read.csv("TAXON.csv")
survey_output <- read.csv("RBV_Site_Submission_Form_0.csv")
grp_id <- read.csv("groups.csv")
summary <- read.csv("summary.csv", check.names= FALSE)

####step 0: get list of monitoring groups, open this csv file, add org code and coordinators name before running the rest of the script
#groups <- unique(survey_output$Monitoring.Organization.)
#groups <- as.data.frame(groups)
#write.csv(groups, "groups.csv", row.names = FALSE)

####step 1: compare new samples with master taxa list####
new_taxa <- subset(new_samples, new_samples$DEEPTaxaID == "(Not on List)") #list of allegedly new samples
colnames(new_taxa)[colnames(new_taxa)== "TAXON_NAME" ] <- "finalID"
new_taxa_confirm <- anti_join(new_taxa, master_taxa_list, by = "finalID") #now empty since we added them
write.csv(new_taxa, "rbv_newtaxa_2024.csv", row.names = FALSE)

####step 2: compare master taxa list with wqx taxa list####
master_vs_wqx <- anti_join(wqx_taxa_list, master_taxa_list, by = "finalID") #if empty this should mean everything in wqx list is in master taxa list
taxon_wqx_merge <- merge(taxon, wqx_taxa_list, by = "Name") #eliminates names not in wqx_taxa_list that shouldnt be there, ie not aquatic creatures
taxon_wqx_antijoin <- anti_join(wqx_taxa_list, taxon_wqx_merge, by = "Name") #this should be names that are present in wqx_taxa_list that are not in 'taxon' that *should* be there

##figuring out if i can upload
upload_test <- new_samples
colnames(upload_test)[colnames(upload_test) == "TAXON_NAME"] <- "Name"
upload_test$Name <- gsub(" sp.", "", upload_test$Name) #formatting like wqx
upload_test <- anti_join(upload_test, taxon, by = "Name")
upload_test <- upload_test[!duplicated(upload_test[c("Name")]), ]#stuff that i need to update first to have 2024 data in wqx

#all names
update_names <- master_taxa_list
colnames(update_names)[colnames(update_names) == "finalID"] <- "Name"
test <- anti_join(wqx_taxa_list, taxon, by = "Name")
write.csv(test, "outdated_names.csv")
write.csv(upload_test, "2024_outdated_names.csv")
 
#####Adding in a DEEP Taxa ID for the new samples, so they are counted, taxa specific for 2024 data#####
#new_taxa$DEEPTaxaID <- NA
#new_taxa <- new_taxa %>% mutate(DEEPTaxaID =
                                #case_when(new_taxa$finalID == "Peltoperla sp."	~ 1245,
                                          #new_taxa$finalID == "Limnocharidae" ~ 1246,
                                          #new_taxa$finalID == "Strophopteryx sp." ~ 1247))
#names(new_taxa)[names(new_taxa) == "finalID"] <- "TAXON_NAME"
#new_samples <- new_samples[new_samples$DEEPTaxaID != '(Not on List)',]
#new_samples <- rbind(new_samples, new_taxa)

####step 3: merge master taxa list with new list####
merged_list <- merge(new_samples, master_taxa_list, by = "DEEPTaxaID")

#####step 4: merge master list + new list with rbv lookup list to add rbv category#####
#Again Specific for 2024, these taxa names can just be changed to next years new taxa###
#rbv_taxa_list <- rbv_taxa_list %>% mutate(DEEPTaxaID =
                                  #case_when(rbv_taxa_list$finalID == "Peltoperla"	~ 1245,
                                            #rbv_taxa_list$finalID == "Limnocharidae" ~ 1246,
                                            #rbv_taxa_list$finalID == "Strophopteryx" ~ 1247, TRUE ~ DEEPTaxaID))

merged_list <- merge(merged_list, rbv_taxa_list, by = "DEEPTaxaID")
colnames(awx_stations)[colnames(awx_stations) == "STA_SEQ"] <- "AWQ..Station"
colnames(merged_list)[colnames(merged_list) == "staSeq"] <- "AWQ..Station"
merged_list <- merge(merged_list, awx_stations, by = "AWQ..Station")

#####making rbv final id category######
rbv_cat_table <- merged_list
rbv_cat_table <- rbv_cat_table[c("CTDEEP.LabID", "AWQ..Station", "Stream", "munName",
   "DATE_COL", "CLASS.x", "SUBCLASS.x", 
   "ORDER.x", "SUBORDER.x", 
   "FAMILY.x", "GENUS.x", "finalID.x", "RBVCategory")]
rbv_cat_table <- subset(rbv_cat_table, 
       rbv_cat_table$RBVCategory == "MOST WANTED" | 
       rbv_cat_table$RBVCategory == "MODERATELY WANTED" |
       rbv_cat_table$RBVCategory == "LEAST WANTED" |
         rbv_cat_table$FAMILY.x == "Tipulidae" | #rbv field sheet other family
         rbv_cat_table$FAMILY.x == "Elmidae" |
         rbv_cat_table$FAMILY.x == "Baetidae" |
         rbv_cat_table$FAMILY.x == "Athericidae" |
         rbv_cat_table$FAMILY.x == "Planariidae" |
         rbv_cat_table$CLASS.x == "Bivalvia") #rbv field sheet other class
rbv_cat_table$RBVFinalIDCat <- NA #make empty column
#most wanted
rbv_cat_table$RBVFinalIDCat <- ifelse(rbv_cat_table$RBVCategory == "MOST WANTED", rbv_cat_table$FAMILY.x, rbv_cat_table$RBVFinalIDCat) #all most wanted are to family with 2 exceptions
rbv_cat_table$RBVFinalIDCat <- ifelse(rbv_cat_table$GENUS.x == "Epeorus", rbv_cat_table$GENUS.x, rbv_cat_table$RBVFinalIDCat) #if genus = epeorus, replace with epeorus, if not use existing value
rbv_cat_table$RBVFinalIDCat <- ifelse(rbv_cat_table$GENUS.x == "Drunella", rbv_cat_table$GENUS.x, rbv_cat_table$RBVFinalIDCat) #exception to most wanted
#moderately wanted
rbv_cat_table$RBVFinalIDCat <- ifelse(rbv_cat_table$RBVCategory == "MODERATELY WANTED", rbv_cat_table$FAMILY.x, rbv_cat_table$RBVFinalIDCat) #most moderates are family
rbv_cat_table$RBVFinalIDCat <- ifelse(rbv_cat_table$SUBORDER.x == "Anisoptera", rbv_cat_table$SUBORDER.x, rbv_cat_table$RBVFinalIDCat) #exception to moderate
rbv_cat_table$RBVFinalIDCat <- ifelse(rbv_cat_table$SUBORDER.x == "Zygoptera", rbv_cat_table$SUBORDER.x, rbv_cat_table$RBVFinalIDCat) #exception to moderate
#least wanted
rbv_cat_table$RBVFinalIDCat <- ifelse(rbv_cat_table$ORDER.x == "Amphipoda", rbv_cat_table$ORDER.x, rbv_cat_table$RBVFinalIDCat) #least wanted below, taxa id level varies a lot
rbv_cat_table$RBVFinalIDCat <- ifelse(rbv_cat_table$ORDER.x == "Isopoda", rbv_cat_table$ORDER.x, rbv_cat_table$RBVFinalIDCat)
rbv_cat_table$RBVFinalIDCat <- ifelse(rbv_cat_table$SUBCLASS.x == "Hirudinea", rbv_cat_table$SUBCLASS.x, rbv_cat_table$RBVFinalIDCat)
rbv_cat_table$RBVFinalIDCat <- ifelse(rbv_cat_table$SUBCLASS.x == "Hirudinida", "Hirudinea", rbv_cat_table$RBVFinalIDCat) #synonym for hirudinea
rbv_cat_table$RBVFinalIDCat <- ifelse(rbv_cat_table$SUBCLASS.x == "Oligochaeta", rbv_cat_table$SUBCLASS.x, rbv_cat_table$RBVFinalIDCat)
rbv_cat_table$RBVFinalIDCat <- ifelse(rbv_cat_table$CLASS.x == "Gastropoda", rbv_cat_table$CLASS.x, rbv_cat_table$RBVFinalIDCat)
rbv_cat_table$RBVFinalIDCat <- ifelse(rbv_cat_table$FAMILY.x == "Chironomidae", rbv_cat_table$FAMILY.x, rbv_cat_table$RBVFinalIDCat)
rbv_cat_table$RBVFinalIDCat <- ifelse(rbv_cat_table$FAMILY.x == "Simuliidae", rbv_cat_table$FAMILY.x, rbv_cat_table$RBVFinalIDCat)
#other subset that are present on the field sheet
rbv_cat_table$RBVFinalIDCat <- ifelse(rbv_cat_table$FAMILY.x == "Tipulidae", rbv_cat_table$FAMILY.x, rbv_cat_table$RBVFinalIDCat) #below are the subset of 'other' on rbv field sheet
rbv_cat_table$RBVFinalIDCat <- ifelse(rbv_cat_table$FAMILY.x == "Elmidae", rbv_cat_table$FAMILY.x, rbv_cat_table$RBVFinalIDCat) 
rbv_cat_table$RBVFinalIDCat <- ifelse(rbv_cat_table$FAMILY.x == "Baetidae", rbv_cat_table$FAMILY.x, rbv_cat_table$RBVFinalIDCat) 
rbv_cat_table$RBVFinalIDCat <- ifelse(rbv_cat_table$FAMILY.x == "Athericidae", rbv_cat_table$FAMILY.x, rbv_cat_table$RBVFinalIDCat) 
rbv_cat_table$RBVFinalIDCat <- ifelse(rbv_cat_table$FAMILY.x == "Planariidae", rbv_cat_table$FAMILY.x, rbv_cat_table$RBVFinalIDCat) 
rbv_cat_table$RBVFinalIDCat <- ifelse(rbv_cat_table$ORDER.x == "Bivalvia", rbv_cat_table$ORDER.x, rbv_cat_table$RBVFinalIDCat)
#we ended up with ephemerellidae which are not included as a category on the sheet, only drunella spp. are
rbv_cat_table <- subset(rbv_cat_table, rbv_cat_table$RBVFinalIDCat != "Ephemerellidae")
rbv_cat_table <- rbv_cat_table[!duplicated(rbv_cat_table[c("CTDEEP.LabID", "RBVFinalIDCat")]), ] #counting one finalid cat per sample

######presence/absence table for report######
rbv_cat_pivot <- rbv_cat_table
rbv_cat_pivot <- rbv_cat_pivot[c("CTDEEP.LabID","AWQ..Station","Stream", "munName", "DATE_COL", "RBVFinalIDCat")] 
rbv_cat_pivot$Stream_Mun <- paste(rbv_cat_pivot$Stream, " ", "(", rbv_cat_pivot$munName, ")", sep = "")
rbv_cat_pivot$presence <- "X" #trying something funky
rbvidlist <- rbv_cat_pivot[!duplicated(rbv_cat_pivot[c("RBVFinalIDCat")]), ]
rbvidlist$RBVFinalIDCat #print cat columns
rbv_cat_pivot <- filter(rbv_cat_pivot, RBVFinalIDCat %in% 
                              c("Pteronarcyidae","Rhyacophilidae","Hydropsychidae","Perlidae","Elmidae","Capniidae",
                                "Leuctridae","Nemouridae","Peltoperlidae","Taeniopterygidae","Chloroperlidae",
                                "Brachycentridae","Perlodidae","Zygoptera",
                                "Baetidae","Epeorus","Heptageniidae","Isonychiidae","Amphipoda","Drunella","Isopoda",
                                "Psephenidae","Athericidae","Chironomidae","Oligochaeta","Simuliidae","Tipulidae","
                                Gastropoda","Hirudinea","Corydalidae","Glossosomatidae","Lepidostomatidae","Apataniidae",
                                "Philopotamidae","Anisoptera")) %>%
  pivot_wider(names_from = RBVFinalIDCat, values_from = presence)
colnames(rbv_cat_pivot)
rbv_cat_pivot$Chironimidae <- NA
rbv_cat_pivot$Gastropoda <- NA
rbv_cat_pivot$Planariidae <- NA
rbv_cat_pivot$Bivalvia <- NA
rbv_cat_pivot <- rbv_cat_pivot[c("CTDEEP.LabID","AWQ..Station","Stream_Mun","DATE_COL", #just to look at, not write
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
rbv_cat_pivot <- rbv_cat_pivot[c("Stream_Mun","AWQ..Station","DATE_COL", 
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
write.csv(rbv_cat_pivot, "presence_absence_report.csv", row.names = FALSE)

######step 5: per site, calculate number of most wanted taxa#####
all_sites <- merged_list %>% #reference list of all unique site + date combos
  group_by(CTDEEP.LabID, AWQ..Station, DATE_COL) %>%
  summarize() 

sites_with_most_wanted <- merged_list %>% #count of most wanted per sample
  filter(RBVCategory == "MOST WANTED") %>%
  group_by(CTDEEP.LabID, AWQ..Station, DATE_COL) %>%
  distinct(FAMILY.x) %>% #there are duplicate family columns ie x and y, probably unnecessary joining
  summarize(RBV_most_wanted_count = n()) 

sites_with_no_most_wanted <- all_sites %>% 
  anti_join(sites_with_most_wanted, by = c("CTDEEP.LabID", "AWQ..Station", "DATE_COL")) %>% #looking for what isnt in most wanted but is in all sites
  mutate(RBV_most_wanted_count = 0)  #makes a new column with value 0 since they had no most wanted

#summary table with most wanted count by site
final_site_metrics <- bind_rows(sites_with_most_wanted, sites_with_no_most_wanted)
final_site_metrics <- merge(final_site_metrics, awx_stations, by = "AWQ..Station")
write.csv(final_site_metrics, "metrics_labid.csv", row.names = FALSE)
final_site_metrics <- final_site_metrics[c("CTDEEP.LabID", "AWQ..Station", "WaterbodyName", "munName", "DATE_COL", "RBV_most_wanted_count")]
colnames(final_site_metrics)[colnames(final_site_metrics) == "munName"] <- "Municipality"
write.csv(final_site_metrics, "RBV_Summary_2024.csv", row.names = FALSE)

#filtered rbv id per site, # of unique taxa should match summary count by site
#e.g. if a site had a count of 4, 4 unique rbv category taxa from that site should be found in this table
#alternate way of viewing presence/absence table
most_wanted_by_site <- merged_list
most_wanted_by_site <- subset(most_wanted_by_site, most_wanted_by_site$RBVCategory == "MOST WANTED")
most_wanted_by_site <- most_wanted_by_site[c("CTDEEP.LabID", "AWQ..Station", "Stream", "DATE_COL", "FAMILY.x", "finalID.x")]
most_wanted_by_site <- unique(most_wanted_by_site)
most_wanted_by_site <- most_wanted_by_site[!duplicated(most_wanted_by_site[c("AWQ..Station", "FAMILY.x", "DATE_COL")]), ]
colnames(most_wanted_by_site)[colnames(most_wanted_by_site) == "AWQ..Station"] <- "STA_SEQ"
colnames(most_wanted_by_site)[colnames(most_wanted_by_site) == "FAMILY.x"] <- "Family"
colnames(most_wanted_by_site)[colnames(most_wanted_by_site) == "finalID.x"] <- "finalID"
write.csv(most_wanted_by_site, "RBV_MostWantedBySite_2024.csv", row.names = FALSE)

####checking S123 output with summary table####
survey_output$testcol <- paste(survey_output$Waterbody.Name., survey_output$Station.ID., sep = "")
final_test <- final_site_metrics
final_test$testcol <- paste(final_test$WaterbodyName, final_test$STA_SEQ, sep = "")
data_entry_errors <- anti_join(survey_output, final_test, by = "testcol")

#####putting org name with count by site#####
labid <- read.csv("metrics_labid.csv")
colnames(labid)[colnames(labid) == "CTDEEP.LabID"] <- "sampleID"
labid <- merge(survey_output, labid, by = "sampleID")
groups <- read.csv("groups.csv")
colnames(groups)[1] <- ("Monitoring.Organization.")
labid <- merge(groups, labid, by = "Monitoring.Organization.")
labid <- labid[c("Waterbody.Name.", "AWQ..Station", "code", "DATE_COL", "RBV_most_wanted_count")]
write.csv(labid, "summarybysite_code.csv", row.names = FALSE)

#####For Summary Stats file used in report#####
stats <- data.frame(matrix(ncol = 5, nrow = 1))
colnames(stats) <-c("# RBV Samples Submitted", "# Monitoring Stations", "# Streams Monitored", "# Samples w/ 4+ 'Most Wanted' Types", "% of Samples w/ 4+  'Most Wanted' Types")
stats$'# RBV Samples Submitted' <- nrow(labid)
stats$`# Monitoring Stations` <- length(unique(labid$AWQ..Station))
stats$`# Streams Monitored` <- length(unique(labid$Waterbody.Name.))
stats$`# Samples w/ 4+ 'Most Wanted' Types` <- sum(labid$RBV_most_wanted_count >= 4)
stats$`% of Samples w/ 4+  'Most Wanted' Types` <- paste0(round(sum(labid$RBV_most_wanted_count >= 4)/ nrow(labid) *100), "%")
# rearranging dataframe so I can merge
stats_t <- as.data.frame(t(stats))
stats_t <- rownames_to_column(stats_t, var = "Category")
colnames(stats_t)[2] <- "2024"
summarystats <- merge(summary, stats_t, by = "Category", sort = FALSE)
write.csv(summarystats, "summarystats.csv", row.names = FALSE)


#####For formatted presence absent report, this still needs to be edited a little in excel for the final version#####
colnames(rbv_cat_pivot)[1] <- ("Waterbody.Name.")
presabsforreport <- merge(labid, rbv_cat_pivot, by = c("AWQ..Station", "DATE_COL"))
### eliminating duplicate values that arise when samples are taken on the same day at the same  stream
presabsforreport <- presabsforreport %>%
  distinct(AWQ..Station, DATE_COL, Waterbody.Name..x, code, RBV_most_wanted_count, .keep_all = TRUE)
presabsforreport$Waterbody.Name..x <- NULL
presabsforreport <- presabsforreport %>% select(Waterbody.Name..y, AWQ..Station, code, DATE_COL, everything())
names(presabsforreport)[1:5] <- c("Waterbody", "Station ID", "Org Name", "Date Collected", "Total Most Wanted Count")
write.csv(presabsforreport, "PresentAbsent_Reformat.csv", na = " ", row.names = FALSE)

######wqx stuff#################################################################

#stations for wqx
wqx_stations <- left_join(all_sites, awx_stations, by = "AWQ..Station")
colnames(wqx_stations)
wqx_stations <- wqx_stations[c("AWQ..Station", "WaterbodyName", "ylat", "xlong")]
write.csv(wqx_stations, "upload_stations.csv", row.names = FALSE)

####Site info for RVB sites, formatted file for the report###
sites <- merge(wqx_stations,awx_stations, by = "AWQ..Station")
sites <- sites[c("WaterbodyName.x","AWQ..Station", "munName", "Description","ylat.x", "xlong.x")]
names(sites) <- c("Waterbody Name", "Station ID", "Municipality", "Description", "Latitude", "Longitude")
write.csv(sites, "sitesforreport.csv", row.names = FALSE)

####lazy from old version
awx_stations <- read.csv("awx_stations_webservice2024.csv")
new_sites <- read.csv("RBV_Summary_2024.csv") #stationid and metrics results
sampleinfo <- read.csv("RBV_2024_Samples.csv") 
taxon <- read.csv("TAXON.csv") #wqx taxa list
master_taxa_list <- read.csv("Master_Taxa_List.csv") #DEEP taxa
wqx_master_lookup <- read.csv("WQXTaxon_MasterTaxon_Lookup.csv")
rbv_cat <- read.csv("RBV_MasterTaxaList_Lookup.csv") #manually added in new spp

#####metrics upload file#####
upload_metrics <- merge(sampleinfo, new_sites, by = "CTDEEP.LabID") #to preserve unique samples
#need to make: activity ID (accession# + _TaxaMostW)
#activity metric comment (= deep field id: / lab id:)
upload_metrics <- upload_metrics[c("staSeq", "CTDEEP.LabID", "DATE_COL.x", "RBV_most_wanted_count", "LAB_SAMPLE_ID" )]
upload_metrics$'Activity ID' <- paste((gsub("[^0-9]+", "", upload_metrics$CTDEEP.LabID)), '_TaxaMostW') #pulling out numeric values and adding text
upload_metrics$'Activity Metric Comment' <- paste('DEEP Field ID: ', upload_metrics$CTDEEP.LabID, ' / Taxonomy Lab ID: ', upload_metrics$LAB_SAMPLE_ID)
upload_metrics$'Activity Metric Value' <- upload_metrics$RBV_most_wanted_count #there are duplicated value and score columns
colnames(upload_metrics)[colnames(upload_metrics) == "staSeq"] <- 'Monitoring Location ID'
colnames(upload_metrics)[colnames(upload_metrics) == "DATE_COL.x"] <- 'Activity Start Date'
colnames(upload_metrics)[colnames(upload_metrics) == "RBV_most_wanted_count"] <- 'Activity Metric Score'
upload_metrics <- upload_metrics[c("Monitoring Location ID", "Activity ID", "Activity Start Date", 
                                   "Activity Metric Score", "Activity Metric Value", "Activity Metric Comment")]
upload_metrics <- upload_metrics[!duplicated(upload_metrics[c("Activity ID")]), ] #QA check: make sure you still have 66 samples and different metrics for different sample at same site
write.csv(upload_metrics, "upload_metrics.csv", row.names = FALSE)

#####biological upload file#####
upload_bio <- merge(sampleinfo, master_taxa_list, by = "DEEPTaxaID") #match DEEP id# to master taxa list
rbv_cat <- rbv_cat[!duplicated(rbv_cat[c("DEEPTaxaID")]), ] #dont need synonyms
upload_bio <- merge(rbv_cat, upload_bio, by = "DEEPTaxaID")
upload_bio <- upload_bio[c("staSeq", "CTDEEP.LabID", "DATE_COL", "finalID.y", "DEEPTaxaID", "LAB_NAME", "RBVCategory" )] #need to add: activity ID, analysis start date (= DATE_COL)
upload_bio$finalID.y <- gsub("Turbellaria", "Platyhelminthes", upload_bio$finalID.y) #name vs finalid are not the same between wqx and master taxa list, so manually changing
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

#ADD IN THE TAXA W NO DEEP ID
new_taxa <- subset(sampleinfo, sampleinfo$DEEPTaxaID == "(Not on List)")
colnames(new_taxa)[colnames(new_taxa) == "TAXON_NAME"] <- "finalID"
new_taxa$finalID <- gsub(" sp.", "", new_taxa$finalID)
new_taxa <- left_join(new_taxa, rbv_cat, by = "finalID")

new_taxa <- new_taxa[c("staSeq", "CTDEEP.LabID", "DATE_COL", "finalID", "DEEPTaxaID.x", "LAB_NAME", "RBVCategory" )] #need to add: activity ID, analysis start date (= DATE_COL)
colnames(new_taxa)[colnames(new_taxa) == "staSeq"] <- 'Monitoring Location ID'
new_taxa$'Activity ID' <- paste((gsub("[^0-9]+", "", new_taxa$CTDEEP.LabID)), '_BioResults', sep = "") #this used to be an auto number but i am changing it :)
new_taxa$'Analysis Start Date' <- new_taxa$DATE_COL #these are the same date in older existing data
colnames(new_taxa)[colnames(new_taxa) == "finalID"] <- 'Subject Taxonomic Name'
colnames(new_taxa)[colnames(new_taxa) == "LAB_NAME"] <- 'Laboratory Name'
colnames(new_taxa)[colnames(new_taxa) == "RBVCategory"] <- 'Result Comment'
colnames(new_taxa)[colnames(new_taxa) == "DATE_COL"] <- 'Activity Start Date'
new_taxa <- new_taxa[c("Monitoring Location ID", "Activity ID", "Activity Start Date", 
                           "Analysis Start Date", "Subject Taxonomic Name", "Laboratory Name", "Result Comment")]
new_taxa$'Result Comment' <- gsub("LEAST WANTED", "Least Wanted Taxa", new_taxa$'Result Comment')#formatting like the template
new_taxa$'Result Comment' <- gsub("MODERATELY WANTED", "Moderately Wanted Taxa", new_taxa$'Result Comment')
new_taxa$'Result Comment' <- gsub("MOST WANTED", "MOST Wanted Taxa", new_taxa$'Result Comment')
new_taxa$'Result Comment' <- gsub("OTHER", "Not Categorized - Other Taxa", new_taxa$'Result Comment')
upload_bio <- rbind(new_taxa, upload_bio)
upload_bio$`Subject Taxonomic Name` <- gsub("Oligochaeta", "Oligochaeta (Oligochaeta)", upload_bio$`Subject Taxonomic Name`) #invalid
write.csv(upload_bio, "upload_bio.csv", row.names = FALSE)