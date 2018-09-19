################################################################################
# R script to compute the number of gut microbiomes sampled per species
#
# Sebastien Villeger September 2018
################################################################################


rm(list=ls()) # cleaning memory

# loading libraries
library(googledrive)
library(googlesheets)

# ==============================================================================

# Importing the most recent version of fish metadata table and 
#  the table containing the species taxonomy

# ==============================================================================


# connecting to the Google sheets
id_gs_fish <- gs_title("fish")
id_gs_fish_taxo <- gs_title("fish_taxonomy")

# read the "fish" google sheet and keep only columns of interest
metadata_fish   <- gs_read(ss = id_gs_fish, ws = 1, range = cell_cols("A:V"), 
                           col_names = T) 
summary(metadata_fish)

metadata_fish <- metadata_fish[, c("Sample_ID", "Sampling_ID", "Taxonomy", 
                                   "body_mass", "gut_fullness", "gut_metabarcod_samples")]
metadata_fish <- as.data.frame(metadata_fish)
summary(metadata_fish)

# read the "fish_taxonomy" google sheet and reformat it
fish_taxonomy <- gs_read(ss = id_gs_fish_taxo, ws = 1, range = cell_cols("A:F"), 
                         col_names = T)
fish_taxonomy <- as.data.frame(fish_taxonomy)

# Create genus and species columns along with fishbase link columns
fish_taxonomy <- cbind(fish_taxonomy,
                       Genus = sapply(fish_taxonomy$Taxonomy, function(x) {
                         strsplit(x, "_")[[1]][1] }),
                       Species = sapply(fish_taxonomy$Taxonomy, function(x) {
                         strsplit(x, "_")[[1]][2] }),
                       FishBase = paste0("https://www.fishbase.de/summary/",
                                         gsub("_", "-", fish_taxonomy$Taxonomy))
)
fish_taxonomy <- fish_taxonomy[, c("Taxonomy","Order","Family","Genus","Species",
                                   "Common_name", "FishBase")]

# Order the table according to family and order
mask <- order(fish_taxonomy$Order, fish_taxonomy$Family)
fish_taxonomy <- fish_taxonomy[mask,]


# ==============================================================================

# Generate a table summarizing the number of gut microbiomes sampled per species

# ==============================================================================


# names of  fish species (as in metadata table)
names_sp <- sort(unique(metadata_fish$Taxonomy))

# Create an empty table to fill with results
microbiome_species <- as.data.frame(matrix(NA, length(names_sp) + 1, 6))
row.names(microbiome_species) <- c(names_sp, "TOTAL") 
names(microbiome_species)     <- c("Order", "Family", "N", "N_full", "min_Mass", "max_Mass")

# Compute total number of fish collected in each site for each species and range of their mass
# Loop on species
for (k in names_sp) {
  
  # individuals from species k
  mask   <-  which(metadata_fish$Taxonomy == k & (!is.na(metadata_fish$gut_metabarcod_samples)))
  fish_k <- metadata_fish[mask, ]

  # add species taxonomy
  microbiome_species[k, c("Order", "Family")] <- fish_taxonomy[k, c("Order", "Family")]
  
  # total number of individuals for species k
  microbiome_species[k, "N"] <- nrow(fish_k)
  
  # number of non-empty guts
  condition <- length(grep("empty", fish_k$gut_fullness))
  if (condition == 0) {
    microbiome_species[k, "N_full"] <- nrow(fish_k)
  } else {
    microbiome_species[k, "N_full"] <- nrow(fish_k) - condition
  }
  
  # range of body wet mass
  microbiome_species[k, c("min_Mass", "max_Mass") ] <- range(fish_k$body_mass, na.rm=T)
  
}# end of k

# adding values for all species combined
microbiome_species["TOTAL", c("N", "min_Mass", "max_Mass")] <- 
    c(  sum(microbiome_species[-nrow(microbiome_species), "N"] ), 
        min(microbiome_species[-nrow(microbiome_species), "min_Mass"]),  
        max(microbiome_species[-nrow(microbiome_species), "max_Mass"])
      )

# Order the table according to family and order
mask <- order(microbiome_species$Order, microbiome_species$Family)
microbiome_species <- microbiome_species[mask,]


# ==============================================================================

# Saving the files as .csv files then update corresponding google sheets 

# ==============================================================================

# Identify a local folder that is also synchronized with google drive
if (grep("arthur", getwd()) == 1) {
  saving_dir <- "/home/arthur/Projets/exofishmed/data/"
} else {
  saving_dir <- "/Users/SebV/Google Drive um/EXOFISHMED/WP4_gut_microbiome/4_3-5 Analyzes/"
}

# files named with date of metadata checking
date_save <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")
date_save

# create path to save files in local directory
save_path_species  <- file.path(paste0(saving_dir, "microbiome_species_", date_save, ".csv"))
save_path_taxonomy <- file.path(paste0(saving_dir, "fish_taxonomy_", date_save, ".csv"))

# saving in local directory that is also synchronized with google drive
write.csv(microbiome_species, file = save_path_species, row.names = T)
write.csv(fish_taxonomy, file = save_path_taxonomy, row.names = T)

# uploading as a google sheet on the googledrive
drive_upload(media = save_path_species, 
             path = as_id("https://drive.google.com/drive/folders/1z26u18MAchlO5VC6k8JQ6xkkjf3zXFPN"), 
             name = "microbiome_species", type = "spreadsheet", verbose = TRUE)

drive_upload(media = save_path_taxonomy, 
             path = as_id("https://drive.google.com/drive/folders/1z26u18MAchlO5VC6k8JQ6xkkjf3zXFPN"), 
             name = "fish_taxonomy", type = "spreadsheet", verbose = TRUE)



################################################################################
# END OF SCRIPT
################################################################################




