  library(googlesheets4)
  library(rmarkdown)
  library(jsonlite)
  
  # link to the google sheet
  google_sheet <- "https://docs.google.com/spreadsheets/d/1JIDRSzAm6RgGXqMTtxVwK0W1Fb1mlIl-xb_Li2vWsbg/edit#gid=1284778813"
  
  # get the properties of the google sheet
  gs_properties <- sheet_properties(google_sheet)
  
  # read in the sheet of interest 3
  bee_gs <- range_read(google_sheet, sheet = which(gs_properties$name == "Confirmed Species"))
sp = 1
for(sp in 1:length(bee_gs$scientificName)){
skip_to_next <- FALSE
tryCatch({
render(input = "C:/Users/mhall/State_Of_Biodiversity/Bee_Monograph_Hardy_et_al/scripts/Species_Account_Template_Reduced.Rmd",
       output_format = "html_document",
       output_file = paste0("C:/Users/mhall/State_Of_Biodiversity/Bee_Monograph_Hardy_et_al/species_accounts/",gsub(bee_gs$scientificName[sp],pattern=" ",replacement = "_")),
       params = list(species = bee_gs$scientificName[sp],
                     genus = bee_gs$genus[sp],
                     s_rank = bee_gs$`SPH S-rank`[sp],
                     subgenus = bee_gs$Subgenus[sp],
                     common_name = bee_gs$vernacularName[sp],
                     account = bee_gs$`Species Account`[sp]))
}, error = function(e){ cat("no account for - ",bee_gs$scientificName[sp],"\n")
   skip_to_next <<- TRUE})

if(skip_to_next){ next }
}

# check to ensure species that aren't represented are in the folder 
  file_names <- list.files("species_accounts/", pattern = ".html");
  file_names <- gsub(pattern = ".html", replacement = "", file_names);
  fileNames <- gsub(pattern = " ", replacement = "_", bee_gs$scientificName)
  
  RedoThese <- fileNames[!(fileNames %in% file_names)]
  RedoThese <- gsub(pattern = "_", replacement = " ", RedoThese);
  
  theseSpp <- match(RedoThese, bee_gs$scientificName)
  
for(sp in 1:length(RedoThese)){
    skip_to_next <- FALSE
    tryCatch({
      render(input = "C:/Users/mhall/State_Of_Biodiversity/Bee_Monograph_Hardy_et_al/scripts/Species_Account_Template_Reduced.Rmd",
             output_format = "html_document",
             output_file = paste0("C:/Users/mhall/State_Of_Biodiversity/Bee_Monograph_Hardy_et_al/species_accounts/",gsub(bee_gs$scientificName[theseSpp[sp]],pattern=" ",replacement = "_")),
             params = list(species = bee_gs$scientificName[theseSpp[sp]],
                           genus = bee_gs$genus[theseSpp[sp]],
                           s_rank = bee_gs$`SPH S-rank`[theseSpp[sp]],
                           subgenus = bee_gs$Subgenus[theseSpp[sp]],
                           common_name = bee_gs$vernacularName[theseSpp[sp]],
                           account = bee_gs$`Species Account`[theseSpp[sp]]))
    }, error = function(e){ cat("no account for - ",bee_gs$scientificName[theseSpp[sp]],"\n")
      
      if(skip_to_next){ next }
    })
}