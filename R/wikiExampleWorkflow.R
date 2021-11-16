#-------------------------------------------------------------------------#
#------------------------ EXAMPLE WORKFLOW WIKI --------------------------#
#-------------------------------------------------------------------------#

## read in data
iucn_wd <- 'C:/Users/cub20nju/OneDrive - University of East Anglia/side_projects/species_attention_methods_paper/red_list_data_CR_birds'

simple_summary <- read_csv(paste0(iucn_wd, '/simple_summary.csv'))

## pull out wikipedia names and IDs (in this case the ID will be the scientific
## name as I don't like using the IUCN internalTaxonId) search has to have
## spaces replaced with underscores

wiki_searches <- tibble(ID = paste(simple_summary$genusName,
                                   simple_summary$speciesName,
                                   sep = ' '
                                   ),
                        search = paste(simple_summary$genusName,
                                       simple_summary$speciesName,
                                       sep = '_'
                                       )
                        )

