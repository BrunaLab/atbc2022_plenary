# load libraries ----------------------------------------------------------
library(tidyverse)

# spelling
library(hunspell)
library(tidytext)
library(stringi)
# lemmatization
library(koRpus)
library(koRpus.lang.en)
library(tokenizers)
# stopwords
library(stopwords)
library(stringr)



library(ngram)
library(igraph)
library(tidystringdist)
library(SemNetCleaner)
library(SemNeT)

# tutorial
# https://www.youtube.com/watch?v=FbznaCOXbcU
# Load reference records, clean keywords and text of DE and titles -----------------

complete_data <- read_csv("./bibliometrics/data_clean/complete_data.csv") %>%
  mutate(TI = gsub(" - ", " ", TI)) %>%
  replace_na(list(pub_cat = "temperate")) %>%
  mutate(DE = gsub("\n", " ", DE)) %>%
  mutate(DE = gsub("\"", "", DE)) %>%
  mutate(DE = gsub("<bold>", "", DE)) %>%
  mutate(DE = gsub("</bold>", "", DE)) %>%
  mutate(DE = gsub("- ", "-", DE)) %>%
  mutate(DE = gsub("r  theory *", "rstar theory", DE)) %>%
  mutate(DE = gsub("&#8208", "-", DE)) %>%
  mutate(DE = gsub("&#8211", "-", DE)) %>%
  mutate(DE = gsub("&#8217", "'", DE)) %>%
  mutate(DE = gsub("&amp", "&", DE))
unique(complete_data$SO)

# open refine  ------------------------------------------------------------------

# Take the KW column and split it up, arrange as a column
keywords_all <- complete_data %>%
  select(refID, DE) %>%
  drop_na(DE) %>%
  rename(kw_original = DE) %>%
  #   some of the key words are seperated by ",". This complicates things because some key words are also in the
  # format "biomass, microbial" (instead of microbial biomass"). Also have "STRI, Panama" or "Montana, USA" But easier to split, i think.
  mutate(kw_original = gsub(",", ";", kw_original)) %>%
  # most efficient way to split withoutknowing number of columns
  # https://stackoverflow.com/questions/33288695/how-to-use-tidyrseparate-when-the-number-of-needed-variables-is-unknown
  mutate(to = strsplit(kw_original, ";")) %>%
  unnest(to) %>%
  group_by(kw_original) %>%
  mutate(row = row_number()) %>%
  spread(row, to) %>%
  ungroup() %>%
  select(-kw_original) %>%
  #   separate(kw_original,c(LETTERS[seq( from = 1, to = 20 )]), sep = ";") %>%
  pivot_longer(!refID, names_to = "letter", values_to = "kw_original") %>%
  select(-letter) %>%
  drop_na(kw_original) %>%
  mutate(kw_original = trimws(kw_original)) %>%
  mutate(kw_original = gsub("\n", " ", kw_original)) %>%
  # mutate(kw_original=str_replace('\\n"', '')) %>%
  mutate(kw_original = tolower(kw_original))

keywords_all %>% summarize(n_distinct(kw_original))

# remove the opnes that didnt split up--------------------------------------
unsplit_kw <-keywords_all %>% 
  select(refID,kw_original) %>% 
  mutate(nchar=nchar(kw_original)) %>% 
  arrange(desc(nchar)) %>% 
  filter(nchar>50)


# remove unsplit kw -------------------------------------------------------

keywords<-anti_join(keywords_all,unsplit_kw,by="refID")



keywords <- keywords %>%
  mutate(kw_original = gsub("above-ground", "aboveground", kw_original)) %>%
  mutate(kw_original = gsub("below-ground", "belowground", kw_original)) %>%
  mutate(kw_original = gsub("below- ", "belowground", kw_original)) %>%
  mutate(kw_original = gsub("above- ", "aboveground", kw_original)) %>%
  mutate(kw_original = gsub("c-3", " c3", kw_original)) %>%
  mutate(kw_original = gsub("c-4", " c4", kw_original)) %>%
  mutate(kw_refined_manual = case_when(
    kw_original == "ant plant interactions" ~ "ant-plant interactions",
    # kw_original ==  "ant plant interactions" ~ "ant-plant interactions",
    # kw_original ==  "tropical forests" ~ "tropical forest",
    # kw_original ==  "tropical forests" ~ "tropical forest",
    kw_original == "germination" ~ "seed germination",
    # kw_original ==  "litter" ~ "litterfall",
    kw_original == "litterfall" ~ "litter",
    # kw_original ==  "tropical" ~ "tropics/tropical",
    # kw_original ==  "tropics" ~ "tropics/tropical",
    kw_original == "germination" ~ "seed germination",
    # kw_original ==  "tropical" ~ "tropics/tropical",
    # kw_original ==  "tropics" ~ "tropics/tropical",
    # kw_original ==  "tropical forestsuccession" ~ "tropical forest succession/regeneration",
    kw_original == "tropical forestsuccession" ~ "tropical forest succession",
    # kw_original ==  "tropical forest succession" ~ "tropical forest succession/regeneration",
    # kw_original ==  "tropical forest regeneration" ~ "tropical forest succession/regeneration",
    # kw_original ==  "tropical forests" ~ "tropical forest(s)",
    # kw_original ==  "tropical forest" ~ "tropical forest(s)",
    # kw_original ==  "tropical forest fragmentation" ~ "tropical forest fragments/fragmentation",
    # kw_original ==  "tropical forest fragments" ~ "tropical forest fragmentation",
    # kw_original ==  "tropical forest fragment" ~"tropical forest fragmentation",
    # kw_original ==  "tropical forest fragments" ~ "tropical forest fragments/fragmentation",
    # kw_original ==  "tropical forest fragment" ~"tropical forest fragments/fragmentation",
    kw_original == "tropical forest manage-" ~ "tropical forest management",
    # kw_original ==  "invertebrate herbivory" ~ "invertebrate herbivory/herbivore(s)",
    # kw_original ==  "invertebrate herbivores" ~ "invertebrate herbivory/herbivore(s)",
    # kw_original ==  "invertebrate community structure" ~ "invertebrate communities/structure",
    # kw_original ==  "invertebrate communities" ~ "invertebrate communities/structure",
    # kw_original ==  "invertebrate predators" ~ " invertebrate predator(s)",
    # kw_original ==  "invertebrate predator" ~ " invertebrate predator(s)",
    # kw_original ==  "seed dispersal networks" ~ "seed dispersal network(s)",
    # kw_original ==  "seed dispersal network" ~ "seed dispersal network(s)",
    # kw_original ==  "seed dispersal kernels" ~ "seed dispersal kernel(s)",
    # kw_original ==  "seed dispersal kernel" ~ "seed dispersal kernel(s)",
    # kw_original ==  "seed-dispersal mutualism" ~ "seed dispersal mutualism(s)",
    # kw_original ==  "seed dispersal mutualisms" ~ "seed dispersal mutualism(s)",
    # kw_original ==  "drosophila" ~ "drosophila / d melanogaster",
    # kw_original ==  "drosophila melanogaster" ~ "drosophila / d melanogaster",
    # kw_original ==  "dispersal modes" ~ "dispersal mode(s)",
    # kw_original ==  "dispersal mode" ~ "dispersal mode(s)",
    # kw_original ==  "dispersal models" ~ "dispersal model(s)",
    # kw_original ==  "dispersal model" ~ "dispersal model(s)",
    # kw_original ==  "foraging mode" ~ "foraging mode(s)",
    # kw_original ==  "foraging models" ~ "foraging model(s)",
    # kw_original ==  "foraging model" ~ "foraging model(s)",
    kw_original == "cerraddo" ~ "cerrado",
    kw_original == "path analyses" ~ "path analysis",
    # kw_original ==  "feather" ~ "feather(s)",
    # kw_original ==  "feathers" ~ "feather(s)",
    # kw_original ==  "p ratio" ~ "P ratio(s)",
    # kw_original ==  "p ratios" ~ "P ratio(s)",
    # kw_original ==  "x choromosomes" ~ "x choromosomes",
    # kw_original ==  "rain forest" ~ "(tropical) rain forest(s)",
    kw_original == "rain forest" ~ "tropical rain forest",
    # kw_original ==  "diversity" ~ "(species) diversity/biodiversity",
    # kw_original ==  "species diversity" ~ "(species) diversity/biodiversity",
    kw_original == "species diversity" ~ "diversity",
    kw_original == "biodiversity" ~ "diversity",
    # kw_original ==  "biodiversity" ~ "(species) diversity/biodiversity",
    # kw_original ==  "tropical rainforest"~"(tropical) rain forest(s)",
    kw_original == "tropical rainforest" ~ "tropical rain forest",
    kw_original == "redundancy analysis (rda)" ~ "redundancy analysis",
    # kw_original ==  "plant community" ~ "plant communities",
    # kw_original ==  "pinus plantations"~"pine plantation(s)",
    # kw_original ==  "pinus plantation"~"pine plantation(s)",
    # kw_original ==  "lowland tropical forest" ~ "lowland tropical rain forest",
    # kw_original ==  "vapour pressure deficits" ~ "vapor pressure deficit",
    kw_original == "megafaunal-dispersal syndrome" ~ "megafaunal dispersal syndrome",
    # kw_original ==  "amazon" ~ "amazon(ia)",
    kw_original == "land use" ~ "land-use",
    # kw_original ==  "ant" ~ "ant(s)",
    # kw_original ==  "ants" ~ "ant(s)",
    # kw_original ==  "mammal" ~ "mammalia",
    # kw_original ==  "mammals" ~ "mammalia",
    kw_original == "oil-palm" ~ "oil palm",
    # kw_original ==  "fig" ~ "fig(s)",
    # kw_original ==  "rodent" ~ "rodentia",
    kw_original == "bambusoideae" ~ "bambuseae",
    # kw_original ==  "relative growth" ~ "relative growth (rate)",
    kw_original == "relative growth" ~ "relative growth rate",
    # kw_original ==  "relative growth rate" ~ "relative growth (rate)",
    # kw_original ==  "tropical montane cloud forest"~"tropical montane forest",
    kw_original == "reduced impact logging" ~ "reduced-impact logging",
    kw_original == "afrotropical" ~ "afrotropics",
    # kw_original ==  "insectivores" ~ "insectivores/insectivory",
    # kw_original ==  "insectivory" ~ "insectivores/insectivory",
    kw_original == "land use history" ~ "land-use history",
    # kw_original ==  "bird community" ~ "bird communities",
    kw_original == "agriculture intensification" ~ "agricultural intensification",
    kw_original == "phylogenetic generalized least squares gls" ~ "phylogenetic generalized least squares",
    kw_original == "phylogenetic generalized least squares pgls" ~ "phylogenetic generalized least squares",
    # kw_original ==  "camera trapping" ~ "camera trap(ping)",
    # kw_original ==  "camera trap" ~ "camera trap(ping)",
    # kw_original ==  "leaf litterfall" ~ "leaf litter",
    kw_original == "leaf litterfall" ~ "litter",
    # kw_original ==  "liana-tree interaction" ~ "liana-tree interaction (network)",
    kw_original == "liana-tree interaction network" ~ "liana-tree interaction (network)",
    # kw_original ==  "canopy openness" ~ "canopy openness/openings",
    # kw_original ==  "canopy openings" ~ "canopy openness/openings",
    # kw_original ==  "insect-plant interactions" ~ "plant-insect interaction",
    # kw_original ==  "thermal performance" ~ "thermal performance (curves)",
    # kw_original ==  "thermal performance curves" ~ "thermal performance (curves)",
    # kw_original ==  "atlantic rain forest biome" ~ "atlantic rain forest",
    kw_original == "atlantic rainforest" ~ "atlantic rain forest",
    # kw_original ==  "arboreal" ~ "arboreal/arboreality",
    # kw_original ==  "arboreality" ~ "arboreal/arboreality",
    # kw_original ==  "resprout" ~ "resprout(ing)",
    # kw_original ==  "resprouting" ~ "resprout(ing)",
    kw_original == "land use change" ~ "land-use change",
    # kw_original ==  "forest canopies" ~ "forest canopy",
    kw_original == "tropical mountain forests" ~ "tropical montane forest",
    # kw_original ==  "decomposition" ~ "decomposition rate",
    kw_original == "albertine rift eco-region" ~ "albertine rift region",
    kw_original == "climatic change" ~ "climate change",
    # kw_original ==  "neotropical" ~ "neotropics",
    kw_original == "psittacidae" ~ "psittacids",
    kw_original == "psittacines" ~ "psittacids",
    kw_original == "pan troglodytes verus" ~ "pan troglodytes",
    kw_original == "anura" ~ "anurans",
    kw_original == "la selva biological research station" ~ "la selva",
    kw_original == "la selva biological station" ~ "la selva",
    kw_original == "animal-plant interaction" ~ "plant-animal interactions",
    kw_original == "long distance dispersal" ~ "long-distance dispersal",
    kw_original == "twig-nesting ant species" ~ "twig-nesting ants",
    kw_original == "tropical mountain cloud forest" ~ "tropical montane cloud forest",
    # kw_original ==  "life histories" ~ "life history",
    kw_original == "seasonally dry tropical forest" ~ "tropical dry forest",
    kw_original == "seasonal dry tropical forest" ~ "tropical dry forest",
    kw_original == "dry-season flushing" ~ "dry season flushing",
    # kw_original ==  "photosynthesis rates" ~ "photosynthesis (rates)",
    # kw_original ==  "photosynthesis" ~ "photosynthesis (rates)",
    kw_original == "janzen-connell model" ~ "janzen-connell",
    kw_original == "termitidae" ~ "termite",
    kw_original == "carbon dioxide (co2)" ~ "carbon dioxide",
    kw_original == "tropical montane" ~ "tropical montane forest",
    # kw_original ==  "tropical lowland forests" ~ "tropical lowland rain forest(s)",
    kw_original == "tropical lowland forests" ~ "tropical lowland rain forest",
    kw_original == "atlantic rain forest biome" ~ "atlantic rain forest",
    kw_original == "symbiotic microbiota" ~ "symbiotic microbes",
    kw_original == "caribbean sea" ~ "caribbean",
    kw_original == "post-dispersal predation" ~ "postdispersal seed predation",
    kw_original == "phyllostomid bats" ~ "phyllostomidae",
    kw_original == "life table response experiment" ~ "life table response experiments",
    # kw_original ==  "tropical rainforest" ~ "tropical rain forest(s)",
    kw_original == "b matrix" ~ "b-matrix",
    kw_original == "type 1 error" ~ "type-1 error",
    kw_original == "type i error" ~ "type-1 error",
    kw_original == "c : p ratio" ~ "cp ratio",
    kw_original == "c : p ratios" ~ "cp ratio",
    kw_original == "k : p ratio" ~ "kp ratio",
    kw_original == "k : p ratios" ~ "kp ratio",
    kw_original == "n : k ratio" ~ "nk ratio",
    kw_original == "n : k ratios" ~ "nk ratio",
    kw_original == "n : p ratios" ~ "np ratios",
    kw_original == "g matrix" ~ "g-matrix",
    kw_original == "b matrix" ~ "b-matrix",
    kw_original == "m matrix" ~ "m-matrix",
    kw_original == "p matrix" ~ "p-matrix",
    kw_original == "barro colorado island" ~ "bci",
    kw_original == "barro colorado-island" ~ "bci",
    kw_original == "barro-colorado island" ~ "bci",
    kw_original == "burro colorado island" ~ "bci",
    kw_original == "site-dependence" ~ "site-dependence",
    kw_original == "site-dependency" ~ "site-dependence",
    kw_original == "b-chromosomes" ~ "b-chromosome",
    kw_original == "f statistics" ~ "f-statistics",
    # kw_original ==  "np ratio"~"np ratio(s)",
    # kw_original ==  "np ratios"~"np ratio(s)",
    kw_original == "n limitation" ~ "n-limitation",
    kw_original == "rapid biodiversity assessment protocol" ~ "rapid biodiversity assessment",
    # kw_original ==  "noninvasive sample"~"noninvasive sample/sampling",
    # kw_original ==  "noninvasive sampling"~"noninvasive sample/sampling",
    # kw_original ==  "road"~"roads",
    kw_original == "varillales" ~ "varillal",
    # kw_original ==  "palm"~"palm(s)",
    # kw_original ==  "palms"~"palm(s)",
    # kw_original ==  "bird"~"bird(s)",
    # kw_original ==  "birds"~"bird(s)",
    #  kw_original ==  abiotic&#8208~"abiotic",
    kw_original == "abundant centre model" ~ "abundant center model",
    kw_original == "barley and cereal yellow dwarf virus" ~ "barley and cereal yellow dwarf viruses",
    kw_original == "acer opalus ssp granatense" ~ "acer opalus",
    kw_original == "acer opalus subsp granatense" ~ "acer opalus",
    kw_original == "australian monsoon tropics" ~ "australian monsoonal tropics",
    kw_original == "adaptation and trade-off" ~ "adaptations and trade-offs",
    kw_original == "biodiversity and ecosystem functioning" ~ "biodiversity and ecosystem function",
    kw_original == "alternative stable state" ~ "alternate stable state",
    kw_original == "anas plathyrynchos" ~ "anas platyrhynchos",
    kw_original == "asymmetric competition" ~ "asymmetrical competition",
    kw_original == "binomial mixture model" ~ "binomial n-mixture model",
    kw_original == "barley yellow dwarf virus (bydv)" ~ "barley and cereal yellow dwarf viruses",
    kw_original == "barley yellow dwarf viruses (bydvs)" ~ "barley and cereal yellow dwarf viruses",
    kw_original == "arbuscular mycorrhiza" ~ "arbuscular myccorrhyiza",
    kw_original == "aguoti paca" ~ "agouti paca",
    kw_original == "anti-predator behavior" ~ "antipredatory behavior",
    kw_original == "below-ground process" ~ "below-ground processes",
    kw_original == "behavioural tradeoff" ~ "behavioral trade-off",
    kw_original == "anti-plant" ~ "ant-plant",
    kw_original == "bayesian hierarchical modeling" ~ "bayesian hierarchical model",
    kw_original == "behavior genetics" ~ "behavioral genetics",
    kw_original == "alternate states" ~ "alternative states",
    kw_original == "arciidae" ~ "arctiidae",
    kw_original == "area-concentrated search" ~ "area-concentrated searching",
    kw_original == "behavioral changes" ~ "behavioral change",
    kw_original == "behavioural change" ~ "behavioral change",
    kw_original == "age-specific breeding probabilities" ~ "age-specific breeding probability",
    kw_original == "alternative reproductive strategies" ~ "alternative reproductive strategy",
    kw_original == "anadromous fish" ~ "anadromous fishes",
    kw_original == "bialowieza forest" ~ "biaowiea forest",
    kw_original == "above and belowground herbivores" ~ "above- and belowground herbivory",
    kw_original == "alternate prey" ~ "alternative prey",
    kw_original == "arciidae" ~ "ariidae",
    kw_original == "anthropogenic stress" ~ "anthropogenic stressors",
    kw_original == "biogeochemical model" ~ "biogeochemical modeling",
    # kw_original == "ant assemblages"~"bat assemblages",
    # kw_original == "ant pollination"~"bat pollination",
    # kw_original == "arboreal ants"~"arboreal plants",
    kw_original == "allogenic ecosystem engineers" ~ "autogenic ecosystem engineers",
    kw_original == "basic reproductive number" ~ "r0",
    kw_original == "r-o" ~ "r0",
    kw_original == "alternative mating strategies" ~ "alternative mating strategy",
    kw_original == "above-ground competition" ~ "above-ground competition cue",
    kw_original == "agelaia" ~ "aglaia",
    kw_original == "agro-ecosystem" ~ "agroecosystems",
    kw_original == "akaike information criterion" ~ "akaikes information criteria",
    # kw_original == "annual grass"~"annual grasses",
    kw_original == "baetids" ~ "baetis",
    # kw_original == "bioenergetic model"~"bioenergetic modeling",
    kw_original == "biodiversity and ecosystem function (bef)" ~ "biodiversity and ecosystem function",
    kw_original == "altitudinal migrant" ~ "altitudinal migration",
    kw_original == "centre-periphery hypothesis" ~ "center-periphery hypothesis",
    kw_original == "borrelia burdgorferi" ~ "borrelia burgdorferi",
    kw_original == "blue-green aglae" ~ "blue-green algae",
    kw_original == "coastal temperate rain forests" ~ "coastal temperate rainforest",
    kw_original == "c4 grassland" ~ "c4 grasslands",
    kw_original == "brachyramphus marmoratus" ~ "brachyramphus marmotus",
    kw_original == "coadapted gene complex" ~ "coadapted gene complexes",
    kw_original == "coffee agro-ecosystems" ~ "coffee agroecosystem",
    kw_original == "comparative approach" ~ "comparative approaches",
    kw_original == "capreolus capreolus" ~ "capreolus capreolus l",
    kw_original == "cedar creek natural history area, minnesota" ~ "cedar creek natural history area, minnesota, usa",
    kw_original == "branching process" ~ "branching processes",
    kw_original == "coffee agroforest" ~ "coffee agroforestry",
    kw_original == "bloflies" ~ "blowflies",
    kw_original == "carry-over effects" ~ "carryover effect",
    kw_original == "colombian amazonia" ~ "colombian amazon",
    kw_original == "complementary resource use" ~ "complementary resources",
    kw_original == "catastrophic regime shifts" ~ "catastrophic regime-shift",
    kw_original == "coefficient of additive genetic variance" ~ "coefficient of additive genetic variation",
    kw_original == "coevolutionary arm races" ~ "coevolutionary arms race",
    kw_original == "chasmagnathus granulata" ~ "chasmagnathus granulatus",
    kw_original == "bodega marine reserve, california" ~ "bodega marine reserve, california, usa",
    kw_original == "body size distribution" ~ "brood size distribution",
    kw_original == "coastal marsh" ~ "coastal marshes",
    kw_original == "community assembly by trait selection, cats" ~ "community assembly by trait selection",
    kw_original == "breeding age" ~ "breeding range",
    kw_original == "caesium" ~ "cesium",
    kw_original == "community function" ~ "community functioning",
    kw_original == "california floristic province, usa" ~ "california floristic province, usa",
    kw_original == "chihuahuan desert, new mexico" ~ "chihuahuan desert, new mexico, usa",
    kw_original == "compensatory growth" ~ "compensatory regrowth",
    kw_original == "climate-growth relation" ~ "climate-growth relationship",
    kw_original == "community dynamic model" ~ "community dynamics modeling",
    kw_original == "competition for pollination" ~ "competition for pollinators",
    # kw_original == "GxE interactions(s)"~"GxE interaction(s)",
    kw_original == "GxE interactions(s)" ~ "GxE interactions",
    kw_original == "top-down vs. bottom-up control" ~ "bottom-up vs top-down control",
    kw_original == "50-ha forest dynamics plot" ~ "50-ha plot",
    kw_original == "<bold>g</bold>-matrix" ~ "g-matrix",
    kw_original == "acacia species" ~ "acacia",
    kw_original == "africa, bat reproduction" ~ "african bat reproduction",
    # kw_original == "age-specific reproduction"~"age-specific reproduction/survival",
    # kw_original == "age-specific reproduction and survival"~"age-specific reproduction/survival",
    # kw_original == "age-specific reproductive success"~"age-specific reproduction/survival",
    # kw_original == "age-specific survival"~"age-specific reproduction/survival",
    kw_original == "aboveground annual net primary productivity" ~ "anpp",
    kw_original == "above ground net primary productivity (anpp)" ~ "anpp",
    kw_original == "aboveground net primary production (anpp)" ~ "anpp",
    kw_original == "age-dependent mortality demography" ~ "age-dependent mortality",
    kw_original == "age-from-stage modeling" ~ "age-from-stage models",
    kw_original == "abandoned agricultural lands" ~ "abandoned agriculture",
    kw_original == "abandoned cattle pastures" ~ "abandoned pastures",
    kw_original == "abandoned pasture" ~ "abandoned pastures",
    kw_original == "abandoned farmland" ~ "abandoned fields",
    # kw_original == "16s rdna"~"16s rdna/rrna",
    # kw_original == "16s rdna"~"16s rdna/rrna",
    # kw_original == "16s"~"16s rdna/rrna",
    # kw_original == "16s rdna sequencing"~"16s rdna/rrna",
    kw_original == "16s rdna sequencing" ~ "16s rdna",
    kw_original == "coevoltion" ~ "coevolution",
    # kw_original == "16s rrna"~"16s rdna/rrna",
    # kw_original == "16s rrna genes"~"16s rdna/rrna",
    kw_original == "16s rrna genes" ~ "16s rrna",
    kw_original == "16s-rrna and its gene sequencing" ~ "16s rrna",
    # kw_original == "16s-rrna and its gene sequencing"~"16s rdna/rrna",
    kw_original == "smithsonian forestgeo" ~ "bci",
    # kw_original ==  "smithsonian tropical research institute"~"bci",
    kw_original == "smithsonian tropical research institute" ~ "stri",
    # kw_original ==  "smithsonian tropical research institute, gamboa, panama"~"bci",
    kw_original == "smithsonian tropical research institute, gamboa, panama" ~ "stri, panama",
    kw_original == "site dependence" ~ "site-dependence",
    # kw_original ==  ", usa"~"",
    TRUE ~ as.character(kw_original)
  ))
#
#
#
#
# keywords_to_refine<-keywords %>%
#   select(kw_original,kw_refined_manual) %>%
#   distinct(kw_refined_manual)
#
# write_csv(keywords_to_refine,"./bibliometrics/data_intermediate/kw_to_refine2.csv")
# rm(keywords_to_refine)
#
# refined_kw<-read_csv("./bibliometrics/data_intermediate/kw-to-refine2.csv") %>%
#   rename(kw_final=kw) %>%
#   filter(!str_detect(kw_final, 'frailty model, hierarchical model,')) %>%
#   filter(!str_detect(kw_final, 'reproductive activity, sierra nevada de santa marta')) %>%
#   filter(!str_detect(kw_final, 'assemblages, extinction,')) %>%
#   filter(!str_detect(kw_final, 'photosynthesis, path')) %>%
#   filter(!str_detect(kw_final, 'georgia, usa, geostatistics'))
keywords <- keywords %>%
  rename(kw_final = kw_refined_manual) %>%
  mutate(kw_final = gsub("sensu stricto", "sensustricto", kw_final)) %>% # Only for purposes of searching...it's two words!
  mutate(kw_final = gsub("grasslands.", "grasslands.", kw_final)) %>% # Only for purposes of searching...it's two words!
  mutate(kw_final = gsub("rain forest", "rainforest", kw_final)) %>% # Only for purposes of searching...it's two words!
  mutate(kw_final = gsub("rain-forest", "rainforest", kw_final)) %>% # Only for purposes of searching...it's two words!
  mutate(kw_final = gsub("forests", "forest", kw_final)) %>%
  mutate(kw_final = gsub("savannas", "savanna", kw_final)) %>%
  mutate(kw_final = gsub("grasslands", "grassland", kw_final)) %>%
  mutate(kw_final = gsub("savannah", "savanna", kw_final)) %>%
  mutate(kw_final = gsub("savannahs", "savanna", kw_final)) %>%
  # mutate(kw_final=gsub("(usa) ","",kw_final)) %>%
  mutate(kw_final = gsub("reefs", "reef", kw_final)) %>%
  mutate(kw_final = gsub("reefcape", "reefscape", kw_final)) %>% # this corrects back the one messed up by the prior correction
  mutate(kw_final = gsub("systems", "system", kw_final)) %>%
  mutate(kw_final = case_when(
    (str_detect(kw_original, "trait mediated indirect interaction") == TRUE) ~ "trait mediated indirect interaction",
    (str_detect(kw_original, "sloss") == TRUE) ~ "sloss",
    (str_detect(kw_original, "la selva") == TRUE) ~ "la selva",
    (str_detect(kw_original, "organization for tropical studies") == TRUE) ~ "ots",
    (str_detect(kw_original, "las cruces") == TRUE) ~ "las cruces",
    (str_detect(kw_original, "palo verde") == TRUE) ~ "palo verde",
    (str_detect(kw_original, "guanacaste") == TRUE) ~ "guanacaste",
    (str_detect(kw_original, "manu national") == TRUE) ~ "manu",
    (str_detect(kw_original, "bci") == TRUE) ~ "bci",
    (str_detect(kw_original, "republic of panama") == TRUE) ~ "panama",
    (str_detect(kw_original, "cocha cashu") == TRUE) ~ "cocha cashu",
    (str_detect(kw_original, "amazonian ecuador") == TRUE) ~ "ecuadorian amazon",
    (str_detect(kw_original, "biological dynamics of forest fragments") == TRUE) ~ "bdffp",
    kw_final == "rainforest (christman island" ~ "rainforest (christman island)",
    kw_final == "rainforest (christman island" ~ "rainforest, christman island",
    # kw_final == "longleaf pine savanna, southeastern usa"~"longleaf pine savanna",
    kw_final == "ots" ~ "ots-oet",
    kw_final == "ots" ~ "ots-oet",
    kw_final == "c grassland composition 4" ~ " c4 grassland composition",
    kw_final == "c grassland composition 3" ~ " c3 grassland composition",
    kw_final == "life table response experiment, ltre" ~ "ltre",
    kw_final == "ltre analysis" ~ "ltre",
    kw_final == "δ n 15" ~ "delta n15",
    kw_final == "δ c 13" ~ "delta c13",
    kw_final == "β-diversity" ~ "beta diversity",
    kw_final == "zostera-marina" ~ "zostera marina",
    kw_final == "zostera-capricorni" ~ "zostera capricorni",
    kw_final == "zooplancton" ~ "zooplankton",
    kw_final == "zooplancton" ~ "zooplankton",
    kw_final == "alaska (usa)" ~ "alaska",
    kw_final == "alaska (usa)" ~ "usa",
    kw_final == "akaikes information criteria" ~ "aic",
    kw_final == "akaike's information criterion (aic)" ~ "aic",
    kw_final == "akaike's information criterion" ~ "aic",
    kw_final == "akaike's information criteria" ~ "aic",
    kw_final == "akaike weights" ~ "aic",
    kw_final == "akaike information criterion (aic)" ~ "aic",
    kw_final == "akaike information criteria" ~ "aic",
    kw_final == "akaike criterion" ~ "aic",
    kw_final == "akaike" ~ "aic",
    # kw_final == "manu national park, peru"~"manu national park",
    kw_final == "manu national park (peru)" ~ "manu national park, peru",
    # kw_final == "manu national park (peru)"~"manu national park",
    # kw_final == "yasuni national park, amazonian ecuador"~"yasuni national park",
    # kw_final == "yasuni ecological research station, ecuador"~"yasuni ecological research station",
    kw_final == "acer-saccharum" ~ "acer saccharum",
    kw_final == "acer-rubrum" ~ "acer rubrum",
    kw_final == "acer-negundo" ~ "acer negundo",
    kw_final == "agro-forest system" ~ "agroforest system",
    # kw_final == "tropical forest"~"tropical forest(s)",
    kw_final == "aboveground/belowground interactions" ~ "above- and belowground interactions",
    kw_final == "aboveground/belowground interactions" ~ "above- and belowground interactions",
    TRUE ~ as.character(kw_final)
  )) %>%
  mutate(kw_final = gsub("behaviour", "behavior", kw_final)) %>%
  # mutate(kw_final=gsub("behavioural","reef",kw_final)) %>%
  mutate(kw_final = gsub("colour", "color", kw_final)) %>%
  # mutate(kw_final=gsub("colouration","reef",kw_final)) %>%
  mutate(kw_final = gsub("harbour", "harbor", kw_final)) %>%
  mutate(kw_final = gsub("adultplants", "adult plant", kw_final)) %>%
  mutate(kw_final = gsub("insectivoresinsectivory", "insectivores/insectivory", kw_final)) %>%
  mutate(kw_final = gsub("densitydependence", "density dependence", kw_final)) %>%
  mutate(kw_final = gsub("moult", "molt", kw_final)) %>%
  mutate(kw_final = gsub("neighbour", "neighbor", kw_final)) %>%
  # mutate(kw_final=gsub("neighbourhood","reef",kw_final)) %>%
  mutate(kw_final = gsub("signalling", "signaling", kw_final)) %>%
  mutate(kw_final = gsub("modelling", "modeling", kw_final)) %>%
  mutate(kw_final = gsub("ageing", "aging", kw_final)) %>%
  mutate(kw_final = gsub("'", "", kw_final)) %>%
  mutate(kw_final = gsub("“", "", kw_final)) %>%
  mutate(kw_final = gsub("”", "", kw_final)) %>%
  mutate(kw_final = gsub("‘", "", kw_final)) %>%
  mutate(kw_final = gsub("’", "", kw_final)) %>%
  mutate(kw_final = gsub("“", "", kw_final)) %>%
  mutate(kw_final = gsub("’", "", kw_final)) %>%
  # mutate(kw_final=gsub(", usa","(usa)",kw_final)) %>%
  mutate(kw_final = gsub("defence", "defense", kw_final)) %>%
  filter(!str_detect(kw_final, "funding was provided by grants from the academy")) %>%
  filter(!str_detect(kw_final, "este trabajo es parte del trabajo doctoral de la autora")) %>%
  filter(!str_detect(kw_final, "atom percent excess")) %>%
  filter(!str_detect(kw_final, "fruit census fruit trap collection")) # need to fix this one

keywords <- keywords %>%
  mutate(kw_final = trimws(kw_final)) %>%
  mutate(kw_final = tolower(kw_final)) %>%
  mutate(kw_final = gsub("sensu stricto", "sensustricto", kw_final)) %>% # Only for purposes of searching...it's two words!
  mutate(kw_final = gsub("grasslands.", "grasslands.", kw_final)) %>% # Only for purposes of searching...it's two words!
  mutate(kw_final = gsub("rain forest", "rainforest", kw_final)) %>% # Only for purposes of searching...it's two words!
  mutate(kw_final = gsub("rain-forest", "rainforest", kw_final)) %>% # Only for purposes of searching...it's two words!
  mutate(kw_final = gsub("savannah", "savanna", kw_final)) %>%
  mutate(kw_final = gsub("forests", "forest", kw_final)) %>%
  mutate(kw_final = gsub("savannas", "savanna", kw_final)) %>%
  mutate(kw_final = gsub("grasslands", "grassland", kw_final)) %>%
    mutate(kw_final = gsub("savannahs", "savanna", kw_final)) %>%
  # mutate(kw_final=gsub("(usa) ","",kw_final)) %>%
  mutate(kw_final = gsub("reefs", "reef", kw_final)) %>%
  mutate(kw_final = gsub("reefcape", "reefscape", kw_final)) %>% # this corrects back the one messed up by the prior correction
  mutate(kw_final = gsub("systems", "system", kw_final)) %>%
  mutate(kw_final = case_when(
    (str_detect(kw_original, "sloss") == TRUE) ~ "sloss",
    (str_detect(kw_original, "la selva") == TRUE) ~ "la selva",
    (str_detect(kw_original, "organization for tropical studies") == TRUE) ~ "ots",
    (str_detect(kw_original, "las cruces") == TRUE) ~ "las cruces",
    (str_detect(kw_original, "palo verde") == TRUE) ~ "palo verde",
    (str_detect(kw_original, "guanacaste") == TRUE) ~ "guanacaste",
    (str_detect(kw_original, "manu national") == TRUE) ~ "manu",
    (str_detect(kw_original, "bci") == TRUE) ~ "bci",
    (str_detect(kw_original, "republic of panama") == TRUE) ~ "panama",
    (str_detect(kw_original, "cocha cashu") == TRUE) ~ "cocha cashu",
    (str_detect(kw_original, "amazonian ecuador") == TRUE) ~ "ecuadorian amazon",
    (str_detect(kw_original, "biological dynamics of forest fragments") == TRUE) ~ "bdffp",
    kw_final == "rainforest (christman island" ~ "rainforest (christman island)",
    kw_final == "rainforest (christman island" ~ "rainforest, christman island",
    # kw_final == "longleaf pine savanna, southeastern usa"~"longleaf pine savanna",
    kw_final == "ots" ~ "ots-oet",
    kw_final == "ots" ~ "ots-oet",
    kw_final == "c grassland composition 4" ~ " c4 grassland composition",
    kw_final == "c grassland composition 3" ~ " c3 grassland composition",
    kw_final == "life table response experiment, ltre" ~ "ltre",
    kw_final == "ltre analysis" ~ "ltre",
    kw_final == "δ n 15" ~ "delta n15",
    kw_final == "δ c 13" ~ "delta c13",
    kw_final == "β-diversity" ~ "beta diversity",
    kw_final == "zostera-marina" ~ "zostera marina",
    kw_final == "zostera-capricorni" ~ "zostera capricorni",
    kw_final == "zooplancton" ~ "zooplankton",
    kw_final == "zooplancton" ~ "zooplankton",
    # kw_final == "alaska (usa)" ~ "alaska",
    # kw_final == "alaska (usa)" ~ "usa",
    kw_final == "akaikes information criteria" ~ "aic",
    kw_final == "akaike's information criterion (aic)" ~ "aic",
    kw_final == "akaike's information criterion" ~ "aic",
    kw_final == "akaike's information criteria" ~ "aic",
    kw_final == "akaike weights" ~ "aic",
    kw_final == "akaike information criterion (aic)" ~ "aic",
    kw_final == "akaike information criteria" ~ "aic",
    kw_final == "akaike criterion" ~ "aic",
    kw_final == "akaike" ~ "aic",
    kw_final == "manu national park, peru"~"manu national park (peru)",
    # kw_final == "manu national park (peru)" ~ "manu national park, peru",
    # kw_final == "manu national park (peru)"~"manu national park",
    kw_final == "yasuni national park, amazonian ecuador"~"yasuni",
    # kw_final == "yasuni ecological research station, ecuador"~"yasuni ecological research station",
    kw_final == "acer-saccharum" ~ "acer saccharum",
    kw_final == "acer-rubrum" ~ "acer rubrum",
    kw_final == "acer-negundo" ~ "acer negundo",
    kw_final == "agro-forest system" ~ "agroforest system",
    kw_final == "tropical forest" ~ "tropical forest(s)",
    kw_final == "aboveground/belowground interactions" ~ "above- and belowground interactions",
    kw_final == "aboveground/belowground interactions" ~ "above- and belowground interactions",
    TRUE ~ as.character(kw_final)
  )) %>%
  mutate(kw_final = gsub("behaviour", "behavior", kw_final)) %>%
  # mutate(kw_final=gsub("behavioural","reef",kw_final)) %>%
  mutate(kw_final = gsub("colour", "color", kw_final)) %>%
  # mutate(kw_final=gsub("colouration","reef",kw_final)) %>%
  mutate(kw_final = gsub("harbour", "harbor", kw_final)) %>%
  mutate(kw_final = gsub("adultplants", "adult plant", kw_final)) %>%
  mutate(kw_final = gsub("insectivoresinsectivory", "insectivores/insectivory", kw_final)) %>%
  mutate(kw_final = gsub("densitydependence", "density dependence", kw_final)) %>%
  mutate(kw_final = gsub("moult", "molt", kw_final)) %>%
  mutate(kw_final = gsub("neighbour", "neighbor", kw_final)) %>%
  # mutate(kw_final=gsub("neighbourhood","reef",kw_final)) %>%
  mutate(kw_final = gsub("signalling", "signaling", kw_final)) %>%
  mutate(kw_final = gsub("modelling", "modeling", kw_final)) %>%
  mutate(kw_final = gsub("ageing", "aging", kw_final)) %>%
  mutate(kw_final = gsub("'", "", kw_final)) %>%
  mutate(kw_final = gsub("“", "", kw_final)) %>%
  mutate(kw_final = gsub("”", "", kw_final)) %>%
  mutate(kw_final = gsub("‘", "", kw_final)) %>%
  mutate(kw_final = gsub("’", "", kw_final)) %>%
  mutate(kw_final = gsub("“", "", kw_final)) %>%
  mutate(kw_final = gsub("’", "", kw_final)) %>%
  # mutate(kw_final=gsub(", usa","(usa)",kw_final)) %>%
  mutate(kw_final = gsub("defence", "defense", kw_final)) %>%
  filter(!str_detect(kw_final, "funding was provided by grants from the academy")) %>%
  filter(!str_detect(kw_final, "este trabajo es parte del trabajo doctoral de la autora")) %>%
  filter(!str_detect(kw_final, "atom percent excess")) %>%
  filter(!str_detect(kw_final, "fruit census fruit trap collection")) # need to fix this one




keywords$kw_final <- str_replace(keywords$kw_final, "[-]", " ")
keywords$kw_final <- str_replace(keywords$kw_final, "[.]", "")
keywords$kw_final <- str_replace(keywords$kw_final, "[.]", "")
keywords$kw_final <- str_replace(keywords$kw_final, "[.]", "")
# keywords$kw_final<-str_replace(keywords$kw_final,"[.]", "")
keywords$kw_final <- str_replace(keywords$kw_final, "[(]", "")
keywords$kw_final <- str_replace(keywords$kw_final, "[)]", "")
keywords$kw_final <- str_replace(keywords$kw_final, "tropical forest(s)", "tropical forest")
keywords$kw_final <- str_replace(keywords$kw_final, "tropical forests(s)", "tropical forest")
keywords <- keywords %>% filter(nchar(kw_final) > 0)




unique_kw_summary <- keywords %>%
  group_by(kw_final) %>%
  tally() %>%
  arrange(desc(n))
unique_kw_summary


# out to open refine ------------------------------------------------------



write_csv(unique_kw_summary, "./bibliometrics/data_intermediate/kw-to-refine3.csv")


# read open refine back in ------------------------------------------------

# -
# –

# replace hyphens/emdash
keywords <- keywords %>%
  mutate(kw_original = gsub("–", "-", kw_original)) %>%
  mutate(kw_final = gsub("–", "-", kw_final)) %>%
  mutate(kw_final = gsub("/", "-", kw_final)) %>%
  mutate(kw_final = gsub(" - ", "-", kw_final))





# lets try again ----------------------------------------------------------

  keywords$kw_final<-str_replace(keywords$kw_final, "life history", "life-history")
  keywords$kw_final<-str_replace(keywords$kw_final, "trade offs", "tradeoff")
  # kw_final =="trade offs"~"tradeoffs")
  keywords$kw_final<-str_replace(keywords$kw_final, "trade off", "tradeoff")
  keywords$kw_final<-str_replace(keywords$kw_final, "life history evolution", "life-history evolution")
  keywords$kw_final<-str_replace(keywords$kw_final, "meta analysis", "metaanalysis")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant herbivore interactions", "plant-herbivore interaction")
  # kw_final =="plant herbivore interactions"~"plant-herbivore interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "predator prey interactions", "predator-prey interaction")
  # kw_final =="predator prey interactions"~"predator-prey interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "life history traits", "life-history trait")
  # kw_final =="life history traits"~"life-history traits")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant insect interactions", "plant-insect interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "self fertilization", "self-fertilization")
  keywords$kw_final<-str_replace(keywords$kw_final, "home range", "homerange")
  keywords$kw_final<-str_replace(keywords$kw_final, "frequency dependent selection", "frequency-dependent selection")
  keywords$kw_final<-str_replace(keywords$kw_final, "predator prey", "predator-prey")
  keywords$kw_final<-str_replace(keywords$kw_final, "phylogenetic comparative methods", "comparative phylogenetic method")
  # kw_final =="phylogenetic comparative methods"~"comparative phylogenetic methods")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant soil belowground interactions", "plant-soil belowground interaction")
  # kw_final =="plant soil belowground interactions"~"plant-soil belowground interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "individual based model", "individual-based model")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant animal interactions", "plant-animal interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant climate interactions", "plant-climate interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "host parasite interactions", "host-parasite interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "méxico", "mexico")
  keywords$kw_final<-str_replace(keywords$kw_final, "salt marsh", "saltmarsh")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant plant interactions", "plant interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "land use", "land-use")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant-herbivore interactions", "plant-herbivore interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "host parasite interaction", "host-parasite interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "capture recapture", "capture-recapture")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant diversity", "diversity plant")
  keywords$kw_final<-str_replace(keywords$kw_final, "mark recapture", "mark-recapture")
  keywords$kw_final<-str_replace(keywords$kw_final, "eco evolutionary dynamics", "eco-evolutionary dynamics")
  keywords$kw_final<-str_replace(keywords$kw_final, "isolation by distance", "isolation-by-distance")
  keywords$kw_final<-str_replace(keywords$kw_final, "top down control", "top-down control")
  keywords$kw_final<-str_replace(keywords$kw_final, "life history trade-offs", "life-history tradeoffs")
  keywords$kw_final<-str_replace(keywords$kw_final, "species area relationship", "species-area relationship")
  keywords$kw_final<-str_replace(keywords$kw_final, "life span", "lifespan")
  keywords$kw_final<-str_replace(keywords$kw_final, "genotype by-environment interaction", "gxe")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant functional traits", "functional plant traits")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant-plant interactions", "ant-plant interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "predator prey interaction", "predator-prey interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "janzen connell hypothesis", "janzen-connell hypothesis")
  keywords$kw_final<-str_replace(keywords$kw_final, "el niño", "enso")
  keywords$kw_final<-str_replace(keywords$kw_final, "host parasite coevolution", "host-parasite coevolution")
  keywords$kw_final<-str_replace(keywords$kw_final, "long distance dispersal", "long-distance dispersal")
  keywords$kw_final<-str_replace(keywords$kw_final, "parent offspring conflict", "parent-offspring conflict")
  keywords$kw_final<-str_replace(keywords$kw_final, "insect plant interactions", "plant-insect interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "tropical montane forest", "montane tropical forest")
  keywords$kw_final<-str_replace(keywords$kw_final, "top down", "top-down")
  keywords$kw_final<-str_replace(keywords$kw_final, "host parasite", "host-parasite")
  keywords$kw_final<-str_replace(keywords$kw_final, "co evolution", "coevolution")
  keywords$kw_final<-str_replace(keywords$kw_final, "host pathogen interactions", "host-pathogen interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "old growth forest", "old-growth forest")
  keywords$kw_final<-str_replace(keywords$kw_final, "stage structure", "age-stage structure")
  keywords$kw_final<-str_replace(keywords$kw_final, "ant plant interactions", "ant-plant interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "male male competition", "male-male competition")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant soil feedback", "plant-soil feedback")
  keywords$kw_final<-str_replace(keywords$kw_final, "predator prey dynamics", "predator-prey dynamics")
  keywords$kw_final<-str_replace(keywords$kw_final, "co occurrence", "cooccurrence")
  keywords$kw_final<-str_replace(keywords$kw_final, "population genetic structure", "genetic population structure")
  keywords$kw_final<-str_replace(keywords$kw_final, "predator-prey interactions", "predator-prey interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant herbivore interaction", "plant-herbivore interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant insect interaction", "insect-plant interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "state space model", "state-space model")
  keywords$kw_final<-str_replace(keywords$kw_final, "15 n", "15n")
  keywords$kw_final<-str_replace(keywords$kw_final, "artemisia tridentata", "artemisia tridentate")
  keywords$kw_final<-str_replace(keywords$kw_final, "biodiversity ecosystem functioning", "biodiversity-ecosystem functioning")
  keywords$kw_final<-str_replace(keywords$kw_final, "elevated co 2", "elevated co2")
  keywords$kw_final<-str_replace(keywords$kw_final, "f st", "fst")
  keywords$kw_final<-str_replace(keywords$kw_final, "poeciliidae", "poeciilidae")
  keywords$kw_final<-str_replace(keywords$kw_final, "life history trade-off", "life-history trade-off")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant animal interaction", "plant-animal interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "resources", "resource")
  keywords$kw_final<-str_replace(keywords$kw_final, "self organization", "self-organization")
  keywords$kw_final<-str_replace(keywords$kw_final, "source sink", "source-sink")
  keywords$kw_final<-str_replace(keywords$kw_final, "biodiversity hotspot", "biodiversity hotspots")
  keywords$kw_final<-str_replace(keywords$kw_final, "el niño southern oscillation", "enso")
  keywords$kw_final<-str_replace(keywords$kw_final, "extra pair paternity", "extrapair paternity")
  keywords$kw_final<-str_replace(keywords$kw_final, "phylogenetic comparative method", "comparative phylogenetic method")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant pollinator interactions", "plant-pollinator interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "alces alces", "alces")
  keywords$kw_final<-str_replace(keywords$kw_final, "life table response experiments", "life-table response experiments")
  keywords$kw_final<-str_replace(keywords$kw_final, "long term monitoring", "long-term monitoring")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant-soil interactions", "plant-soil interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "radio tracking", "radiotracking")
  keywords$kw_final<-str_replace(keywords$kw_final, "scatter hoarding", "scatterhoarding")
  keywords$kw_final<-str_replace(keywords$kw_final, "selection natural", "natural selection")
  keywords$kw_final<-str_replace(keywords$kw_final, "species distribution modeling", "modeling species distribution")
  keywords$kw_final<-str_replace(keywords$kw_final, "tropical eastern pacific", "eastern tropical pacific")
  keywords$kw_final<-str_replace(keywords$kw_final, "leaf life span", "leaf lifespan")
  keywords$kw_final<-str_replace(keywords$kw_final, "long term data", "long-term data")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant soil interactions", "plant-soil interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant-animal interactions", "plant-animal interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "trait mediated indirect interactions", "indirect trait mediated interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "biodiversity ecosystem function", "biodiversity-ecosystem function")
  keywords$kw_final<-str_replace(keywords$kw_final, "bird song", "birdsong")
  keywords$kw_final<-str_replace(keywords$kw_final, "enemy free space", "enemy-free space")
  keywords$kw_final<-str_replace(keywords$kw_final, "evo devo", "evo-devo")
  keywords$kw_final<-str_replace(keywords$kw_final, "negative frequency dependent selection", "negative frequency-dependent selection")
  keywords$kw_final<-str_replace(keywords$kw_final, "non timber forest products", "nontimber forest products")
  keywords$kw_final<-str_replace(keywords$kw_final, "semi arid", "semiarid")
  keywords$kw_final<-str_replace(keywords$kw_final, "co 2", "co2")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant soil feedbacks", "plant-soil feedbacks")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant-pollinator interactions", "plant-pollinator interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "radio telemetry", "radiotelemetry")
  keywords$kw_final<-str_replace(keywords$kw_final, "south east asia", "southeast asia")
  keywords$kw_final<-str_replace(keywords$kw_final, "tree line", "treeline")
  keywords$kw_final<-str_replace(keywords$kw_final, "ant plant mutualism", "ant-plant mutualism")
  keywords$kw_final<-str_replace(keywords$kw_final, "giving up density", "giving-up density")
  keywords$kw_final<-str_replace(keywords$kw_final, "janzen-connell hypothesis", "janzen-connell hypothesis")
  keywords$kw_final<-str_replace(keywords$kw_final, "spatiotemporal variation", "spatio-temporal variation")
  keywords$kw_final<-str_replace(keywords$kw_final, "agent based model", "agent-based model")
  keywords$kw_final<-str_replace(keywords$kw_final, "bumble bee", "bumblebee")
  keywords$kw_final<-str_replace(keywords$kw_final, "consumer resource interactions", "consumer-resource interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "galápagos", "galapagos")
  keywords$kw_final<-str_replace(keywords$kw_final, "gene for-gene", "gene-for-gene")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant microbe interactions", "plant-microbe interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant pathogen interactions", "plant-pathogen interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant-insect interactions", "plant-insect interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "genotype by-environment interactions", "gxe")
  keywords$kw_final<-str_replace(keywords$kw_final, "gonadosomatic index", "gonado-somatic index")
  keywords$kw_final<-str_replace(keywords$kw_final, "host-parasite interactions", "host-parasite interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "invasive alien species", "alien invasive species")
  keywords$kw_final<-str_replace(keywords$kw_final, "stingless bees", "stingless bee")
  keywords$kw_final<-str_replace(keywords$kw_final, "biodiversity-ecosystem functioning", "biodiversity-ecosystem functioning")
  keywords$kw_final<-str_replace(keywords$kw_final, "host parasitoid interactions", "host-parasitoid interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant pollinator interaction", "plant-pollinator interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant secondary metabolites", "secondary plant metabolites")
  keywords$kw_final<-str_replace(keywords$kw_final, "temperature dependent sex determination", "temperature-dependent sex determination")
  keywords$kw_final<-str_replace(keywords$kw_final, "tri trophic interactions", "tritrophic interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "anti predator behavior", "antipredator behavior")
  keywords$kw_final<-str_replace(keywords$kw_final, "consumer resource dynamics", "consumer-resource dynamics")
  keywords$kw_final<-str_replace(keywords$kw_final, "dry tropical forest", "tropical dry forest")
  keywords$kw_final<-str_replace(keywords$kw_final, "host pathogen interaction", "host-pathogen interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "multi trophic interactions", "multitrophic interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "post copulatory sexual selection", "postcopulatory sexual selection")
  keywords$kw_final<-str_replace(keywords$kw_final, "post zygotic isolation", "postzygotic isolation")
  keywords$kw_final<-str_replace(keywords$kw_final, "pre dispersal seed predation", "predispersal seed predation")
  keywords$kw_final<-str_replace(keywords$kw_final, "predator prey model", "predator-prey model")
  keywords$kw_final<-str_replace(keywords$kw_final, "predator-prey interaction", "predator-prey interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "várzea", "varzea")
  keywords$kw_final<-str_replace(keywords$kw_final, "alternate stable states", "alternate stable state")
  keywords$kw_final<-str_replace(keywords$kw_final, "co infection", "coinfection")
  keywords$kw_final<-str_replace(keywords$kw_final, "insect plant interaction", "insect-plant interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "isolation by-distance", "isolation-by-distance")
  keywords$kw_final<-str_replace(keywords$kw_final, "müllerian mimicry", "mullerian mimicry")
  keywords$kw_final<-str_replace(keywords$kw_final, "non native species", "nonnative species")
  keywords$kw_final<-str_replace(keywords$kw_final, "phylogenetic comparative analysis", "comparative phylogenetic analysis")
  keywords$kw_final<-str_replace(keywords$kw_final, "predator-prey", "predator-prey")
  keywords$kw_final<-str_replace(keywords$kw_final, "resource pulses", "resource pulse")
  keywords$kw_final<-str_replace(keywords$kw_final, "species tree", "tree species")
  keywords$kw_final<-str_replace(keywords$kw_final, "trait mediated indirect interaction", "trait-mediated indirect interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "within host dynamics", "within-host dynamics")
  keywords$kw_final<-str_replace(keywords$kw_final, "distance decay", "distance-decay")
  keywords$kw_final<-str_replace(keywords$kw_final, "eco evolutionary feedback", "ecoevolutionary feedback")
  keywords$kw_final<-str_replace(keywords$kw_final, "floodplain forest", "flood plain forest")
  keywords$kw_final<-str_replace(keywords$kw_final, "genotype by environment interaction", "gxe")
  keywords$kw_final<-str_replace(keywords$kw_final, "life history trade offs", "life-history tradeoffs")
  keywords$kw_final<-str_replace(keywords$kw_final, "non consumptive effects", "nonconsumptive effects")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant-herbivore interaction", "plant-herbivore interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "sexual signals", "sexual signal")
  keywords$kw_final<-str_replace(keywords$kw_final, "species co occurrence", "species cooccurrence")
  keywords$kw_final<-str_replace(keywords$kw_final, "bayesian hierarchical model", "hierarchical bayesian model")
  keywords$kw_final<-str_replace(keywords$kw_final, "bumble bees", "bumblebees")
  keywords$kw_final<-str_replace(keywords$kw_final, "correlated response", "correlated responses")
  keywords$kw_final<-str_replace(keywords$kw_final, "el nino", "enso")
  keywords$kw_final<-str_replace(keywords$kw_final, "life history trade off", "life-history trade-off")
  keywords$kw_final<-str_replace(keywords$kw_final, "non additive effects", "nonadditive effects")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant-pathogen interactions", "plant-pathogen interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "radseq", "rad seq")
  keywords$kw_final<-str_replace(keywords$kw_final, "rattus rattus", "rattus")
  keywords$kw_final<-str_replace(keywords$kw_final, "signaling-courtship", "signaling-courtship")
  keywords$kw_final<-str_replace(keywords$kw_final, "yucatán", "yucatan")
  keywords$kw_final<-str_replace(keywords$kw_final, "anti herbivore defense", "antiherbivore defense")
  keywords$kw_final<-str_replace(keywords$kw_final, "côte divoire", "ivory coast")
  keywords$kw_final<-str_replace(keywords$kw_final, "eastern himalaya", "eastern himalayas")
  keywords$kw_final<-str_replace(keywords$kw_final, "host pathogen", "host-pathogen")
  keywords$kw_final<-str_replace(keywords$kw_final, "host-parasite interaction", "host-parasite interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "human wildlife conflict", "human-wildlife conflict")
  keywords$kw_final<-str_replace(keywords$kw_final, "long term", "longterm")
  keywords$kw_final<-str_replace(keywords$kw_final, "lotka volterra", "lotka-volterra")
  keywords$kw_final<-str_replace(keywords$kw_final, "n 15", "15n")
  keywords$kw_final<-str_replace(keywords$kw_final, "non structural carbohydrates", "nonstructural carbohydrates")
  keywords$kw_final<-str_replace(keywords$kw_final, "ornstein uhlenbeck", "ornstein-uhlenbeck")
  keywords$kw_final<-str_replace(keywords$kw_final, "pair correlation function", "pair-correlation function")
  keywords$kw_final<-str_replace(keywords$kw_final, "pesticides", "pesticide")
  keywords$kw_final<-str_replace(keywords$kw_final, "post dispersal seed predation", "postdispersal seed predation")
  keywords$kw_final<-str_replace(keywords$kw_final, "queen worker conflict", "queen-worker conflict")
  keywords$kw_final<-str_replace(keywords$kw_final, "species area relationships", "species-area relationships")
  keywords$kw_final<-str_replace(keywords$kw_final, "stable carbon isotopes", "carbon stable isotopes")
  keywords$kw_final<-str_replace(keywords$kw_final, "wet tropical forest", "tropical wet forest")
  keywords$kw_final<-str_replace(keywords$kw_final, "aboveground belowground interactions", "aboveground-belowground interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "aboveground-belowground interactions", "aboveground-belowground interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "consumer resource interaction", "consumer-resource interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "dead wood", "deadwood")
  keywords$kw_final<-str_replace(keywords$kw_final, "density mediated indirect interactions", "density-mediated indirect interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "ecoimmunology", "eco immunology")
  keywords$kw_final<-str_replace(keywords$kw_final, "evolutionary transitions", "evolutionary transition")
  keywords$kw_final<-str_replace(keywords$kw_final, "forest tundra", "forest-tundra")
  keywords$kw_final<-str_replace(keywords$kw_final, "honey bee", "honeybee")
  keywords$kw_final<-str_replace(keywords$kw_final, "host feeding", "host-feeding")
  keywords$kw_final<-str_replace(keywords$kw_final, "leaf fall", "leaf-fall")
  keywords$kw_final<-str_replace(keywords$kw_final, "lotka volterra model", "lotka-volterra model")
  keywords$kw_final<-str_replace(keywords$kw_final, "male competition", "male-male competition")
  keywords$kw_final<-str_replace(keywords$kw_final, "marine reserves", "marine reserve")
  keywords$kw_final<-str_replace(keywords$kw_final, "meta ecosystem", "metaecosystem")
  keywords$kw_final<-str_replace(keywords$kw_final, "non native", "nonnative")
  keywords$kw_final<-str_replace(keywords$kw_final, "ornstein uhlenbeck model", "ornstein-uhlenbeck model")
  keywords$kw_final<-str_replace(keywords$kw_final, "safe sites", "safe site")
  keywords$kw_final<-str_replace(keywords$kw_final, "slash and-burn agriculture", "slash and burn agriculture")
  keywords$kw_final<-str_replace(keywords$kw_final, "stage structured models", "stage-structured models")
  keywords$kw_final<-str_replace(keywords$kw_final, "time lag", "lag time")
  keywords$kw_final<-str_replace(keywords$kw_final, "tree fall gaps", "treefall gaps")
  keywords$kw_final<-str_replace(keywords$kw_final, "tropical soils", "tropical soil")
  keywords$kw_final<-str_replace(keywords$kw_final, "yasuní national park", "yasuni")
  keywords$kw_final<-str_replace(keywords$kw_final, "yasuni national park", "yasuni")
  keywords$kw_final<-str_replace(keywords$kw_final, "bison bison", "bison")
  keywords$kw_final<-str_replace(keywords$kw_final, "capture-mark-recapture", "capture mark-recapture")
  keywords$kw_final<-str_replace(keywords$kw_final, "carry over effect", "carryover effect")
  keywords$kw_final<-str_replace(keywords$kw_final, "competition colonization trade-off", "competition-colonization trade-off")
  keywords$kw_final<-str_replace(keywords$kw_final, "consumer resource model", "consumer-resource model")
  keywords$kw_final<-str_replace(keywords$kw_final, "cormack jolly-seber model", "cormack-jolly-seber model")
  keywords$kw_final<-str_replace(keywords$kw_final, "cost benefit analysis", "cost-benefit analysis")
  keywords$kw_final<-str_replace(keywords$kw_final, "critical transitions", "critical transition")
  keywords$kw_final<-str_replace(keywords$kw_final, "eco evolutionary feedbacks", "eco-evolutionary feedbacks")
  keywords$kw_final<-str_replace(keywords$kw_final, "food resources", "food resource")
  keywords$kw_final<-str_replace(keywords$kw_final, "forest types", "forest type")
  keywords$kw_final<-str_replace(keywords$kw_final, "honey bees", "honeybees")
  keywords$kw_final<-str_replace(keywords$kw_final, "host parasite dynamics", "host-parasite dynamics")
  keywords$kw_final<-str_replace(keywords$kw_final, "host pathogen dynamics", "host-pathogen dynamics")
  keywords$kw_final<-str_replace(keywords$kw_final, "host shifts", "host shift")
  keywords$kw_final<-str_replace(keywords$kw_final, "host-pathogen interactions", "host-pathogen interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "leaf miner", "leafminer")
  keywords$kw_final<-str_replace(keywords$kw_final, "mixed grass prairie", "mixed-grass prairie")
  keywords$kw_final<-str_replace(keywords$kw_final, "panamá", "panama")
  keywords$kw_final<-str_replace(keywords$kw_final, "predator prey system", "prey-predator system")
  keywords$kw_final<-str_replace(keywords$kw_final, "predator-prey dynamics", "predator-prey dynamics")
  keywords$kw_final<-str_replace(keywords$kw_final, "presence absence", "presence-absence")
  keywords$kw_final<-str_replace(keywords$kw_final, "páramo", "paramo")
  keywords$kw_final<-str_replace(keywords$kw_final, "q st", "qst")
  keywords$kw_final<-str_replace(keywords$kw_final, "selection experimental", "experimental selection")
  keywords$kw_final<-str_replace(keywords$kw_final, "size structured populations", "size-structured populations")
  keywords$kw_final<-str_replace(keywords$kw_final, "soil resources", "soil resource")
  keywords$kw_final<-str_replace(keywords$kw_final, "spatial capture-recapture", "spatial capture-recapture")
  keywords$kw_final<-str_replace(keywords$kw_final, "species abundance distributions", "species-abundance distributions")
  keywords$kw_final<-str_replace(keywords$kw_final, "species area", "species-area")
  keywords$kw_final<-str_replace(keywords$kw_final, "species area curve", "species-area curve")
  keywords$kw_final<-str_replace(keywords$kw_final, "species area curves", "species-area curve")
  keywords$kw_final<-str_replace(keywords$kw_final, "tree grass interactions", "tree-grass interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "tri trophic interaction", "tritrophic interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "tropical lowland rainforest", "lowland tropical rainforest")
  keywords$kw_final<-str_replace(keywords$kw_final, "above ground biomass", "aboveground biomass")
  keywords$kw_final<-str_replace(keywords$kw_final, "allocation trade offs", "allocation tradeoffs")
  keywords$kw_final<-str_replace(keywords$kw_final, "ancestral states", "ancestral state")
  keywords$kw_final<-str_replace(keywords$kw_final, "bayesian hierarchical models", "hierarchical bayesian models")
  keywords$kw_final<-str_replace(keywords$kw_final, "behavioral responses", "behavioral response")
  keywords$kw_final<-str_replace(keywords$kw_final, "benthic pelagic coupling", "benthic-pelagic coupling")
  keywords$kw_final<-str_replace(keywords$kw_final, "bio logging", "biologging")
  keywords$kw_final<-str_replace(keywords$kw_final, "birth weight", "birthweight")
  keywords$kw_final<-str_replace(keywords$kw_final, "consumer resource", "consumer-resource")
  keywords$kw_final<-str_replace(keywords$kw_final, "deer mice", "deermice")
  keywords$kw_final<-str_replace(keywords$kw_final, "el nino southern oscillation", "enso")
  keywords$kw_final<-str_replace(keywords$kw_final, "fast slow continuum", "fast-slow continuum")
  keywords$kw_final<-str_replace(keywords$kw_final, "forest fires", "forest fire")
  keywords$kw_final<-str_replace(keywords$kw_final, "geographic information system gis", "gis")
  keywords$kw_final<-str_replace(keywords$kw_final, "heterozygosity fitness correlation", "heterozygosity-fitness correlation")
  keywords$kw_final<-str_replace(keywords$kw_final, "host parasitoid", "host-parasitoid")
  keywords$kw_final<-str_replace(keywords$kw_final, "leaf functional traits", "functional leaf traits")
  keywords$kw_final<-str_replace(keywords$kw_final, "length weight relationship", "length-weight relationship")
  keywords$kw_final<-str_replace(keywords$kw_final, "parasite host interactions", "host-parasite interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "perú", "peru")
  keywords$kw_final<-str_replace(keywords$kw_final, "post fledging survival", "postfledging survival")
  keywords$kw_final<-str_replace(keywords$kw_final, "range expansions", "range expansion")
  keywords$kw_final<-str_replace(keywords$kw_final, "reproductive trade offs", "reproductive tradeoffs")
  keywords$kw_final<-str_replace(keywords$kw_final, "selection sexual", "sexual selection")
  keywords$kw_final<-str_replace(keywords$kw_final, "semi deciduous forest", "semideciduous forest")
  keywords$kw_final<-str_replace(keywords$kw_final, "spatio temporal variation", "spatio-temporal variation")
  keywords$kw_final<-str_replace(keywords$kw_final, "above belowground interactions", "above-belowground interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "allochthonous resources", "allochthonous resource")
  keywords$kw_final<-str_replace(keywords$kw_final, "alpine tree line", "alpine treeline")
  keywords$kw_final<-str_replace(keywords$kw_final, "ant-plant interactions", "ant-plant interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "aquatic terrestrial linkages", "terrestrial-aquatic linkages")
  keywords$kw_final<-str_replace(keywords$kw_final, "by catch", "bycatch")
  keywords$kw_final<-str_replace(keywords$kw_final, "carbon nutrient balance", "c:n balance")
  keywords$kw_final<-str_replace(keywords$kw_final, "co adaptation", "coadaptation")
  keywords$kw_final<-str_replace(keywords$kw_final, "co existence", "coexistence")
  keywords$kw_final<-str_replace(keywords$kw_final, "co limitation", "colimitation")
  keywords$kw_final<-str_replace(keywords$kw_final, "condition dependent dispersal", "condition-dependent dispersal")
  keywords$kw_final<-str_replace(keywords$kw_final, "epichloë", "epichloe")
  keywords$kw_final<-str_replace(keywords$kw_final, "evolution of co operation", "evolution of cooperation")
  keywords$kw_final<-str_replace(keywords$kw_final, "f  st", "fst")
  keywords$kw_final<-str_replace(keywords$kw_final, "g x e", "gxe")
  keywords$kw_final<-str_replace(keywords$kw_final, "genotype by-genotype interactions", "gxe")
  keywords$kw_final<-str_replace(keywords$kw_final, "genotyping by-sequencing", "genotyping by sequencing")
  keywords$kw_final<-str_replace(keywords$kw_final, "helpers at-the-nest", "helpers at the nest")
  keywords$kw_final<-str_replace(keywords$kw_final, "host parasitoid dynamics", "host-parasitoid dynamics")
  keywords$kw_final<-str_replace(keywords$kw_final, "host-parasite", "host-parasite")
  keywords$kw_final<-str_replace(keywords$kw_final, "hotspot", "hotspots")
  keywords$kw_final<-str_replace(keywords$kw_final, "life stages", "life stage")
  keywords$kw_final<-str_replace(keywords$kw_final, "long distance migration", "long-distance migration")
  keywords$kw_final<-str_replace(keywords$kw_final, "major transitions", "major transition")
  keywords$kw_final<-str_replace(keywords$kw_final, "mark release-recapture", "mark-release-recapture")
  keywords$kw_final<-str_replace(keywords$kw_final, "mark-recapture", "mark-recapture")
  keywords$kw_final<-str_replace(keywords$kw_final, "menidia menidia", "menidia")
  keywords$kw_final<-str_replace(keywords$kw_final, "p st", "pst")
  keywords$kw_final<-str_replace(keywords$kw_final, "pace of-life", "pace of life")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant-animal interaction", "plant-animal interaction")
  # keywords$kw_final<-str_replace(keywords$kw_final, "r", "r* theory")
  keywords$kw_final<-str_replace(keywords$kw_final, "r and k-selection", "r and k selection")
  keywords$kw_final<-str_replace(keywords$kw_final, "r\\* theory", "rstar theory")
  keywords$kw_final<-str_replace(keywords$kw_final, "r\\* rule", "rstar rule")
  keywords$kw_final<-str_replace(keywords$kw_final, "r\\*", "rstar theory")
  keywords$kw_final<-str_replace(keywords$kw_final, "regional species pool", "regional species pools")
  keywords$kw_final<-str_replace(keywords$kw_final, "rutilus rutilus", "rutilus")
  keywords$kw_final<-str_replace(keywords$kw_final, "sea stars", "seastars")
  keywords$kw_final<-str_replace(keywords$kw_final, "semi arid ecosystem", "semiarid ecosystem")
  keywords$kw_final<-str_replace(keywords$kw_final, "space for-time substitution", "space-for-time substitution")
  keywords$kw_final<-str_replace(keywords$kw_final, "species assemblages", "species assemblage")
  keywords$kw_final<-str_replace(keywords$kw_final, "species ranges", "species range")
  keywords$kw_final<-str_replace(keywords$kw_final, "trait mediated interaction", "trait-mediated interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "tropical semi deciduous forest", "tropical semideciduous forest")
  keywords$kw_final<-str_replace(keywords$kw_final, "vulpes vulpes", "vulpes")
  keywords$kw_final<-str_replace(keywords$kw_final, "aboveground belowground linkages", "aboveground-belowground linkages")
  keywords$kw_final<-str_replace(keywords$kw_final, "agro ecosystem", "agroecosystem")
  keywords$kw_final<-str_replace(keywords$kw_final, "ant aphid mutualism", "ant-aphid mutualism")
  keywords$kw_final<-str_replace(keywords$kw_final, "carbon nutrient balance hypothesis", "carbon:nutrient balance hypothesis")
  keywords$kw_final<-str_replace(keywords$kw_final, "consumer-resource interactions", "consumer-resource interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "curaçao", "curacao")
  keywords$kw_final<-str_replace(keywords$kw_final, "developmental mode", "developmental model")
  keywords$kw_final<-str_replace(keywords$kw_final, "diversity-productivity relationship", "diversity-productivity relationship")
  keywords$kw_final<-str_replace(keywords$kw_final, "female competition", "female-female competition")
  keywords$kw_final<-str_replace(keywords$kw_final, "genotype by environment", "gxe")
  keywords$kw_final<-str_replace(keywords$kw_final, "gxe interaction", "gxe")
  keywords$kw_final<-str_replace(keywords$kw_final, "habitat shifts", "habitat shift")
  keywords$kw_final<-str_replace(keywords$kw_final, "hill robertson interference", "hill-robertson interference")
  keywords$kw_final<-str_replace(keywords$kw_final, "host-microbe interactions", "host-microbe interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "igapó", "igapo")
  keywords$kw_final<-str_replace(keywords$kw_final, "individual based simulation", "individual-based simulation")
  keywords$kw_final<-str_replace(keywords$kw_final, "insect plant relationships", "insect-plant relationships")
  keywords$kw_final<-str_replace(keywords$kw_final, "insect-plant interactions", "insect-plant interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "isolation by environment", "isolation-by-environment")
  keywords$kw_final<-str_replace(keywords$kw_final, "leafminers", "leaf miners")
  keywords$kw_final<-str_replace(keywords$kw_final, "limiting resources", "limiting resource")
  keywords$kw_final<-str_replace(keywords$kw_final, "lévy flight", "levy flight")
  keywords$kw_final<-str_replace(keywords$kw_final, "lévy walk", "levy walk")
  keywords$kw_final<-str_replace(keywords$kw_final, "meta community", "metacommunity")
  keywords$kw_final<-str_replace(keywords$kw_final, "micro organisms", "microorganisms")
  keywords$kw_final<-str_replace(keywords$kw_final, "multistate mark recapture", "multistate capture mark-recapture")
  keywords$kw_final<-str_replace(keywords$kw_final, "mus musculus musculus", "mus musculus")
  keywords$kw_final<-str_replace(keywords$kw_final, "non breeding season", "nonbreeding season")
  keywords$kw_final<-str_replace(keywords$kw_final, "non equilibrium", "nonequilibrium")
  keywords$kw_final<-str_replace(keywords$kw_final, "non indigenous", "nonindigenous")
  keywords$kw_final<-str_replace(keywords$kw_final, "non lethal effects", "nonlethal effects")
  keywords$kw_final<-str_replace(keywords$kw_final, "north east india", "northeast india")
  keywords$kw_final<-str_replace(keywords$kw_final, "parasite interactions", "parasite-parasite interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant plant interaction", "plant-plant interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant pollinator network", "plant-pollinator network")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant soil interaction", "plant-soil interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "post copulatory", "post-copulatory")
  keywords$kw_final<-str_replace(keywords$kw_final, "predator prey theory", "predator-prey theory")
  keywords$kw_final<-str_replace(keywords$kw_final, "presence absence data", "presence-absence data")
  keywords$kw_final<-str_replace(keywords$kw_final, "pungitius pungitius", "pungitius")
  keywords$kw_final<-str_replace(keywords$kw_final, "robertson price identity", "robertson-price identity")
  keywords$kw_final<-str_replace(keywords$kw_final, "selection  natural", "natural selection")
  keywords$kw_final<-str_replace(keywords$kw_final, "selection group-kin", "group-kin selection  ")
  keywords$kw_final<-str_replace(keywords$kw_final, "semi arid savanna", "semiarid savanna")
  keywords$kw_final<-str_replace(keywords$kw_final, "slow fast continuum", "fast-slow continuum")
  keywords$kw_final<-str_replace(keywords$kw_final, "soil borne pathogens", "soilborne pathogens")
  keywords$kw_final<-str_replace(keywords$kw_final, "spatial capture recapture", "spatial capture-recapture")
  keywords$kw_final<-str_replace(keywords$kw_final, "spatially explicit capture recapture", "spatially explicit capture-recapture")
  keywords$kw_final<-str_replace(keywords$kw_final, "sub arctic", "subarctic")
  keywords$kw_final<-str_replace(keywords$kw_final, "successional stages", "successional stage")
  keywords$kw_final<-str_replace(keywords$kw_final, "temperature-size rule", "temperature size rule")
  keywords$kw_final<-str_replace(keywords$kw_final, "trivers willard hypothesis", "trivers-willard hypothesis")
  keywords$kw_final<-str_replace(keywords$kw_final, "tropical seasonal forest", "seasonal tropical forest")
  keywords$kw_final<-str_replace(keywords$kw_final, "visual signals", "visual signal")
  keywords$kw_final<-str_replace(keywords$kw_final, "waste water", "wastewater")
  keywords$kw_final<-str_replace(keywords$kw_final, "15 n isotope", "15n isotope")
  keywords$kw_final<-str_replace(keywords$kw_final, "abundance distribution", "distribution-abundance")
  keywords$kw_final<-str_replace(keywords$kw_final, "animal plant interactions", "plant-animal interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "arid lands", "aridlands")
  keywords$kw_final<-str_replace(keywords$kw_final, "bio indicator", "bioindicator")
  keywords$kw_final<-str_replace(keywords$kw_final, "boosted regression trees", "boosted regression tree")
  keywords$kw_final<-str_replace(keywords$kw_final, "cave fish", "cavefish")
  keywords$kw_final<-str_replace(keywords$kw_final, "chocó", "choco")
  keywords$kw_final<-str_replace(keywords$kw_final, "community phylogenetic structure", "phylogenetic community structure")
  keywords$kw_final<-str_replace(keywords$kw_final, "compensatory mutations", "compensatory mutation")
  keywords$kw_final<-str_replace(keywords$kw_final, "competition colonization", "competition-colonization")
  keywords$kw_final<-str_replace(keywords$kw_final, "condensed tannin", "condensed tannins")
  keywords$kw_final<-str_replace(keywords$kw_final, "cormack jolly-seber", "cormack-jolly-seber")
  keywords$kw_final<-str_replace(keywords$kw_final, "cost benefit", "cost-benefit")
  keywords$kw_final<-str_replace(keywords$kw_final, "counter gradient variation", "countergradient variation")
  keywords$kw_final<-str_replace(keywords$kw_final, "creosotebush", "creosote bush")
  keywords$kw_final<-str_replace(keywords$kw_final, "ctmax", "ct max")
  keywords$kw_final<-str_replace(keywords$kw_final, "data integration for population models special feature", "special feature: data integration for population models")
  keywords$kw_final<-str_replace(keywords$kw_final, "dispersal vectors", "dispersal vector")
  keywords$kw_final<-str_replace(keywords$kw_final, "diversity stability", "diversity-stability")
  keywords$kw_final<-str_replace(keywords$kw_final, "drip tips", "driptips")
  keywords$kw_final<-str_replace(keywords$kw_final, "el niño southern oscillation enso", "enso")
  keywords$kw_final<-str_replace(keywords$kw_final, "el niño-southern oscillation", "enso")
  keywords$kw_final<-str_replace(keywords$kw_final, "environmental stress models", "environmental stress model")
  keywords$kw_final<-str_replace(keywords$kw_final, "extra pair fertilization", "extrapair fertilization")
  keywords$kw_final<-str_replace(keywords$kw_final, "extreme climate", "climate extreme")
  keywords$kw_final<-str_replace(keywords$kw_final, "functional responses", "functional response")
  keywords$kw_final<-str_replace(keywords$kw_final, "g x e", "gxe")
  keywords$kw_final<-str_replace(keywords$kw_final, "galápagos islands", "galapagos islands")
  keywords$kw_final<-str_replace(keywords$kw_final, "gene for gene", "gene-for-gene")
  keywords$kw_final<-str_replace(keywords$kw_final, "genotype phenotype map", "genotype-phenotype map")
  keywords$kw_final<-str_replace(keywords$kw_final, "gorilla gorilla gorilla", "gorilla")
  keywords$kw_final<-str_replace(keywords$kw_final, "growth defense trade-off", "growth-defense tradeoff")
  keywords$kw_final<-str_replace(keywords$kw_final, "heat wave", "heatwave")
  keywords$kw_final<-str_replace(keywords$kw_final, "host parasite system", "host-parasite system")
  keywords$kw_final<-str_replace(keywords$kw_final, "host-parasitoid interactions", "host-parasitoid interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "human-elephant conflict", "human-elephant conflict")
  keywords$kw_final<-str_replace(keywords$kw_final, "hydrography", "hydrograph")
  keywords$kw_final<-str_replace(keywords$kw_final, "induced responses", "induced response")
  keywords$kw_final<-str_replace(keywords$kw_final, "kernel density", "density kernel")
  keywords$kw_final<-str_replace(keywords$kw_final, "life history tradeoffs", "life-history tradeoffs")
  keywords$kw_final<-str_replace(keywords$kw_final, "lychnis flos cuculi", "lychnis floscuculi")
  keywords$kw_final<-str_replace(keywords$kw_final, "macro evolution", "macroevolution")
  keywords$kw_final<-str_replace(keywords$kw_final, "male-male competition", "male-male competition")
  keywords$kw_final<-str_replace(keywords$kw_final, "mark recapture models", "capture mark-recapture models")
  keywords$kw_final<-str_replace(keywords$kw_final, "mark resight", "mark-resight")
  keywords$kw_final<-str_replace(keywords$kw_final, "marmota marmota", "marmota")
  keywords$kw_final<-str_replace(keywords$kw_final, "mercenaria mercenaria", "mercenaria")
  keywords$kw_final<-str_replace(keywords$kw_final, "meta regression", "metaregression")
  keywords$kw_final<-str_replace(keywords$kw_final, "moist tropical forest", "tropical moist forest")
  keywords$kw_final<-str_replace(keywords$kw_final, "multi host pathogens", "multihost pathogens")
  keywords$kw_final<-str_replace(keywords$kw_final, "non consumptive effect", "nonconsumptive effect")
  keywords$kw_final<-str_replace(keywords$kw_final, "non trophic interactions", "nontrophic interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "paraná river", "parana river")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant water relations", "plant-water relations")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant-insect interaction", "plant-insect interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "pre adaptation", "preadaptation")
  keywords$kw_final<-str_replace(keywords$kw_final, "prey predator interactions", "predator-prey interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "projection matrix model", "matrix projection model")
  keywords$kw_final<-str_replace(keywords$kw_final, "r:fr", "red:far red ratio")
  keywords$kw_final<-str_replace(keywords$kw_final, "reaction diffusion", "reaction-diffusion")
  keywords$kw_final<-str_replace(keywords$kw_final, "reserves", "reserve")
  keywords$kw_final<-str_replace(keywords$kw_final, "root shoot ratio", "root:shoot ratio")
  keywords$kw_final<-str_replace(keywords$kw_final, "root to-shoot ratio", "root:shoot ratios")
  keywords$kw_final<-str_replace(keywords$kw_final, "selection  sexual", "sexual selection")
  keywords$kw_final<-str_replace(keywords$kw_final, "semi arid environment", "semiarid environment")
  keywords$kw_final<-str_replace(keywords$kw_final, "semi arid region", "semiarid region")
  keywords$kw_final<-str_replace(keywords$kw_final, "semi natural grassland", "seminatural grassland")
  keywords$kw_final<-str_replace(keywords$kw_final, "slash and-burn", "slash-and-burn")
  keywords$kw_final<-str_replace(keywords$kw_final, "slave making ants", "slavemaking ants")
  keywords$kw_final<-str_replace(keywords$kw_final, "species abundances", "species abundance")
  keywords$kw_final<-str_replace(keywords$kw_final, "species-area relationship", "species-area relationship")
  keywords$kw_final<-str_replace(keywords$kw_final, "structural equation modeling sem", "structural equation modeling")
  keywords$kw_final<-str_replace(keywords$kw_final, "taï national park", "tai national park")
  keywords$kw_final<-str_replace(keywords$kw_final, "top down forces", "top-down forces")
  keywords$kw_final<-str_replace(keywords$kw_final, "transposons", "transposon")
  keywords$kw_final<-str_replace(keywords$kw_final, "tree falls", "treefalls")
  keywords$kw_final<-str_replace(keywords$kw_final, "tree grass coexistence", "tree-grass coexistence")
  keywords$kw_final<-str_replace(keywords$kw_final, "tropical lowland forest", "lowland tropical forest")
  keywords$kw_final<-str_replace(keywords$kw_final, "túngara frogs", "tungara frogs")
  keywords$kw_final<-str_replace(keywords$kw_final, "abiotic and biotic factors", "biotic and abiotic factors")
  keywords$kw_final<-str_replace(keywords$kw_final, "abundance occupancy relationship", "occupancy-abundance relationship")
  keywords$kw_final<-str_replace(keywords$kw_final, "behavioral indirect effects", "indirect behavioral effects")
  keywords$kw_final<-str_replace(keywords$kw_final, "bia≈Çowie≈ºa forest", "bia≈Çowieza forest")
  keywords$kw_final<-str_replace(keywords$kw_final, "big leaf mahogany", "bigleaf mahogany")
  keywords$kw_final<-str_replace(keywords$kw_final, "bio indicators", "bioindicators")
  keywords$kw_final<-str_replace(keywords$kw_final, "biodiversity ecosystem functioning relationship", "biodiversity-ecosystem functioning relationship")
  keywords$kw_final<-str_replace(keywords$kw_final, "biodiversity-ecosystem function", "biodiversity-ecosystem function")
  keywords$kw_final<-str_replace(keywords$kw_final, "buteo buteo", "buteo")
  keywords$kw_final<-str_replace(keywords$kw_final, "c 13", "13 c")
  keywords$kw_final<-str_replace(keywords$kw_final, "c 3", "c3")
  keywords$kw_final<-str_replace(keywords$kw_final, "c 4", "c4")
  keywords$kw_final<-str_replace(keywords$kw_final, "check list", "checklist")
  keywords$kw_final<-str_replace(keywords$kw_final, "clarkia xantiana ssp xantiana", "clarkia xantiana")
  keywords$kw_final<-str_replace(keywords$kw_final, "clear cutting", "clearcutting")
  keywords$kw_final<-str_replace(keywords$kw_final, "co  2", "co2")
  keywords$kw_final<-str_replace(keywords$kw_final, "co flowering", "coflowering")
  keywords$kw_final<-str_replace(keywords$kw_final, "cost-benefit", "cost-benefit")
  keywords$kw_final<-str_replace(keywords$kw_final, "density mediated indirect effect", "density-mediated indirect effect")
  keywords$kw_final<-str_replace(keywords$kw_final, "density mediated indirect interaction", "density-mediated indirect interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "die back", "dieback")
  keywords$kw_final<-str_replace(keywords$kw_final, "differential selection", "selection differential")
  keywords$kw_final<-str_replace(keywords$kw_final, "dispersal barriers", "dispersal barrier")
  keywords$kw_final<-str_replace(keywords$kw_final, "distribution and abundance", "distribution-abundance")
  keywords$kw_final<-str_replace(keywords$kw_final, "diversity productivity", "productivity-diversity")
  keywords$kw_final<-str_replace(keywords$kw_final, "diversity productivity relationship", "diversity-productivity relationship")
  keywords$kw_final<-str_replace(keywords$kw_final, "dobzhansky muller incompatibility", "dobzhansky-muller incompatibility")
  keywords$kw_final<-str_replace(keywords$kw_final, "doñana national park", "donana")
  keywords$kw_final<-str_replace(keywords$kw_final, "earlywood", "early wood")
  keywords$kw_final<-str_replace(keywords$kw_final, "eco epidemiology", "ecoepidemiology")
  keywords$kw_final<-str_replace(keywords$kw_final, "ecoevolutionary dynamics", "eco-evolutionary dynamics")
  keywords$kw_final<-str_replace(keywords$kw_final, "f  q st st", "q-f st")
  keywords$kw_final<-str_replace(keywords$kw_final, "fitness landscapes", "fitness landscape")
  keywords$kw_final<-str_replace(keywords$kw_final, "foodweb", "food web")
  keywords$kw_final<-str_replace(keywords$kw_final, "foraging-predation risk trade off", "foraging predation risk trade-off")
  keywords$kw_final<-str_replace(keywords$kw_final, "forest edges", "forest edge")
  keywords$kw_final<-str_replace(keywords$kw_final, "forest trees", "forest tree")
  keywords$kw_final<-str_replace(keywords$kw_final, "free air co  enrichment face 2", "face")
  keywords$kw_final<-str_replace(keywords$kw_final, "fresh water", "freshwater")
  keywords$kw_final<-str_replace(keywords$kw_final, "freshwater macrophytes", "freshwater macrophyte")
  keywords$kw_final<-str_replace(keywords$kw_final, "genotype by environment interactions", "gxe")
  keywords$kw_final<-str_replace(keywords$kw_final, "ground layer", "groundlayer")
  keywords$kw_final<-str_replace(keywords$kw_final, "habitat transitions", "habitat transition")
  keywords$kw_final<-str_replace(keywords$kw_final, "haplo diploidy", "haplodiploidy")
  keywords$kw_final<-str_replace(keywords$kw_final, "herbivore plant interactions", "plant-herbivore interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "host-parasitoid", "host-parasitoid")
  keywords$kw_final<-str_replace(keywords$kw_final, "human-wildlife interactions", "human-wildlife interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "iguana iguana", "iguana")
  keywords$kw_final<-str_replace(keywords$kw_final, "individual species area relationship", "individual species-area relationship")
  keywords$kw_final<-str_replace(keywords$kw_final, "inflorescences", "inflorescence")
  keywords$kw_final<-str_replace(keywords$kw_final, "isolation by-resistance", "isolation by resistance")
  keywords$kw_final<-str_replace(keywords$kw_final, "janzen connell effect", "janzen-connell effect")
  keywords$kw_final<-str_replace(keywords$kw_final, "keystone resources", "keystone resource")
  keywords$kw_final<-str_replace(keywords$kw_final, "landuse", "land-use")
  keywords$kw_final<-str_replace(keywords$kw_final, "large scale disturbance", "large-scale disturbance")
  keywords$kw_final<-str_replace(keywords$kw_final, "leaf cutter ants", "leafcutter ants")
  keywords$kw_final<-str_replace(keywords$kw_final, "live bearing", "livebearing")
  keywords$kw_final<-str_replace(keywords$kw_final, "lock and-key", "lock-and-key")
  keywords$kw_final<-str_replace(keywords$kw_final, "log normal", "lognormal")
  keywords$kw_final<-str_replace(keywords$kw_final, "luquillo experimental-forest", "luquillo experimental forest")
  keywords$kw_final<-str_replace(keywords$kw_final, "lynx lynx", "lynx")
  keywords$kw_final<-str_replace(keywords$kw_final, "maracá", "maraca")
  keywords$kw_final<-str_replace(keywords$kw_final, "match mismatch", "match-mismatch")
  keywords$kw_final<-str_replace(keywords$kw_final, "match mismatch hypothesis", "match-mismatch hypothesis")
  keywords$kw_final<-str_replace(keywords$kw_final, "maternal environmental effects", "environmental maternal effects")
  keywords$kw_final<-str_replace(keywords$kw_final, "mean d 2", "mean d2")
  keywords$kw_final<-str_replace(keywords$kw_final, "micro evolution", "microevolution")
  keywords$kw_final<-str_replace(keywords$kw_final, "montane tropical rainforest", "tropical montane rainforest")
  keywords$kw_final<-str_replace(keywords$kw_final, "monte carlo markov chain", "markov chain monte carlo")
  keywords$kw_final<-str_replace(keywords$kw_final, "multi level selection", "multilevel selection")
  keywords$kw_final<-str_replace(keywords$kw_final, "multimodal signals", "multimodal signal")
  keywords$kw_final<-str_replace(keywords$kw_final, "multiple spatial scales", "multiple spatial scale")
  keywords$kw_final<-str_replace(keywords$kw_final, "multivariate regression trees", "multivariate regression tree")
  keywords$kw_final<-str_replace(keywords$kw_final, "n  fixation 2", "nitrogen fixation")
  keywords$kw_final<-str_replace(keywords$kw_final, "non genetic inheritance", "nongenetic inheritance")
  keywords$kw_final<-str_replace(keywords$kw_final, "non indigenous species", "nonindigenous species")
  keywords$kw_final<-str_replace(keywords$kw_final, "non lethal predator effects", "nonlethal predator effects")
  keywords$kw_final<-str_replace(keywords$kw_final, "non metric multidimensional scaling", "nmds")
  keywords$kw_final<-str_replace(keywords$kw_final, "non random species loss", "nonrandom species loss")
  keywords$kw_final<-str_replace(keywords$kw_final, "over exploitation", "overexploitation")
  keywords$kw_final<-str_replace(keywords$kw_final, "p 2", "p2")
  keywords$kw_final<-str_replace(keywords$kw_final, "palm oil", "oil palm")
  keywords$kw_final<-str_replace(keywords$kw_final, "pan troglodytes troglodytes", "pan troglodytes")
  keywords$kw_final<-str_replace(keywords$kw_final, "parasite host dynamics", "host-parasite dynamics")
  keywords$kw_final<-str_replace(keywords$kw_final, "pea aphid acyrthosiphon pisum", "acyrthosiphon pisum, pea aphid")
  keywords$kw_final<-str_replace(keywords$kw_final, "performance trade off", "performance tradeoff")
  keywords$kw_final<-str_replace(keywords$kw_final, "phylogenetic comparative analyses", "comparative phylogenetic analyses")
  keywords$kw_final<-str_replace(keywords$kw_final, "pine oak forest", "oak pine forest")
  keywords$kw_final<-str_replace(keywords$kw_final, "pitvipers", "pit vipers")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant animal mutualism", "plant-animal mutualism")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant frugivore networks", "plant-frugivore networks")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant fungal interactions", "plant-fungal interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant life span", "plant lifespan")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant pollinator networks", "plant-pollinator networks")
  keywords$kw_final<-str_replace(keywords$kw_final, "postmating", "post-mating")
  keywords$kw_final<-str_replace(keywords$kw_final, "pre dispersal seed predator", "predispersal seed predator")
  keywords$kw_final<-str_replace(keywords$kw_final, "predator prey ratios", "predator:prey ratios")
  keywords$kw_final<-str_replace(keywords$kw_final, "productivity diversity relationship", "diversity-productivity relationship")
  keywords$kw_final<-str_replace(keywords$kw_final, "q  st", "qst")
  keywords$kw_final<-str_replace(keywords$kw_final, "r 0", "r0")
  keywords$kw_final<-str_replace(keywords$kw_final, "random amplified polymorphic dna", "rapd")
  keywords$kw_final<-str_replace(keywords$kw_final, "range extensions", "range extension")
  keywords$kw_final<-str_replace(keywords$kw_final, "rangifer tarandus caribou", "caribou rangifer tarandus")
  keywords$kw_final<-str_replace(keywords$kw_final, "red-far red", "red:far red")
  keywords$kw_final<-str_replace(keywords$kw_final, "respiration rates", "respiration rate")
  keywords$kw_final<-str_replace(keywords$kw_final, "rnaseq", "rna seq")
  keywords$kw_final<-str_replace(keywords$kw_final, "rock paper-scissors", "rock-paper-scissors")
  keywords$kw_final<-str_replace(keywords$kw_final, "root-shoot ratio", "root:shoot ratio")
  keywords$kw_final<-str_replace(keywords$kw_final, "sea grass", "seagrass")
  keywords$kw_final<-str_replace(keywords$kw_final, "secondary tropical forest", "tropical secondary forest")
  keywords$kw_final<-str_replace(keywords$kw_final, "seed bank persistence", "seedbank persistence")
  keywords$kw_final<-str_replace(keywords$kw_final, "selection artificial", "artificial selection")
  keywords$kw_final<-str_replace(keywords$kw_final, "selection—sexual", "sexual selection")
  keywords$kw_final<-str_replace(keywords$kw_final, "selective pressures", "selective pressure")
  keywords$kw_final<-str_replace(keywords$kw_final, "semi arid steppe", "semiarid steppe")
  keywords$kw_final<-str_replace(keywords$kw_final, "shrubsteppe", "shrub-steppe")
  keywords$kw_final<-str_replace(keywords$kw_final, "speciation with-gene-flow", "speciation with gene flow")
  keywords$kw_final<-str_replace(keywords$kw_final, "species accumulation curves", "species accumulation curve")
  keywords$kw_final<-str_replace(keywords$kw_final, "species co existence", "species coexistence")
  keywords$kw_final<-str_replace(keywords$kw_final, "species roles", "species role")
  keywords$kw_final<-str_replace(keywords$kw_final, "sub alpine forest", "subalpine forest")
  keywords$kw_final<-str_replace(keywords$kw_final, "sub saharan africa", "sub-saharan africa")
  keywords$kw_final<-str_replace(keywords$kw_final, "terrestrial aquatic linkages", "terrestrial-aquatic linkages")
  keywords$kw_final<-str_replace(keywords$kw_final, "tide pool", "tidepool")
  keywords$kw_final<-str_replace(keywords$kw_final, "tide pools", "tidepools")
  keywords$kw_final<-str_replace(keywords$kw_final, "timescales", "timescale")
  keywords$kw_final<-str_replace(keywords$kw_final, "tree frogs", "treefrogs")
  keywords$kw_final<-str_replace(keywords$kw_final, "tree hole", "treehole")
  keywords$kw_final<-str_replace(keywords$kw_final, "tropical humid forest", "humid tropical forest")
  keywords$kw_final<-str_replace(keywords$kw_final, "tropical vs temperate", "temperate vs tropical")
  keywords$kw_final<-str_replace(keywords$kw_final, "virulence transmission trade-off", "transmission virulence trade-off")
  keywords$kw_final<-str_replace(keywords$kw_final, "yucatán peninsula", "yucatan peninsula")
  keywords$kw_final<-str_replace(keywords$kw_final, "above ground biomass agb", "aboveground biomass agb")
  keywords$kw_final<-str_replace(keywords$kw_final, "aboveground net primary productivity anpp", "anpp")
  keywords$kw_final<-str_replace(keywords$kw_final, "age stage structure", "age-stage structure")
  keywords$kw_final<-str_replace(keywords$kw_final, "agro ecology", "agroecology")
  keywords$kw_final<-str_replace(keywords$kw_final, "animal signals", "animal signal")
  keywords$kw_final<-str_replace(keywords$kw_final, "anser caerulescens-caerulescens", "anser caerulescens")
  keywords$kw_final<-str_replace(keywords$kw_final, "anti herbivore defenses", "antiherbivore defenses")
  keywords$kw_final<-str_replace(keywords$kw_final, "anti oxidants", "antioxidants")
  keywords$kw_final<-str_replace(keywords$kw_final, "anti predator strategy", "antipredator strategy")
  keywords$kw_final<-str_replace(keywords$kw_final, "anti predatory behavior", "antipredatory behavior")
  keywords$kw_final<-str_replace(keywords$kw_final, "antlion", "ant lion")
  keywords$kw_final<-str_replace(keywords$kw_final, "antlions", "ant lions")
  keywords$kw_final<-str_replace(keywords$kw_final, "aphid ant mutualism", "ant-aphid mutualism")
  keywords$kw_final<-str_replace(keywords$kw_final, "aquatic-terrestrial linkages", "aquatic-terrestrial linkage")
  keywords$kw_final<-str_replace(keywords$kw_final, "atmospheric co 2", "atmospheric co2")
  keywords$kw_final<-str_replace(keywords$kw_final, "automated radio telemetry", "automated radiotelemetry")
  keywords$kw_final<-str_replace(keywords$kw_final, "baja california-sur", "baja california sur")
  keywords$kw_final<-str_replace(keywords$kw_final, "bat flies", "batflies")
  keywords$kw_final<-str_replace(keywords$kw_final, "bayesian hierarchical modeling", "hierarchical bayesian modeling")
  keywords$kw_final<-str_replace(keywords$kw_final, "benefits and costs", "costs and benefits")
  keywords$kw_final<-str_replace(keywords$kw_final, "bi parental care", "biparental care")
  keywords$kw_final<-str_replace(keywords$kw_final, "bi stability", "bistability")
  keywords$kw_final<-str_replace(keywords$kw_final, "biodiversity data set", "biodiversity dataset")
  keywords$kw_final<-str_replace(keywords$kw_final, "blackwater", "black water")
  keywords$kw_final<-str_replace(keywords$kw_final, "bud burst", "budburst")
  keywords$kw_final<-str_replace(keywords$kw_final, "bufo bufo", "bufo")
  keywords$kw_final<-str_replace(keywords$kw_final, "capture mark-recapture analysis", "capture-mark-recapture analysis")
  keywords$kw_final<-str_replace(keywords$kw_final, "carbon-nutrient balance", "c:n balance")
  keywords$kw_final<-str_replace(keywords$kw_final, "cestodes", "cestode")
  keywords$kw_final<-str_replace(keywords$kw_final, "ch 4", "ch4")
  keywords$kw_final<-str_replace(keywords$kw_final, "chaetognatha", "chaetognath")
  keywords$kw_final<-str_replace(keywords$kw_final, "chemical signals", "chemical signal")
  keywords$kw_final<-str_replace(keywords$kw_final, "chen caerulescens caerulescens", "chen caerulescens")
  keywords$kw_final<-str_replace(keywords$kw_final, "climate growth relationship", "climate-growth relationship")
  keywords$kw_final<-str_replace(keywords$kw_final, "co gradient variation", "cogradient variation")
  keywords$kw_final<-str_replace(keywords$kw_final, "co operation", "cooperation")
  keywords$kw_final<-str_replace(keywords$kw_final, "co operative breeding", "cooperative breeding")
  keywords$kw_final<-str_replace(keywords$kw_final, "co speciation", "cospeciation")
  keywords$kw_final<-str_replace(keywords$kw_final, "coefficient of additive genetic variation", "additive genetic coefficient of variation")
  keywords$kw_final<-str_replace(keywords$kw_final, "colonization competition", "competition-colonization")
  keywords$kw_final<-str_replace(keywords$kw_final, "colonization competition trade-off", "competition-colonization trade-off")
  keywords$kw_final<-str_replace(keywords$kw_final, "colonization extinction dynamics", "colonization-extinction dynamics")
  keywords$kw_final<-str_replace(keywords$kw_final, "color ornament", "ornament color")
  keywords$kw_final<-str_replace(keywords$kw_final, "community forest", "forest community")
  keywords$kw_final<-str_replace(keywords$kw_final, "compensatory responses", "compensatory response")
  keywords$kw_final<-str_replace(keywords$kw_final, "competitive responses", "competitive response")
  keywords$kw_final<-str_replace(keywords$kw_final, "consumer resource cycles", "consumer resource cycle")
  keywords$kw_final<-str_replace(keywords$kw_final, "consumer-resource interaction", "consumer-resource interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "crown of-thorns starfish", "crown-of-thorns starfish")
  keywords$kw_final<-str_replace(keywords$kw_final, "ctmin", "ct min")
  keywords$kw_final<-str_replace(keywords$kw_final, "data bases", "databases")
  keywords$kw_final<-str_replace(keywords$kw_final, "deep water", "deepwater")
  keywords$kw_final<-str_replace(keywords$kw_final, "deforestation rates", "deforestation rate")
  keywords$kw_final<-str_replace(keywords$kw_final, "dendro ecology", "dendroecology")
  keywords$kw_final<-str_replace(keywords$kw_final, "diet expansions", "diet expansion")
  keywords$kw_final<-str_replace(keywords$kw_final, "diet shifts", "diet shift")
  keywords$kw_final<-str_replace(keywords$kw_final, "dispersers", "disperser")
  keywords$kw_final<-str_replace(keywords$kw_final, "distribution abundance relationship", "distribution-abundance relationship")
  keywords$kw_final<-str_replace(keywords$kw_final, "diversity stability relationship", "diversity-stability relationship")
  keywords$kw_final<-str_replace(keywords$kw_final, "dose response curve", "dose-response curve")
  keywords$kw_final<-str_replace(keywords$kw_final, "doñana", "donana")
  keywords$kw_final<-str_replace(keywords$kw_final, "eco metabolomics", "ecometabolomics")
  keywords$kw_final<-str_replace(keywords$kw_final, "eco physiology", "ecophysiology")
  keywords$kw_final<-str_replace(keywords$kw_final, "ecoevolutionary feedbacks", "eco-evolutionary feedbacks")
  keywords$kw_final<-str_replace(keywords$kw_final, "elaphe obsoleta obsoleta", "elaphe obsoleta")
  keywords$kw_final<-str_replace(keywords$kw_final, "elevated co  2", "elevated co2")
  keywords$kw_final<-str_replace(keywords$kw_final, "evodevo", "evo-devo")
  keywords$kw_final<-str_replace(keywords$kw_final, "extinction colonization", "extinction-colonization")
  keywords$kw_final<-str_replace(keywords$kw_final, "extra floral nectaries", "extrafloral nectaries")
  keywords$kw_final<-str_replace(keywords$kw_final, "extra pair copulation", "extrapair copulation")
  keywords$kw_final<-str_replace(keywords$kw_final, "extra pair mating", "extra-pair mating")
  keywords$kw_final<-str_replace(keywords$kw_final, "extrapair mating", "extra-pair mating")
  keywords$kw_final<-str_replace(keywords$kw_final, "fast-slow continuum", "fast-slow continuum")
  keywords$kw_final<-str_replace(keywords$kw_final, "field work", "fieldwork")
  keywords$kw_final<-str_replace(keywords$kw_final, "fishfauna", "fish fauna")
  keywords$kw_final<-str_replace(keywords$kw_final, "fission fusion", "fission-fusion")
  keywords$kw_final<-str_replace(keywords$kw_final, "fitness difference", "fitness differences")
  keywords$kw_final<-str_replace(keywords$kw_final, "fitness surfaces", "fitness surface")
  keywords$kw_final<-str_replace(keywords$kw_final, "floral resource", "floral resources")
  keywords$kw_final<-str_replace(keywords$kw_final, "fluctuating resources", "fluctuating resource")
  keywords$kw_final<-str_replace(keywords$kw_final, "fluorescent dyes", "fluorescent dye")
  keywords$kw_final<-str_replace(keywords$kw_final, "forest reserves", "forest reserve")
  keywords$kw_final<-str_replace(keywords$kw_final, "frequencydependent selection", "frequency-dependent selection")
  keywords$kw_final<-str_replace(keywords$kw_final, "fresh water snail", "freshwater snail")
  keywords$kw_final<-str_replace(keywords$kw_final, "fresh water snails", "freshwater snails")
  keywords$kw_final<-str_replace(keywords$kw_final, "freshwater tropical fish", "tropical freshwater fish")
  keywords$kw_final<-str_replace(keywords$kw_final, "freshwater turtles", "freshwater turtle")
  keywords$kw_final<-str_replace(keywords$kw_final, "fuel wood", "fuelwood")
  keywords$kw_final<-str_replace(keywords$kw_final, "functional trade offs", "functional tradeoffs")
  keywords$kw_final<-str_replace(keywords$kw_final, "g st", "gst")
  keywords$kw_final<-str_replace(keywords$kw_final, "gallus gallus", "gallus")
  keywords$kw_final<-str_replace(keywords$kw_final, "gene by environment interaction", "gxe")
  keywords$kw_final<-str_replace(keywords$kw_final, "genetic resource", "genetic resources")
  keywords$kw_final<-str_replace(keywords$kw_final, "genotype x environment interaction", "gxe")
  keywords$kw_final<-str_replace(keywords$kw_final, "germline", "germ line")
  keywords$kw_final<-str_replace(keywords$kw_final, "gorilla beringei beringei", "gorilla beringei")
  keywords$kw_final<-str_replace(keywords$kw_final, "gorilla gorilla", "gorilla")
  keywords$kw_final<-str_replace(keywords$kw_final, "grass land", "grassland")
  keywords$kw_final<-str_replace(keywords$kw_final, "grass shrub competition", "grass-shrub competition")
  keywords$kw_final<-str_replace(keywords$kw_final, "green space", "greenspace")
  keywords$kw_final<-str_replace(keywords$kw_final, "grime tilman debate", "grime-tilman debate")
  keywords$kw_final<-str_replace(keywords$kw_final, "gulf of-mexico", "gulf of mexico")
  keywords$kw_final<-str_replace(keywords$kw_final, "guánica forest", "guanica forest")
  keywords$kw_final<-str_replace(keywords$kw_final, "heat waves", "heatwaves")
  keywords$kw_final<-str_replace(keywords$kw_final, "hemi epiphytes", "hemiepiphytes")
  keywords$kw_final<-str_replace(keywords$kw_final, "hitch hiking", "hitchhiking")
  keywords$kw_final<-str_replace(keywords$kw_final, "homegarden", "home garden")
  keywords$kw_final<-str_replace(keywords$kw_final, "host ", "host")
  keywords$kw_final<-str_replace(keywords$kw_final, "host microbe interactions", "host-microbe interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "host parasite co-evolution", "host-parasite coevolution")
  keywords$kw_final<-str_replace(keywords$kw_final, "host parasite ecology", "host-parasite ecology")
  keywords$kw_final<-str_replace(keywords$kw_final, "host parasitoid interaction", "host-parasitoid interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "host-parasite interactions", "host-parasite interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "hostparasite interaction", "host-parasite interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "host-pathogen dynamics", "host-pathogen dynamics")
  keywords$kw_final<-str_replace(keywords$kw_final, "human elephant conflict", "human-elephant conflict")
  keywords$kw_final<-str_replace(keywords$kw_final, "identity by-descent", "identity by descent")
  keywords$kw_final<-str_replace(keywords$kw_final, "iguazú national park", "iguazu national park")
  keywords$kw_final<-str_replace(keywords$kw_final, "induced plant defenses", "induced plant defense")
  keywords$kw_final<-str_replace(keywords$kw_final, "inter annual variation", "interannual variation")
  keywords$kw_final<-str_replace(keywords$kw_final, "inter patch movement", "interpatch movement")
  keywords$kw_final<-str_replace(keywords$kw_final, "intra guild competition", "intraguild competition")
  keywords$kw_final<-str_replace(keywords$kw_final, "intra guild predation", "intraguild predation")
  keywords$kw_final<-str_replace(keywords$kw_final, "intraspecific and interspecific competition", "interspecific and intraspecific competition")
  keywords$kw_final<-str_replace(keywords$kw_final, "isolation with-migration", "isolation with migration")
  keywords$kw_final<-str_replace(keywords$kw_final, "keystone plant resources", "keystone plant resource")
  keywords$kw_final<-str_replace(keywords$kw_final, "la selva-biological-station", "la selva")
  keywords$kw_final<-str_replace(keywords$kw_final, "lagopus lagopus", "lagopus")
  keywords$kw_final<-str_replace(keywords$kw_final, "land birds", "landbirds")
  keywords$kw_final<-str_replace(keywords$kw_final, "landsnails", "land snails")
  keywords$kw_final<-str_replace(keywords$kw_final, "late successional trees", "late successional tree")
  keywords$kw_final<-str_replace(keywords$kw_final, "leaffall", "leaf-fall")
  keywords$kw_final<-str_replace(keywords$kw_final, "levels of-selection", "levels of selection")
  keywords$kw_final<-str_replace(keywords$kw_final, "life history covariation", "life-history covariation")
  keywords$kw_final<-str_replace(keywords$kw_final, "life history tradeoff", "life-history trade-off")
  keywords$kw_final<-str_replace(keywords$kw_final, "life history transition", "life-history transition")
  keywords$kw_final<-str_replace(keywords$kw_final, "life history transitions", "life-history transition")
  keywords$kw_final<-str_replace(keywords$kw_final, "lifehistory evolution", "life-history evolution")
  keywords$kw_final<-str_replace(keywords$kw_final, "litter bags", "litterbags")
  keywords$kw_final<-str_replace(keywords$kw_final, "log normal distribution", "lognormal distribution")
  keywords$kw_final<-str_replace(keywords$kw_final, "longdistance dispersal", "long-distance dispersal")
  keywords$kw_final<-str_replace(keywords$kw_final, "lychnis flos-cuculi", "lychnis floscuculi")
  keywords$kw_final<-str_replace(keywords$kw_final, "mainland island", "island mainland")
  keywords$kw_final<-str_replace(keywords$kw_final, "marine sponges", "marine sponge")
  keywords$kw_final<-str_replace(keywords$kw_final, "mark release recapture", "mark-release-recapture")
  keywords$kw_final<-str_replace(keywords$kw_final, "martes martes", "martes")
  keywords$kw_final<-str_replace(keywords$kw_final, "maternal genetic effects", "genetic maternal effects")
  keywords$kw_final<-str_replace(keywords$kw_final, "mesocarnivores", "mesocarnivore")
  keywords$kw_final<-str_replace(keywords$kw_final, "mesoherbivores", "mesoherbivore")
  keywords$kw_final<-str_replace(keywords$kw_final, "meta communities", "metacommunities")
  keywords$kw_final<-str_replace(keywords$kw_final, "meta population", "metapopulation")
  keywords$kw_final<-str_replace(keywords$kw_final, "meta populations", "metapopulations")
  keywords$kw_final<-str_replace(keywords$kw_final, "mexican central pacific", "central mexican pacific")
  keywords$kw_final<-str_replace(keywords$kw_final, "micro arthropods", "microarthropods")
  keywords$kw_final<-str_replace(keywords$kw_final, "microct", "micro ct")
  keywords$kw_final<-str_replace(keywords$kw_final, "mito nuclear", "mitonuclear")
  keywords$kw_final<-str_replace(keywords$kw_final, "multi event model", "multievent model")
  keywords$kw_final<-str_replace(keywords$kw_final, "multi event models", "multievent models")
  keywords$kw_final<-str_replace(keywords$kw_final, "multi model inference", "multimodel inference")
  keywords$kw_final<-str_replace(keywords$kw_final, "multi scale ordination", "multiscale ordination")
  keywords$kw_final<-str_replace(keywords$kw_final, "multi species", "multispecies")
  keywords$kw_final<-str_replace(keywords$kw_final, "multi state models", "multistate models")
  keywords$kw_final<-str_replace(keywords$kw_final, "multi trophic", "multitrophic")
  keywords$kw_final<-str_replace(keywords$kw_final, "multievent capture recapture models", "multi event capture-recapture models")
  keywords$kw_final<-str_replace(keywords$kw_final, "multiple scales", "multiple scale")
  keywords$kw_final<-str_replace(keywords$kw_final, "myco heterotrophy", "mycoheterotrophy")
  keywords$kw_final<-str_replace(keywords$kw_final, "n 2 fixation", "nitrogen fixation")
  keywords$kw_final<-str_replace(keywords$kw_final, "n o 2", "no2")
  keywords$kw_final<-str_replace(keywords$kw_final, "n p ratio", "n:p ratio")
  keywords$kw_final<-str_replace(keywords$kw_final, "n-p ratio", "n:p ratio")
  keywords$kw_final<-str_replace(keywords$kw_final, "nestling growth rates", "nestling growth rate")
  keywords$kw_final<-str_replace(keywords$kw_final, "niche dimensions", "niche dimension")
  keywords$kw_final<-str_replace(keywords$kw_final, "non consumptive predator effects", "predator nonconsumptive effects")
  keywords$kw_final<-str_replace(keywords$kw_final, "non stationarity", "nonstationarity")
  keywords$kw_final<-str_replace(keywords$kw_final, "non stationary", "nonstationary")
  keywords$kw_final<-str_replace(keywords$kw_final, "non target effects", "nontarget effects")
  keywords$kw_final<-str_replace(keywords$kw_final, "non trophic interaction", "nontrophic interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "nonconsumptive predator effects", "predator nonconsumptive effects")
  keywords$kw_final<-str_replace(keywords$kw_final, "nucleotide sequences", "nucleotide sequence")
  keywords$kw_final<-str_replace(keywords$kw_final, "nutrient co limitation", "nutrient colimitation")
  keywords$kw_final<-str_replace(keywords$kw_final, "ornstein uhlenbeck models", "ornstein-uhlenbeck models")
  keywords$kw_final<-str_replace(keywords$kw_final, "over compensation", "overcompensation")
  keywords$kw_final<-str_replace(keywords$kw_final, "over dispersion", "overdispersion")
  keywords$kw_final<-str_replace(keywords$kw_final, "over yielding", "overyielding")
  keywords$kw_final<-str_replace(keywords$kw_final, "paloverde", "palo verde")
  keywords$kw_final<-str_replace(keywords$kw_final, "parasite host", "host-parasite")
  keywords$kw_final<-str_replace(keywords$kw_final, "parasitoid host", "host-parasitoid")
  keywords$kw_final<-str_replace(keywords$kw_final, "parasitoid host interactions", "host-parasitoid interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "partitioning diversity", "diversity partitioning")
  keywords$kw_final<-str_replace(keywords$kw_final, "pattern diversity", "diversity pattern")
  keywords$kw_final<-str_replace(keywords$kw_final, "pelagic benthic coupling", "benthic-pelagic coupling")
  keywords$kw_final<-str_replace(keywords$kw_final, "pennsylvanian", "pennsylvania")
  keywords$kw_final<-str_replace(keywords$kw_final, "phenol oxidase", "phenoloxidase")
  keywords$kw_final<-str_replace(keywords$kw_final, "phenotypic responses", "phenotypic response")
  keywords$kw_final<-str_replace(keywords$kw_final, "photoidentification", "photo identification")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant ", "plant")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant  animal interaction", "plant-animal interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant ant interactions", "ant-plant interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant insect relationships", "insect-plant relationships")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant interaction", "plant-plant interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant microbe interaction", "plant-microbe interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant microbial interactions", "plant-microbial interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant plant communication", "plant communication")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant responses", "plant response")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant-plant interaction", "plant-plant interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "pollen ovule ratio", "pollen:ovule ratio")
  keywords$kw_final<-str_replace(keywords$kw_final, "poró", "poro")
  keywords$kw_final<-str_replace(keywords$kw_final, "post copulatory female choice", "postcopulatory female choice")
  keywords$kw_final<-str_replace(keywords$kw_final, "post mating isolation", "postmating isolation")
  keywords$kw_final<-str_replace(keywords$kw_final, "post mating sexual selection", "postmating sexual selection")
  keywords$kw_final<-str_replace(keywords$kw_final, "post settlement processes", "postsettlement processes")
  keywords$kw_final<-str_replace(keywords$kw_final, "post zygotic", "postzygotic")
  keywords$kw_final<-str_replace(keywords$kw_final, "post zygotic reproductive isolation", "postzygotic reproductive isolation")
  keywords$kw_final<-str_replace(keywords$kw_final, "pre mating isolation", "premating isolation")
  keywords$kw_final<-str_replace(keywords$kw_final, "pre zygotic isolation", "prezygotic isolation")
  keywords$kw_final<-str_replace(keywords$kw_final, "predatorprey interaction", "predator-prey interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "predatorprey interactions", "predator-prey interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "presence-absence", "presence-absence")
  keywords$kw_final<-str_replace(keywords$kw_final, "presence-absence data", "presence-absence data")
  keywords$kw_final<-str_replace(keywords$kw_final, "presence-absence", "presence-absence")
  keywords$kw_final<-str_replace(keywords$kw_final, "prey predator", "predator-prey")
  keywords$kw_final<-str_replace(keywords$kw_final, "producer scrounger", "producer-scrounger")
  keywords$kw_final<-str_replace(keywords$kw_final, "projection matrix models", "matrix projection models")
  keywords$kw_final<-str_replace(keywords$kw_final, "prédation", "predation")
  keywords$kw_final<-str_replace(keywords$kw_final, "pseudoarrhenotoky", "pseudo arrhenotoky")
  keywords$kw_final<-str_replace(keywords$kw_final, "q  f st st", "q-f st")
  keywords$kw_final<-str_replace(keywords$kw_final, "qst f-st comparison", "qst fst comparison")
  keywords$kw_final<-str_replace(keywords$kw_final, "québec", "quebec")
  keywords$kw_final<-str_replace(keywords$kw_final, "rain ", "rain")
  keywords$kw_final<-str_replace(keywords$kw_final, "rare and common species", "common and rare species")
  keywords$kw_final<-str_replace(keywords$kw_final, "recessive deleterious mutations", "deleterious recessive mutations")
  keywords$kw_final<-str_replace(keywords$kw_final, "regional vs local", "local vs regional")
  keywords$kw_final<-str_replace(keywords$kw_final, "reserve forest", "forest reserve")
  keywords$kw_final<-str_replace(keywords$kw_final, "residence times", "residence time")
  keywords$kw_final<-str_replace(keywords$kw_final, "resistance genes", "resistance gene")
  keywords$kw_final<-str_replace(keywords$kw_final, "resource consumer interactions", "consumer-resource interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "response curves", "response curve")
  keywords$kw_final<-str_replace(keywords$kw_final, "ricefields", "rice fields")
  keywords$kw_final<-str_replace(keywords$kw_final, "river ganga", "ganga river")
  keywords$kw_final<-str_replace(keywords$kw_final, "roadkills", "roadkill")
  keywords$kw_final<-str_replace(keywords$kw_final, "robertsonian translocations", "robertsonian translocation")
  keywords$kw_final<-str_replace(keywords$kw_final, "root : shoot ratio", "root:shoot ratio")
  keywords$kw_final<-str_replace(keywords$kw_final, "root: shoot ratio", "root:shoot ratio")
  keywords$kw_final<-str_replace(keywords$kw_final, "run off", "runoff")
  keywords$kw_final<-str_replace(keywords$kw_final, "s pecies richness", "species richness")
  keywords$kw_final<-str_replace(keywords$kw_final, "savanna forest boundary", "savanna-forest boundary")
  keywords$kw_final<-str_replace(keywords$kw_final, "sea grasses", "seagrasses")
  keywords$kw_final<-str_replace(keywords$kw_final, "seed reserves", "seed reserve")
  keywords$kw_final<-str_replace(keywords$kw_final, "semiarid shrublands", "semiarid shrubland")
  keywords$kw_final<-str_replace(keywords$kw_final, "semideciduous seasonal forest", "seasonal semideciduous forest")
  keywords$kw_final<-str_replace(keywords$kw_final, "sex allocation ratio", "sex ratio allocation")
  keywords$kw_final<-str_replace(keywords$kw_final, "shoot root ratio", "root:shoot ratio")
  keywords$kw_final<-str_replace(keywords$kw_final, "shrub steppe", "shrub-steppe")
  keywords$kw_final<-str_replace(keywords$kw_final, "size effect", "effect size")
  keywords$kw_final<-str_replace(keywords$kw_final, "sizestructured populations", "size-structured populations")
  keywords$kw_final<-str_replace(keywords$kw_final, "skipping reproduction", "reproduction skipping")
  keywords$kw_final<-str_replace(keywords$kw_final, "slash and burn", "slash-and-burn")
  keywords$kw_final<-str_replace(keywords$kw_final, "soft sediments", "soft sediment")
  keywords$kw_final<-str_replace(keywords$kw_final, "soil micro organisms", "soil microorganisms")
  keywords$kw_final<-str_replace(keywords$kw_final, "source-sink", "source-sink")
  keywords$kw_final<-str_replace(keywords$kw_final, "south eastern brazil", "southeastern brazil")
  keywords$kw_final<-str_replace(keywords$kw_final, "spatially explicit capture-recapture", "spatially explicit capture-recapture")
  keywords$kw_final<-str_replace(keywords$kw_final, "spatio temporal", "spatiotemporal")
  keywords$kw_final<-str_replace(keywords$kw_final, "spatio temporal dynamics", "spatiotemporal dynamics")
  keywords$kw_final<-str_replace(keywords$kw_final, "spatio temporal models", "spatiotemporal models")
  keywords$kw_final<-str_replace(keywords$kw_final, "spatio temporal pattern", "spatiotemporal pattern")
  keywords$kw_final<-str_replace(keywords$kw_final, "spatio temporal scales", "spatiotemporal scales")
  keywords$kw_final<-str_replace(keywords$kw_final, "species area relationship sar", "species-area relationships")
  keywords$kw_final<-str_replace(keywords$kw_final, "species energy relationship", "species-energy relationship")
  keywords$kw_final<-str_replace(keywords$kw_final, "spider web", "web spider")
  keywords$kw_final<-str_replace(keywords$kw_final, "ssurdna", "ssu rdna")
  keywords$kw_final<-str_replace(keywords$kw_final, "stable nitrogen isotope", "nitrogen stable isotope")
  keywords$kw_final<-str_replace(keywords$kw_final, "stable states", "stable state")
  keywords$kw_final<-str_replace(keywords$kw_final, "statespace model", "state-space model")
  keywords$kw_final<-str_replace(keywords$kw_final, "sub alpine", "subalpine")
  keywords$kw_final<-str_replace(keywords$kw_final, "sub tropical", "subtropical")
  keywords$kw_final<-str_replace(keywords$kw_final, "sub tropical forest", "subtropical forest")
  keywords$kw_final<-str_replace(keywords$kw_final, "sugar cane", "sugarcane")
  keywords$kw_final<-str_replace(keywords$kw_final, "super hosts", "superhosts")
  keywords$kw_final<-str_replace(keywords$kw_final, "survival and growth", "growth and survival")
  keywords$kw_final<-str_replace(keywords$kw_final, "sus scrofa scrofa", "sus scrofa")
  keywords$kw_final<-str_replace(keywords$kw_final, "t cell mediated immune response", "t cell-mediated immune response")
  keywords$kw_final<-str_replace(keywords$kw_final, "terrestrial aquatic linkage", "aquatic-terrestrial linkage")
  keywords$kw_final<-str_replace(keywords$kw_final, "tiger salamanders", "tiger salamander")
  keywords$kw_final<-str_replace(keywords$kw_final, "tit for-tat", "tit for tat")
  keywords$kw_final<-str_replace(keywords$kw_final, "top down factors", "top-down factors")
  keywords$kw_final<-str_replace(keywords$kw_final, "top down limitation", "top-down limitation")
  keywords$kw_final<-str_replace(keywords$kw_final, "trait environment relationships", "trait-environment relationships")
  keywords$kw_final<-str_replace(keywords$kw_final, "traitmediated interaction", "trait-mediated interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "trans generational", "transgenerational")
  keywords$kw_final<-str_replace(keywords$kw_final, "transitions", "transition")
  keywords$kw_final<-str_replace(keywords$kw_final, "tree fall", "treefall")
  keywords$kw_final<-str_replace(keywords$kw_final, "tree fall gap", "treefall gap")
  keywords$kw_final<-str_replace(keywords$kw_final, "tree frog", "treefrog")
  keywords$kw_final<-str_replace(keywords$kw_final, "tree line ecotone", "treeline ecotone")
  keywords$kw_final<-str_replace(keywords$kw_final, "tree-grass coexistence", "tree-grass coexistence")
  keywords$kw_final<-str_replace(keywords$kw_final, "tropical temperate comparison", "temperate vs tropical")
  keywords$kw_final<-str_replace(keywords$kw_final, "térraba sierpe", "sierpe térraba")
  keywords$kw_final<-str_replace(keywords$kw_final, "unobservable states", "unobservable state")
  keywords$kw_final<-str_replace(keywords$kw_final, "water flea", "waterflea")
  keywords$kw_final<-str_replace(keywords$kw_final, "water resources", "water resource")
  keywords$kw_final<-str_replace(keywords$kw_final, "water strider", "waterstrider")
  keywords$kw_final<-str_replace(keywords$kw_final, "water striders", "waterstrider")
  keywords$kw_final<-str_replace(keywords$kw_final, "wildlife vehicle collision", "wildlife-vehicle collision")
  keywords$kw_final<-str_replace(keywords$kw_final, "wildlife vehicle collisions", "wildlife-vehicle collisions")
  keywords$kw_final<-str_replace(keywords$kw_final, "wing-thorax ratio", "wing:thorax ratio")
  keywords$kw_final<-str_replace(keywords$kw_final, " 18s rdna", "18s rdna")
  keywords$kw_final<-str_replace(keywords$kw_final, " proteobacteria", "proteobacteria")
  keywords$kw_final<-str_replace(keywords$kw_final, "%plant population and community dynamics", "plant population and community dynamics")
  keywords$kw_final<-str_replace(keywords$kw_final, "15  n", "15n")
  keywords$kw_final<-str_replace(keywords$kw_final, "15 n stable isotope", "15n stable isotope")
  keywords$kw_final<-str_replace(keywords$kw_final, "16srdna", "16s rdna")
  keywords$kw_final<-str_replace(keywords$kw_final, "a triplex patula", "atriplex patula")
  keywords$kw_final<-str_replace(keywords$kw_final, "above  and belowground herbivory", "above and belowground herbivory")
  keywords$kw_final<-str_replace(keywords$kw_final, "above ground", "aboveground")
  keywords$kw_final<-str_replace(keywords$kw_final, "above ground-below ground interactions", "aboveground-belowground interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "aboveground net primary productivity  anpp", "anpp")
  keywords$kw_final<-str_replace(keywords$kw_final, "aboveground-belowground interactions", "aboveground-belowground interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "abundance mass scaling", "mass-abundance scaling")
  keywords$kw_final<-str_replace(keywords$kw_final, "abundance occupancy", "abundance-occupancy")
  keywords$kw_final<-str_replace(keywords$kw_final, "abundance: ant", "ant abundance")
  keywords$kw_final<-str_replace(keywords$kw_final, "acalymma vittatum", "acalymma vitattum")
  keywords$kw_final<-str_replace(keywords$kw_final, "acquisition conservation trade-off", "acquisition-conservation trade off")
  keywords$kw_final<-str_replace(keywords$kw_final, "acyrthosiphon pisum pea aphid", "acyrthosiphon pisum, pea aphid")
  keywords$kw_final<-str_replace(keywords$kw_final, "adalia bipunctata l", "adalia bipunctata")
  keywords$kw_final<-str_replace(keywords$kw_final, "adaptive suites", "adaptive suite")
  keywords$kw_final<-str_replace(keywords$kw_final, "africanized honey bees", "africanized honeybees")
  keywords$kw_final<-str_replace(keywords$kw_final, "agama agama", "agama")
  keywords$kw_final<-str_replace(keywords$kw_final, "age structures", "age structure")
  keywords$kw_final<-str_replace(keywords$kw_final, "agrofo restry", "agroforestry")
  keywords$kw_final<-str_replace(keywords$kw_final, "algal", "alga")
  keywords$kw_final<-str_replace(keywords$kw_final, "alternative stable community states", "alternative community stable state")
  keywords$kw_final<-str_replace(keywords$kw_final, "amazonía", "amazonia")
  keywords$kw_final<-str_replace(keywords$kw_final, "ameiva ameiva", "ameiva")
  keywords$kw_final<-str_replace(keywords$kw_final, "ancestor reconstructions", "ancestor reconstruction")
  keywords$kw_final<-str_replace(keywords$kw_final, "ancestral reconstructions", "ancestral reconstruction")
  keywords$kw_final<-str_replace(keywords$kw_final, "and predators", "predators and")
  keywords$kw_final<-str_replace(keywords$kw_final, "anguilla anguilla", "anguilla")
  keywords$kw_final<-str_replace(keywords$kw_final, "anser anser", "anser")
  keywords$kw_final<-str_replace(keywords$kw_final, "anti fouling", "antifouling")
  keywords$kw_final<-str_replace(keywords$kw_final, "anti fungal compounds", "antifungal compounds")
  keywords$kw_final<-str_replace(keywords$kw_final, "anti inflammatory agent", "antiinflammatory agent")
  keywords$kw_final<-str_replace(keywords$kw_final, "anti inflammmatory agent", "antiinflammatory agent")
  keywords$kw_final<-str_replace(keywords$kw_final, "anti predator", "antipredator")
  keywords$kw_final<-str_replace(keywords$kw_final, "anti predator adaptation", "antipredator adaptation")
  keywords$kw_final<-str_replace(keywords$kw_final, "anti predator defenses", "antipredator defenses")
  keywords$kw_final<-str_replace(keywords$kw_final, "anti predator response", "antipredator response")
  keywords$kw_final<-str_replace(keywords$kw_final, "anti predator responses", "antipredator response")
  keywords$kw_final<-str_replace(keywords$kw_final, "antipredator responses", "antipredator response")
  keywords$kw_final<-str_replace(keywords$kw_final, "antipredator-antipredator strategy", "antipredator strategy")
  keywords$kw_final<-str_replace(keywords$kw_final, "ant-aphid mutualism", "ant-aphid mutualism")
  keywords$kw_final<-str_replace(keywords$kw_final, "aphis fabae fabae", "aphis fabae")
  keywords$kw_final<-str_replace(keywords$kw_final, "aquatic terrestrial linkage", "aquatic-terrestrial linkage")
  keywords$kw_final<-str_replace(keywords$kw_final, "aquatic-terrestrial linkage", "aquatic-terrestrial linkage")
  keywords$kw_final<-str_replace(keywords$kw_final, "arctic tree line", "arctic treeline")
  keywords$kw_final<-str_replace(keywords$kw_final, "aspirochidotida", "aspidochirotida")
  keywords$kw_final<-str_replace(keywords$kw_final, "associational plant defense", "plant associational defense")
  keywords$kw_final<-str_replace(keywords$kw_final, "atta ceph alotes", "atta cephalotes")
  keywords$kw_final<-str_replace(keywords$kw_final, "attraction production", "attraction-production")
  keywords$kw_final<-str_replace(keywords$kw_final, "auto correlation", "autocorrelation")
  keywords$kw_final<-str_replace(keywords$kw_final, "avian inter specific brood parasitism", "avian interspecific brood parasitism")
  keywords$kw_final<-str_replace(keywords$kw_final, "bahía de la ascensión", "bahia de la ascensión")
  keywords$kw_final<-str_replace(keywords$kw_final, "base line", "baseline")
  keywords$kw_final<-str_replace(keywords$kw_final, "bateson dobzhansky-müller incompatibility", "bateson dobzhansky-muller incompatibility")
  keywords$kw_final<-str_replace(keywords$kw_final, "behavioral trade off", "behavioral tradeoff")
  keywords$kw_final<-str_replace(keywords$kw_final, "below ground", "belowground")
  keywords$kw_final<-str_replace(keywords$kw_final, "benefit cost analysis", "cost-benefit analysis")
  keywords$kw_final<-str_replace(keywords$kw_final, "benefits of-philopatry", "benefits of philopatry")
  keywords$kw_final<-str_replace(keywords$kw_final, "betula papyri fera", "betula papyrifera")
  keywords$kw_final<-str_replace(keywords$kw_final, "bill fish", "billfish")
  keywords$kw_final<-str_replace(keywords$kw_final, "bio concentration", "bioconcentration")
  keywords$kw_final<-str_replace(keywords$kw_final, "bio invasion", "bioinvasion")
  keywords$kw_final<-str_replace(keywords$kw_final, "biodiversity ecosystem function relationships", "biodiversity-ecosystem function relationships")
  keywords$kw_final<-str_replace(keywords$kw_final, "biodiversity ecosystem-function", "biodiversity-ecosystem function")
  keywords$kw_final<-str_replace(keywords$kw_final, "biodiversity hot spot", "biodiversity hotspots")
  keywords$kw_final<-str_replace(keywords$kw_final, "biodiversity productivity relationships", "biodiversity-productivity relationships")
  keywords$kw_final<-str_replace(keywords$kw_final, "biodiversity rapid assessment", "rapid biodiversity assessment")
  keywords$kw_final<-str_replace(keywords$kw_final, "biodiversity-ecosystem function relationships", "biodiversity-ecosystem function relationships")
  keywords$kw_final<-str_replace(keywords$kw_final, "biodiversity-ecosystem functioning relationship", "biodiversity-ecosystem functioning relationship")
  keywords$kw_final<-str_replace(keywords$kw_final, "biodiversity-ecosystem functioning", "biodiversity-ecosystem functioning")
  keywords$kw_final<-str_replace(keywords$kw_final, "biogeographic chocó region", "chocó biogeographic region")
  keywords$kw_final<-str_replace(keywords$kw_final, "biosphere reserves", "biosphere reserve")
  keywords$kw_final<-str_replace(keywords$kw_final, "bird watching", "birdwatching")
  keywords$kw_final<-str_replace(keywords$kw_final, "black legged tick", "blacklegged tick")
  keywords$kw_final<-str_replace(keywords$kw_final, "blow flies", "blowflies")
  keywords$kw_final<-str_replace(keywords$kw_final, "blow fly", "blowfly")
  keywords$kw_final<-str_replace(keywords$kw_final, "boots trapping", "bootstrapping")
  keywords$kw_final<-str_replace(keywords$kw_final, "brackishwater", "brackish water")
  keywords$kw_final<-str_replace(keywords$kw_final, "branta bernicla-nigricans", "branta bernicla nigricans")
  keywords$kw_final<-str_replace(keywords$kw_final, "brazilianatlanticforest", "brazilian atlantic forest")
  keywords$kw_final<-str_replace(keywords$kw_final, "bumble bee pollination", "bumblebee pollination")
  keywords$kw_final<-str_replace(keywords$kw_final, "bunch grass", "bunchgrass")
  keywords$kw_final<-str_replace(keywords$kw_final, "buried seeds", "buried seed")
  keywords$kw_final<-str_replace(keywords$kw_final, "bush meat", "bushmeat")
  keywords$kw_final<-str_replace(keywords$kw_final, "bycatch shrimp", "shrimp bycatch")
  keywords$kw_final<-str_replace(keywords$kw_final, "bêche de-mer", "beche de-mer")
  keywords$kw_final<-str_replace(keywords$kw_final, "c  3", "c3")
  keywords$kw_final<-str_replace(keywords$kw_final, "c  4", "c4")
  keywords$kw_final<-str_replace(keywords$kw_final, "c  grassland 4", "c4 grassland")
  keywords$kw_final<-str_replace(keywords$kw_final, "c  grassland4", "c4 grassland")
  keywords$kw_final<-str_replace(keywords$kw_final, "c allocation", "allocation")
  keywords$kw_final<-str_replace(keywords$kw_final, "c n ratio", "c:n ratio")
  keywords$kw_final<-str_replace(keywords$kw_final, "c n ratios", "c:n ratios")
  keywords$kw_final<-str_replace(keywords$kw_final, "c-n", "c:n")
  keywords$kw_final<-str_replace(keywords$kw_final, "c-n ratio", "c:n ratio")
  keywords$kw_final<-str_replace(keywords$kw_final, "cacao theobroma cacao", "theobroma cacao")
  keywords$kw_final<-str_replace(keywords$kw_final, "calidris canutus canutus", "calidris canutus")
  keywords$kw_final<-str_replace(keywords$kw_final, "calling songs", "calling song")
  keywords$kw_final<-str_replace(keywords$kw_final, "cannon ball", "cannonball")
  keywords$kw_final<-str_replace(keywords$kw_final, "canopy turn over times", "canopy turnover times")
  keywords$kw_final<-str_replace(keywords$kw_final, "capture mark recapture", "capture mark-recapture")
  keywords$kw_final<-str_replace(keywords$kw_final, "capture-mark-recapture analysis", "capture-mark-recapture analysis")
  keywords$kw_final<-str_replace(keywords$kw_final, "capture-mark-recapture models", "capture mark-recapture models")
  keywords$kw_final<-str_replace(keywords$kw_final, "carbohydrate reserves", "carbohydrate reserve")
  keywords$kw_final<-str_replace(keywords$kw_final, "carbon : nitrogen ratio", "c:n ratio")
  keywords$kw_final<-str_replace(keywords$kw_final, "carbon to phosphorus ratio", "c:p ratio")
  keywords$kw_final<-str_replace(keywords$kw_final, "carbon to-phosphorus ratio", "c:p ratio")
  keywords$kw_final<-str_replace(keywords$kw_final, "carbon to-phosphorus ratios", "c:p ratio")
  keywords$kw_final<-str_replace(keywords$kw_final, "carbon-nitrogen ratio", "c:n ratio")
  keywords$kw_final<-str_replace(keywords$kw_final, "carbon: nitrogen ratio", "c:n ratio")
  keywords$kw_final<-str_replace(keywords$kw_final, "carbon:nitrogen ratio", "c:n ratio")
  keywords$kw_final<-str_replace(keywords$kw_final, "caryoph yllene oxide", "caryophyllene oxide")
  keywords$kw_final<-str_replace(keywords$kw_final, "case  control", "case control")
  keywords$kw_final<-str_replace(keywords$kw_final, "character transitions", "character transition")
  keywords$kw_final<-str_replace(keywords$kw_final, "characteristic timescales", "characteristic timescale")
  keywords$kw_final<-str_replace(keywords$kw_final, "chlorophylla", "chlorophyll a")
  keywords$kw_final<-str_replace(keywords$kw_final, "choco biogeographic region", "chocó biogeographic region")
  keywords$kw_final<-str_replace(keywords$kw_final, "chocó biogeográfico", "choco biogeografico")
  keywords$kw_final<-str_replace(keywords$kw_final, "clarkia xantiana ssp", "clarkia xantiana")
  keywords$kw_final<-str_replace(keywords$kw_final, "clear cut", "clearcut")
  keywords$kw_final<-str_replace(keywords$kw_final, "cloudforest", "cloud forest")
  keywords$kw_final<-str_replace(keywords$kw_final, "co diversification", "codiversification")
  keywords$kw_final<-str_replace(keywords$kw_final, "co extinction", "coextinction")
  keywords$kw_final<-str_replace(keywords$kw_final, "co limitation nutrient", "nutrient colimitation")
  keywords$kw_final<-str_replace(keywords$kw_final, "co variation", "covariation")
  keywords$kw_final<-str_replace(keywords$kw_final, "coalescence times", "coalescence time")
  keywords$kw_final<-str_replace(keywords$kw_final, "coastal atlantic forest", "atlantic coastal forest")
  keywords$kw_final<-str_replace(keywords$kw_final, "coevolutionary hot spot", "coevolutionary hotspot")
  keywords$kw_final<-str_replace(keywords$kw_final, "coevolutionary hotspots", "coevolutionary hotspot")
  keywords$kw_final<-str_replace(keywords$kw_final, "coffee agro ecosystem", "coffee agroecosystem")
  keywords$kw_final<-str_replace(keywords$kw_final, "cold frontsoutbreaks", "cold fronts outbreaks")
  keywords$kw_final<-str_replace(keywords$kw_final, "community and population dynamics", "population and community dynamics")
  keywords$kw_final<-str_replace(keywords$kw_final, "community weighted-mean", "community weighted mean")
  keywords$kw_final<-str_replace(keywords$kw_final, "competition  colonization tradeoff", "competition-colonization tradeoff")
  keywords$kw_final<-str_replace(keywords$kw_final, "competition colonization tradeoff", "competition-colonization tradeoff")
  keywords$kw_final<-str_replace(keywords$kw_final, "competition-colonization trade off", "competition-colonization tradeoff")
  keywords$kw_final<-str_replace(keywords$kw_final, "competition-colonization trade off", "competition-colonization tradeoff")
  keywords$kw_final<-str_replace(keywords$kw_final, "competition-colonization tradeoff", "competition-colonization tradeoff")
  keywords$kw_final<-str_replace(keywords$kw_final, "compound specific stable-isotope analysis", "compound specific stable isotope analysis")
  keywords$kw_final<-str_replace(keywords$kw_final, "conservation in situ", "in situ conservation")
  keywords$kw_final<-str_replace(keywords$kw_final, "conservation species", "species conservation")
  keywords$kw_final<-str_replace(keywords$kw_final, "conspecific interactions", "conspecific interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "cor egonus", "coregonus")
  keywords$kw_final<-str_replace(keywords$kw_final, "cormack jolly seber", "cormack-jolly-seber")
  keywords$kw_final<-str_replace(keywords$kw_final, "cormack jolly seber model", "cormack-jolly-seber model")
  keywords$kw_final<-str_replace(keywords$kw_final, "cormack-jolly-seber", "cormack-jolly-seber")
  keywords$kw_final<-str_replace(keywords$kw_final, "corona virus", "coronavirus")
  keywords$kw_final<-str_replace(keywords$kw_final, "corridor dispersal", "dispersal corridor")
  keywords$kw_final<-str_replace(keywords$kw_final, "cost benefit ratio", "cost-benefit ratio")
  keywords$kw_final<-str_replace(keywords$kw_final, "cost of-reproduction", "cost of reproduction")
  keywords$kw_final<-str_replace(keywords$kw_final, "cost-benefit analysis", "cost-benefit analysis")
  keywords$kw_final<-str_replace(keywords$kw_final, "cost-benefit ratio", "cost-benefit ratio")
  keywords$kw_final<-str_replace(keywords$kw_final, "cost: benefit analysis", "cost-benefit analysis")
  keywords$kw_final<-str_replace(keywords$kw_final, "costofreproduction", "cost of reproduction")
  keywords$kw_final<-str_replace(keywords$kw_final, "cote divoire", "ivory coast")
  keywords$kw_final<-str_replace(keywords$kw_final, "coyote  canis latrans ", "coyote canis latrans")
  keywords$kw_final<-str_replace(keywords$kw_final, "cross breeding", "crossbreeding")
  keywords$kw_final<-str_replace(keywords$kw_final, "cross dating", "crossdating")
  keywords$kw_final<-str_replace(keywords$kw_final, "cross talk", "crosstalk")
  keywords$kw_final<-str_replace(keywords$kw_final, "crossfostering", "cross fostering")
  keywords$kw_final<-str_replace(keywords$kw_final, "crotaphytus collaris collaris", "crotaphytus collaris")
  keywords$kw_final<-str_replace(keywords$kw_final, "crown of thorns starfish", "crown-of-thorns starfish")
  keywords$kw_final<-str_replace(keywords$kw_final, "ct  max", "ct max")
  keywords$kw_final<-str_replace(keywords$kw_final, "cvalue", "c value")
  keywords$kw_final<-str_replace(keywords$kw_final, "cycling population", "population cycling")
  keywords$kw_final<-str_replace(keywords$kw_final, "cyto nuclear coevolution", "cytonuclear coevolution")
  keywords$kw_final<-str_replace(keywords$kw_final, "cyto nuclear epistasis", "cytonuclear epistasis")
  keywords$kw_final<-str_replace(keywords$kw_final, "c¬∑n ratio", "c:n ratio")
  keywords$kw_final<-str_replace(keywords$kw_final, "cô", "co")
  keywords$kw_final<-str_replace(keywords$kw_final, "data base", "database")
  keywords$kw_final<-str_replace(keywords$kw_final, "data model comparison", "model-data comparison")
  keywords$kw_final<-str_replace(keywords$kw_final, "deep water fishery", "deepwater fishery")
  keywords$kw_final<-str_replace(keywords$kw_final, "defense syndromes", "defense syndrome")
  keywords$kw_final<-str_replace(keywords$kw_final, "degradedlands", "degraded lands")
  keywords$kw_final<-str_replace(keywords$kw_final, "delay time", "time delay")
  keywords$kw_final<-str_replace(keywords$kw_final, "delayed responses", "delayed response")
  keywords$kw_final<-str_replace(keywords$kw_final, "deleterious recessives", "deleterious recessive")
  keywords$kw_final<-str_replace(keywords$kw_final, "delta c 13", "delta c13")
  keywords$kw_final<-str_replace(keywords$kw_final, "delta c-13", "delta c13")
  keywords$kw_final<-str_replace(keywords$kw_final, "delta n 15", "delta n15")
  keywords$kw_final<-str_replace(keywords$kw_final, "density estimates", "density estimate")
  keywords$kw_final<-str_replace(keywords$kw_final, "densitymediated indirect effect", "density-mediated indirect effect")
  keywords$kw_final<-str_replace(keywords$kw_final, "development model", "model development")
  keywords$kw_final<-str_replace(keywords$kw_final, "di nitrogen fixation", "dinitrogen fixation")
  keywords$kw_final<-str_replace(keywords$kw_final, "diamond back moth", "diamondback moth")
  keywords$kw_final<-str_replace(keywords$kw_final, "diasporas", "diaspora")
  keywords$kw_final<-str_replace(keywords$kw_final, "diffusion advection model", "advection diffusion model")
  keywords$kw_final<-str_replace(keywords$kw_final, "dik dik", "dik-dik")
  keywords$kw_final<-str_replace(keywords$kw_final, "dikdik", "dik-dik")
  keywords$kw_final<-str_replace(keywords$kw_final, "dispersal condition dependent", "condition-dependent dispersal")
  keywords$kw_final<-str_replace(keywords$kw_final, "dispersal corridors", "dispersal corridor")
  keywords$kw_final<-str_replace(keywords$kw_final, "dissassortative mating", "disassortative mating")
  keywords$kw_final<-str_replace(keywords$kw_final, "distance dispersal", "dispersal distance")
  keywords$kw_final<-str_replace(keywords$kw_final, "distribution abundance", "distribution-abundance")
  keywords$kw_final<-str_replace(keywords$kw_final, "distribution probability", "probability distribution")
  keywords$kw_final<-str_replace(keywords$kw_final, "distribution range size", "range size distribution")
  keywords$kw_final<-str_replace(keywords$kw_final, "disturbance and soil biodiversity", "soil biodiversity and disturbance")
  keywords$kw_final<-str_replace(keywords$kw_final, "divergence with-gene flow", "divergence with gene flow")
  keywords$kw_final<-str_replace(keywords$kw_final, "diversity disease relationship", "diversity-disease relationship")
  keywords$kw_final<-str_replace(keywords$kw_final, "diversity disturbance relationship", "diversity-disturbance relationship")
  keywords$kw_final<-str_replace(keywords$kw_final, "diversity estimators", "diversity estimator")
  keywords$kw_final<-str_replace(keywords$kw_final, "diversity hotspots", "diversity hotspot")
  keywords$kw_final<-str_replace(keywords$kw_final, "diversity invasibility", "diversity-invasibility")
  keywords$kw_final<-str_replace(keywords$kw_final, "diversity invasibility hypothesis", "diversity-invasibility hypothesis")
  keywords$kw_final<-str_replace(keywords$kw_final, "diversity-disturbance relationship", "diversity-disturbance relationship")
  keywords$kw_final<-str_replace(keywords$kw_final, "diversity-invasibility hypothesis", "diversity-invasibility hypothesis")
  keywords$kw_final<-str_replace(keywords$kw_final, "dn-d(s)", "dn-ds")
  keywords$kw_final<-str_replace(keywords$kw_final, "dna bar coding", "dna barcoding")
  keywords$kw_final<-str_replace(keywords$kw_final, "dna dna hybridization", "dna hybridization")
  keywords$kw_final<-str_replace(keywords$kw_final, "dolphin fish", "dolphinfish")
  keywords$kw_final<-str_replace(keywords$kw_final, "dominance diversity", "dominance-diversity")
  keywords$kw_final<-str_replace(keywords$kw_final, "dominance-diversity", "dominance-diversity")
  keywords$kw_final<-str_replace(keywords$kw_final, "donana national park", "donana")
  keywords$kw_final<-str_replace(keywords$kw_final, "donaña", "donana")
  keywords$kw_final<-str_replace(keywords$kw_final, "donãna national park", "donana")
  keywords$kw_final<-str_replace(keywords$kw_final, "dose response curves", "dose-response curve")
  keywords$kw_final<-str_replace(keywords$kw_final, "dry alpine meadow", "alpine dry meadow")
  keywords$kw_final<-str_replace(keywords$kw_final, "dynamic metapopulation", "metapopulation dynamics")
  keywords$kw_final<-str_replace(keywords$kw_final, "e laeis guineensis", "elaeis guineensis")
  keywords$kw_final<-str_replace(keywords$kw_final, "e scape", "escape")
  keywords$kw_final<-str_replace(keywords$kw_final, "east tropical pacific", "tropical east pacific")
  keywords$kw_final<-str_replace(keywords$kw_final, "eco genomics", "ecogenomics")
  keywords$kw_final<-str_replace(keywords$kw_final, "eco hydrology", "ecohydrology")
  keywords$kw_final<-str_replace(keywords$kw_final, "eco morphology", "ecomorphology")
  keywords$kw_final<-str_replace(keywords$kw_final, "eco phylogenetics", "ecophylogenetics")
  keywords$kw_final<-str_replace(keywords$kw_final, "eco regions", "ecoregions")
  keywords$kw_final<-str_replace(keywords$kw_final, "ecological transitions", "ecological transition")
  keywords$kw_final<-str_replace(keywords$kw_final, "ecoregión", "ecoregion")
  keywords$kw_final<-str_replace(keywords$kw_final, "ecosystem function and ecosystem services", "ecosystem function and services")
  keywords$kw_final<-str_replace(keywords$kw_final, "ecosystem function and structure", "ecosystem structure and function")
  keywords$kw_final<-str_replace(keywords$kw_final, "ecosystem responses", "ecosystem response")
  keywords$kw_final<-str_replace(keywords$kw_final, "eggload", "egg load")
  keywords$kw_final<-str_replace(keywords$kw_final, "el nino drought", "enso drought")
  keywords$kw_final<-str_replace(keywords$kw_final, "el ninõ southern oscillation", "enso")
  keywords$kw_final<-str_replace(keywords$kw_final, "el niño drought", "enso drought")
  keywords$kw_final<-str_replace(keywords$kw_final, "el niño-southern oscillation", "enso")
  keywords$kw_final<-str_replace(keywords$kw_final, "el niño-southern oscillation", "enso")
  keywords$kw_final<-str_replace(keywords$kw_final, "el nī no southern oscillation enso", "enso")
  keywords$kw_final<-str_replace(keywords$kw_final, "elevated [co ] 2", "elevated co2")
  keywords$kw_final<-str_replace(keywords$kw_final, "ende mism", "endemism")
  keywords$kw_final<-str_replace(keywords$kw_final, "enemyfree space", "enemy-free space")
  keywords$kw_final<-str_replace(keywords$kw_final, "enemyrelease hypothesis", "enemy release hypothesis")
  keywords$kw_final<-str_replace(keywords$kw_final, "energetic trade off", "energetic tradeoff")
  keywords$kw_final<-str_replace(keywords$kw_final, "ensifera ensifera", "ensifera")
  keywords$kw_final<-str_replace(keywords$kw_final, "environmental education &", "environmental education")
  keywords$kw_final<-str_replace(keywords$kw_final, "environmental responses", "environmental response")
  keywords$kw_final<-str_replace(keywords$kw_final, "espinhaço range", "espinhaco range")
  keywords$kw_final<-str_replace(keywords$kw_final, "estimates", "estimate")
  keywords$kw_final<-str_replace(keywords$kw_final, "eulemur fulvus fulvus", "eulemur fulvus")
  keywords$kw_final<-str_replace(keywords$kw_final, "everglades florida", "florida everglades")
  keywords$kw_final<-str_replace(keywords$kw_final, "evergreen broad leaved forest", "evergreen broadleaved forest")
  keywords$kw_final<-str_replace(keywords$kw_final, "evergreen lowland rainforest", "lowland evergreen rainforest")
  keywords$kw_final<-str_replace(keywords$kw_final, "evolution and ecology", "ecology and evolution")
  keywords$kw_final<-str_replace(keywords$kw_final, "evolutionarily stable strategie", "evolutionarily stable strategies")
  keywords$kw_final<-str_replace(keywords$kw_final, "evolutionary responses", "evolutionary response")
  keywords$kw_final<-str_replace(keywords$kw_final, "ex tinction", "extinction")
  keywords$kw_final<-str_replace(keywords$kw_final, "exap tation", "exaptation")
  keywords$kw_final<-str_replace(keywords$kw_final, "exotic annual grasses", "annual exotic grasses")
  keywords$kw_final<-str_replace(keywords$kw_final, "exotic invasive plants", "invasive exotic plants")
  keywords$kw_final<-str_replace(keywords$kw_final, "exploration exploitation trade-off", "exploration-exploitation trade-off")
  keywords$kw_final<-str_replace(keywords$kw_final, "exploration-exploitation trade off", "exploration-exploitation trade-off")
  keywords$kw_final<-str_replace(keywords$kw_final, "extinction-colonization", "extinction-colonization")
  keywords$kw_final<-str_replace(keywords$kw_final, "extra floral nectar", "extrafloral nectar")
  keywords$kw_final<-str_replace(keywords$kw_final, "extra floral nectary", "extrafloral nectary")
  keywords$kw_final<-str_replace(keywords$kw_final, "eye span", "eyespan")
  keywords$kw_final<-str_replace(keywords$kw_final, "f  statistics", "f statistics")
  keywords$kw_final<-str_replace(keywords$kw_final, "face free air co  enrichment 2", "face")
  keywords$kw_final<-str_replace(keywords$kw_final, "factors controlling", "controlling factors")
  keywords$kw_final<-str_replace(keywords$kw_final, "far red ratio", "red:far red ratio")
  keywords$kw_final<-str_replace(keywords$kw_final, "farm yard manure", "farmyard manure")
  keywords$kw_final<-str_replace(keywords$kw_final, "fat body", "body fat")
  keywords$kw_final<-str_replace(keywords$kw_final, "female female competition", "female-female competition")
  keywords$kw_final<-str_replace(keywords$kw_final, "female post mating response", "post-mating female response")
  keywords$kw_final<-str_replace(keywords$kw_final, "fi re ant", "fire ant")
  keywords$kw_final<-str_replace(keywords$kw_final, "fire herbivore interactions", "fire-herbivore interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "fl uvial erosion", "fluvial erosion")
  keywords$kw_final<-str_replace(keywords$kw_final, "flood plain", "floodplain")
  keywords$kw_final<-str_replace(keywords$kw_final, "flood plain lakes", "floodplain lakes")
  keywords$kw_final<-str_replace(keywords$kw_final, "flood plains", "floodplains")
  keywords$kw_final<-str_replace(keywords$kw_final, "fluctuatingselection", "fluctuating selection")
  keywords$kw_final<-str_replace(keywords$kw_final, "forest complex", "complex forest")
  keywords$kw_final<-str_replace(keywords$kw_final, "forest savanna boundary", "savanna-forest boundary")
  keywords$kw_final<-str_replace(keywords$kw_final, "forest specialists", "forest specialist")
  keywords$kw_final<-str_replace(keywords$kw_final, "forest uses", "forest use")
  keywords$kw_final<-str_replace(keywords$kw_final, "forestland", "forest land")
  keywords$kw_final<-str_replace(keywords$kw_final, "form function relationship", "form-function relationship")
  keywords$kw_final<-str_replace(keywords$kw_final, "formfunction relationship", "form-function relationship")
  keywords$kw_final<-str_replace(keywords$kw_final, "fourthcorner problem", "fourth corner problem")
  keywords$kw_final<-str_replace(keywords$kw_final, "frequency ", "frequency")
  keywords$kw_final<-str_replace(keywords$kw_final, "fresh water ecology", "freshwater ecology")
  keywords$kw_final<-str_replace(keywords$kw_final, "fresh water fishes", "freshwater fishes")
  keywords$kw_final<-str_replace(keywords$kw_final, "fresh water shrimps", "freshwater shrimps")
  keywords$kw_final<-str_replace(keywords$kw_final, "freshwater lakes", "freshwater lake")
  keywords$kw_final<-str_replace(keywords$kw_final, "fruitbats", "fruit bats")
  keywords$kw_final<-str_replace(keywords$kw_final, "fruitfly", "fruit fly")
  keywords$kw_final<-str_replace(keywords$kw_final, "fruitset", "fruit set")
  keywords$kw_final<-str_replace(keywords$kw_final, "functional plant group", "plant functional group")
  keywords$kw_final<-str_replace(keywords$kw_final, "g eospiza", "geospiza")
  keywords$kw_final<-str_replace(keywords$kw_final, "g max", "gmax")
  keywords$kw_final<-str_replace(keywords$kw_final, "g x e interaction", "gxe")
  keywords$kw_final<-str_replace(keywords$kw_final, "gall insect", "insect gall")
  keywords$kw_final<-str_replace(keywords$kw_final, "gallmaker", "gall maker")
  keywords$kw_final<-str_replace(keywords$kw_final, "gallwasp", "gall wasp")
  keywords$kw_final<-str_replace(keywords$kw_final, "gc ms analysis", "gc-ms analysis")
  keywords$kw_final<-str_replace(keywords$kw_final, "gc-ms analysis", "gc-ms analysis")
  keywords$kw_final<-str_replace(keywords$kw_final, "gekkonida e", "gekkonidae")
  keywords$kw_final<-str_replace(keywords$kw_final, "gene by-environment interaction", "gxe")
  keywords$kw_final<-str_replace(keywords$kw_final, "gene culture co-evolution", "gene culture coevolution")
  keywords$kw_final<-str_replace(keywords$kw_final, "geneculture coevolution", "gene culture coevolution")
  keywords$kw_final<-str_replace(keywords$kw_final, "generalist specialist trade-offs", "generalist-specialist trade-offs")
  keywords$kw_final<-str_replace(keywords$kw_final, "genetic structure and diversity", "genetic diversity and structure")
  keywords$kw_final<-str_replace(keywords$kw_final, "genome wide association studies", "gwas")
  keywords$kw_final<-str_replace(keywords$kw_final, "genomewide association studies", "gwas")
  keywords$kw_final<-str_replace(keywords$kw_final, "genotype by genotype by environment", "gxe")
  keywords$kw_final<-str_replace(keywords$kw_final, "genotype by genotype interactions", "gxe")
  keywords$kw_final<-str_replace(keywords$kw_final, "genotype environment associations", "gxe")
  keywords$kw_final<-str_replace(keywords$kw_final, "genotype environment correlation", "genotype-environment correlation")
  keywords$kw_final<-str_replace(keywords$kw_final, "genotypephenotype map", "genotype-phenotype map")
  keywords$kw_final<-str_replace(keywords$kw_final, "genotype x environment interaction", "gxe")
  keywords$kw_final<-str_replace(keywords$kw_final, "genotype-environment associations", "gxe")
  keywords$kw_final<-str_replace(keywords$kw_final, "gis geographic information system", "gis")
  keywords$kw_final<-str_replace(keywords$kw_final, "giving up-density", "giving-up density")
  keywords$kw_final<-str_replace(keywords$kw_final, "glossophaga commissarisi", "glosophaga commissarisi")
  keywords$kw_final<-str_replace(keywords$kw_final, "gold fish", "goldfish")
  keywords$kw_final<-str_replace(keywords$kw_final, "golfo de-california", "golfo de california")
  keywords$kw_final<-str_replace(keywords$kw_final, "golfo de-nicoya", "golfo de nicoya")
  keywords$kw_final<-str_replace(keywords$kw_final, "gonado somatic index", "gonado-somatic index")
  keywords$kw_final<-str_replace(keywords$kw_final, "goodness of-fit", "goodness of fit")
  keywords$kw_final<-str_replace(keywords$kw_final, "grass tree coexistence", "tree-grass coexistence")
  keywords$kw_final<-str_replace(keywords$kw_final, "grasses and forbs", "forbs and grasses")
  keywords$kw_final<-str_replace(keywords$kw_final, "grass-fire cycle", "grass-fire cycle")
  keywords$kw_final<-str_replace(keywords$kw_final, "green beard", "greenbeard")
  keywords$kw_final<-str_replace(keywords$kw_final, "green beards", "greenbeard")
  keywords$kw_final<-str_replace(keywords$kw_final, "ground water", "groundwater")
  keywords$kw_final<-str_replace(keywords$kw_final, "growth climate relationship", "climate-growth relationship")
  keywords$kw_final<-str_replace(keywords$kw_final, "growth climate responses", "climate growth responses")
  keywords$kw_final<-str_replace(keywords$kw_final, "growth defense trade off", "growth-defense tradeoff")
  keywords$kw_final<-str_replace(keywords$kw_final, "growth differentiation-balance hypothesis", "growth differentiation balance hypothesis")
  keywords$kw_final<-str_replace(keywords$kw_final, "growth responses", "growth response")
  keywords$kw_final<-str_replace(keywords$kw_final, "growth-defense trade off", "growth-defense tradeoff")
  keywords$kw_final<-str_replace(keywords$kw_final, "gulf california", "california gulf")
  keywords$kw_final<-str_replace(keywords$kw_final, "g x e", "gxe")
  keywords$kw_final<-str_replace(keywords$kw_final, "g(st)", "gst")
  keywords$kw_final<-str_replace(keywords$kw_final, "habitat fragmentation and loss", "habitat loss and fragmentation")
  keywords$kw_final<-str_replace(keywords$kw_final, "habitat matrix", "matrix habitat")
  keywords$kw_final<-str_replace(keywords$kw_final, "habitat specialists", "habitat specialist")
  keywords$kw_final<-str_replace(keywords$kw_final, "hair root", "root hair")
  keywords$kw_final<-str_replace(keywords$kw_final, "haplo diploid", "haplodiploid")
  keywords$kw_final<-str_replace(keywords$kw_final, "haplo diploid sex determination", "haplodiploid sex determination")
  keywords$kw_final<-str_replace(keywords$kw_final, "hawai ªi", "hawaii")
  keywords$kw_final<-str_replace(keywords$kw_final, "hawk moths", "hawkmoths")
  keywords$kw_final<-str_replace(keywords$kw_final, "height vegetation", "vegetation height")
  keywords$kw_final<-str_replace(keywords$kw_final, "hemi epiphyte", "hemiepiphyte")
  keywords$kw_final<-str_replace(keywords$kw_final, "herbchronology", "herb chronology")
  keywords$kw_final<-str_replace(keywords$kw_final, "herbivore herbivore interactions", "herbivore-herbivore interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "herbivore induced plant response", "herbivore-induced plant response")
  keywords$kw_final<-str_replace(keywords$kw_final, "herbivore induced plant responses", "herbivore-induced plant response")
  keywords$kw_final<-str_replace(keywords$kw_final, "herbivore interactions", "herbivore-herbivore interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "herbivore plant dynamics", "plant-herbivore dynamics")
  keywords$kw_final<-str_replace(keywords$kw_final, "herbivore plant interaction", "plant-herbivore interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "heterospecificpollen", "heterospecific pollen")
  keywords$kw_final<-str_replace(keywords$kw_final, "heterozygosity-fitness correlation", "heterozygosity-fitness correlation")
  keywords$kw_final<-str_replace(keywords$kw_final, "hind limb", "hindlimb")
  keywords$kw_final<-str_replace(keywords$kw_final, "hluhluwe-imfolozi park", "hluhluwe imfolozi park")
  keywords$kw_final<-str_replace(keywords$kw_final, "hollow tree", "tree hollow")
  keywords$kw_final<-str_replace(keywords$kw_final, "holo epiphyte", "holoepiphyte")
  keywords$kw_final<-str_replace(keywords$kw_final, "home gardens", "homegardens")
  keywords$kw_final<-str_replace(keywords$kw_final, "hop lias malabaricus", "hoplias malabaricus")
  keywords$kw_final<-str_replace(keywords$kw_final, "host  parasite interaction", "host-parasite interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "host parasite relationship", "host-parasite relationship")
  keywords$kw_final<-str_replace(keywords$kw_final, "host pathogen evolution", "host-pathogen evolution")
  keywords$kw_final<-str_replace(keywords$kw_final, "host responses", "host response")
  keywords$kw_final<-str_replace(keywords$kw_final, "host-parasite", "host-parasite")
  keywords$kw_final<-str_replace(keywords$kw_final, "hostfeeding", "host-feeding")
  keywords$kw_final<-str_replace(keywords$kw_final, "hostparasite", "host-parasite")
  keywords$kw_final<-str_replace(keywords$kw_final, "hostparasite system", "host-parasite system")
  keywords$kw_final<-str_replace(keywords$kw_final, "host-parasite system", "host-parasite system")
  keywords$kw_final<-str_replace(keywords$kw_final, "hot spot", "hotspots")
  keywords$kw_final<-str_replace(keywords$kw_final, "hot spots", "hotspots")
  keywords$kw_final<-str_replace(keywords$kw_final, "huisman olff-fresco models", "huisman-olff-fresco models")
  keywords$kw_final<-str_replace(keywords$kw_final, "human  elephant conflict", "human-elephant conflict")
  keywords$kw_final<-str_replace(keywords$kw_final, "human pressures", "human pressure")
  keywords$kw_final<-str_replace(keywords$kw_final, "human wildlife interactions", "human-wildlife interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "hyallela azteca", "hyalella azteca")
  keywords$kw_final<-str_replace(keywords$kw_final, "hybrid female sterility", "female hybrid sterility")
  keywords$kw_final<-str_replace(keywords$kw_final, "hybridiza tion", "hybridization")
  keywords$kw_final<-str_replace(keywords$kw_final, "hyper spectral remote sensing", "hyperspectral remote sensing")
  keywords$kw_final<-str_replace(keywords$kw_final, "i  mates", "i mates")
  keywords$kw_final<-str_replace(keywords$kw_final, "immuno competence handicap", "immunocompetence handicap")
  keywords$kw_final<-str_replace(keywords$kw_final, "immuno suppression", "immunosuppression")
  keywords$kw_final<-str_replace(keywords$kw_final, "immunoelectronmicroscopy", "immunoelectron microscopy")
  keywords$kw_final<-str_replace(keywords$kw_final, "income and capital breeding", "capital and income breeding")
  keywords$kw_final<-str_replace(keywords$kw_final, "increment growth", "growth increment")
  keywords$kw_final<-str_replace(keywords$kw_final, "index selection", "selection index")
  keywords$kw_final<-str_replace(keywords$kw_final, "indirect density mediated interactions", "density-mediated indirect interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "individual based sim-ulation", "individual-based simulation")
  keywords$kw_final<-str_replace(keywords$kw_final, "individual by-environment inter-action", "individual by environment interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "individualbased model", "individual-based model")
  keywords$kw_final<-str_replace(keywords$kw_final, "individualbased simulation", "individual-based simulation")
  keywords$kw_final<-str_replace(keywords$kw_final, "ingestion rates", "ingestion rate")
  keywords$kw_final<-str_replace(keywords$kw_final, "insect interactions", "insect interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "insect pathogens", "insect pathogen")
  keywords$kw_final<-str_replace(keywords$kw_final, "insect plant association", "insect-plant association")
  keywords$kw_final<-str_replace(keywords$kw_final, "insect plant associations", "insect-plant association")
  keywords$kw_final<-str_replace(keywords$kw_final, "insect-plant interaction", "plant-insect interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "insectplant interaction", "plant-insect interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "insect-plant interaction", "plant-insect interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "insect-plant relationships", "insect-plant relationships")
  keywords$kw_final<-str_replace(keywords$kw_final, "insular populations", "insular population")
  keywords$kw_final<-str_replace(keywords$kw_final, "inter annual variability", "interannual variability")
  keywords$kw_final<-str_replace(keywords$kw_final, "inter birth interval", "interbirth interval")
  keywords$kw_final<-str_replace(keywords$kw_final, "inter breeding", "interbreeding")
  keywords$kw_final<-str_replace(keywords$kw_final, "inter population hybridization", "interpopulation hybridization")
  keywords$kw_final<-str_replace(keywords$kw_final, "inter sexual selection", "intersexual selection")
  keywords$kw_final<-str_replace(keywords$kw_final, "inter species interactions", "interspecies interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "inter specific", "interspecific")
  keywords$kw_final<-str_replace(keywords$kw_final, "inter specific competition", "interspecific competition")
  keywords$kw_final<-str_replace(keywords$kw_final, "inter specific interactions", "interspecific interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "intera ctions", "interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "international union for conservation of nature  iucn", "iucn")
  keywords$kw_final<-str_replace(keywords$kw_final, "international union for conservation of nature iucn", "iucn")
  keywords$kw_final<-str_replace(keywords$kw_final, "intertidal snails", "intertidal snail")
  keywords$kw_final<-str_replace(keywords$kw_final, "intra guild interactions", "intraguild interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "intra individual variability", "intraindividual variability")
  keywords$kw_final<-str_replace(keywords$kw_final, "intra locus sexual conflict", "intralocus sexual conflict")
  keywords$kw_final<-str_replace(keywords$kw_final, "intra sexual competition", "intrasexual competition")
  keywords$kw_final<-str_replace(keywords$kw_final, "intra sexual dimorphism", "intrasexual dimorphism")
  keywords$kw_final<-str_replace(keywords$kw_final, "intra sexual selection", "intrasexual selection")
  keywords$kw_final<-str_replace(keywords$kw_final, "intra specific competition", "intraspecific competition")
  keywords$kw_final<-str_replace(keywords$kw_final, "intra specific facilitation", "intraspecific facilitation")
  keywords$kw_final<-str_replace(keywords$kw_final, "intra specific interaction", "intraspecific interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "intra specific trait variation", "intraspecific trait variation")
  keywords$kw_final<-str_replace(keywords$kw_final, "intra specific variation", "intraspecific variation")
  keywords$kw_final<-str_replace(keywords$kw_final, "intrinsic post zygotic isolation", "intrinsic postzygotic isolation")
  keywords$kw_final<-str_replace(keywords$kw_final, "invasive nonnative species", "nonnative invasive species")
  keywords$kw_final<-str_replace(keywords$kw_final, "isolation by-environment", "isolation-by-environment")
  keywords$kw_final<-str_replace(keywords$kw_final, "isolation bydistance", "isolation-by-distance")
  keywords$kw_final<-str_replace(keywords$kw_final, "isotope   n  15", "15n isotope")
  keywords$kw_final<-str_replace(keywords$kw_final, "isthmus of panamá", "isthmus of panama")
  keywords$kw_final<-str_replace(keywords$kw_final, "its 1", "its1")
  keywords$kw_final<-str_replace(keywords$kw_final, "itsrdna", "its rdna")
  keywords$kw_final<-str_replace(keywords$kw_final, "janzen-connell effect", "janzen-connell effect")
  keywords$kw_final<-str_replace(keywords$kw_final, "key resources", "key resource")
  keywords$kw_final<-str_replace(keywords$kw_final, "l ymnaea stagnalis", "lymnaea stagnalis")
  keywords$kw_final<-str_replace(keywords$kw_final, "la paz-bay", "la paz bay")
  keywords$kw_final<-str_replace(keywords$kw_final, "lady beetles", "ladybeetles")
  keywords$kw_final<-str_replace(keywords$kw_final, "lake gatun", "gatun lake")
  keywords$kw_final<-str_replace(keywords$kw_final, "lake land linkages", "lake-land linkages")
  keywords$kw_final<-str_replace(keywords$kw_final, "lake mývatn", "lake myvatn")
  keywords$kw_final<-str_replace(keywords$kw_final, "land use cover", "land-use cover")
  keywords$kw_final<-str_replace(keywords$kw_final, "land use-cover", "land-use cover")
  keywords$kw_final<-str_replace(keywords$kw_final, "larder hoarding", "larderhoarding")
  keywords$kw_final<-str_replace(keywords$kw_final, "large scale disturbances", "large-scale disturbance")
  keywords$kw_final<-str_replace(keywords$kw_final, "late wood", "latewood")
  keywords$kw_final<-str_replace(keywords$kw_final, "leaf ", "leaf")
  keywords$kw_final<-str_replace(keywords$kw_final, "leaf cutter ant", "leafcutter ant")
  keywords$kw_final<-str_replace(keywords$kw_final, "levins metapopulations", "levins metapopulation")
  keywords$kw_final<-str_replace(keywords$kw_final, "levinsb", "levins b")
  keywords$kw_final<-str_replace(keywords$kw_final, "liana tree competition", "liana-tree competition")
  keywords$kw_final<-str_replace(keywords$kw_final, "liana tree interaction", "liana-tree interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "life history switch point", "life-history switch point")
  keywords$kw_final<-str_replace(keywords$kw_final, "life table-response experiments", "life-table response experiments")
  keywords$kw_final<-str_replace(keywords$kw_final, "lifehistory", "life-history")
  keywords$kw_final<-str_replace(keywords$kw_final, "lifehistory covariation", "life-history covariation")
  keywords$kw_final<-str_replace(keywords$kw_final, "lifehistory switch point", "life-history switch point")
  keywords$kw_final<-str_replace(keywords$kw_final, "lifehistory trade offs", "life-history tradeoffs")
  keywords$kw_final<-str_replace(keywords$kw_final, "lifehistory traits", "life-history traits")
  keywords$kw_final<-str_replace(keywords$kw_final, "light : nutrient hypothesis", "light:nutrient hypothesis")
  keywords$kw_final<-str_replace(keywords$kw_final, "light responses curve", "light curve responses")
  keywords$kw_final<-str_replace(keywords$kw_final, "likelihoodratio test", "likelihood ratio test")
  keywords$kw_final<-str_replace(keywords$kw_final, "linearmodels", "linear models")
  keywords$kw_final<-str_replace(keywords$kw_final, "lock and key", "lock-and-key")
  keywords$kw_final<-str_replace(keywords$kw_final, "logseries", "log series")
  keywords$kw_final<-str_replace(keywords$kw_final, "long term ecological research site", "lter")
  keywords$kw_final<-str_replace(keywords$kw_final, "long term ecological research sites", "lter")
  keywords$kw_final<-str_replace(keywords$kw_final, "longdistance migration", "long-distance migration")
  keywords$kw_final<-str_replace(keywords$kw_final, "longterm data", "long-term data")
  keywords$kw_final<-str_replace(keywords$kw_final, "longterm monitoring", "long-term monitoring")
  keywords$kw_final<-str_replace(keywords$kw_final, "loss of-function", "loss of function")
  keywords$kw_final<-str_replace(keywords$kw_final, "lotka volterra competition model", "lotka-volterra competition model")
  keywords$kw_final<-str_replace(keywords$kw_final, "lotka-volterra", "lotka-volterra")
  keywords$kw_final<-str_replace(keywords$kw_final, "loxodonta africana africana", "loxodonta africana")
  keywords$kw_final<-str_replace(keywords$kw_final, "lupïnus arboreus", "lupinus arboreus")
  keywords$kw_final<-str_replace(keywords$kw_final, "luscinia luscinia", "luscinia")
  keywords$kw_final<-str_replace(keywords$kw_final, "lyman tria dispar l", "lymantria dispar")
  keywords$kw_final<-str_replace(keywords$kw_final, "lymantria dispar l", "lymantria dispar")
  keywords$kw_final<-str_replace(keywords$kw_final, "lynx lynx canadensis", "lynx canadensis")
  keywords$kw_final<-str_replace(keywords$kw_final, "lévy flights", "levy flights")
  keywords$kw_final<-str_replace(keywords$kw_final, "ma crocystis pyrifera", "macrocystis pyrifera")
  keywords$kw_final<-str_replace(keywords$kw_final, "macro ecology", "macroecology")
  keywords$kw_final<-str_replace(keywords$kw_final, "macro invertebrate", "macroinvertebrate")
  keywords$kw_final<-str_replace(keywords$kw_final, "macro invertebrates", "macroinvertebrates")
  keywords$kw_final<-str_replace(keywords$kw_final, "macro molluscs", "macromolluscs")
  keywords$kw_final<-str_replace(keywords$kw_final, "macro mutation", "macromutation")
  keywords$kw_final<-str_replace(keywords$kw_final, "mahogany shoot borer", "mahogany shootborer")
  keywords$kw_final<-str_replace(keywords$kw_final, "male contest competition", "male-male competition")
  keywords$kw_final<-str_replace(keywords$kw_final, "male fitness gain curves", "male fitness gain curve")
  keywords$kw_final<-str_replace(keywords$kw_final, "male male contest competition", "male-male competition")
  keywords$kw_final<-str_replace(keywords$kw_final, "manacus manacus", "manacus")
  keywords$kw_final<-str_replace(keywords$kw_final, "manage ment", "management")
  keywords$kw_final<-str_replace(keywords$kw_final, "management conservation", "conservation management")
  keywords$kw_final<-str_replace(keywords$kw_final, "marine protected área", "marine protected area")
  keywords$kw_final<-str_replace(keywords$kw_final, "mark  recapture", "mark-recapture")
  keywords$kw_final<-str_replace(keywords$kw_final, "mark recapture data", "mark-recapture data")
  keywords$kw_final<-str_replace(keywords$kw_final, "mark-recapture", "mark-recapture")
  keywords$kw_final<-str_replace(keywords$kw_final, "mark-recapture model", "capture mark-recapture model")
  keywords$kw_final<-str_replace(keywords$kw_final, "mark-release-recapture", "mark-release-recapture")
  keywords$kw_final<-str_replace(keywords$kw_final, "mass abundance scaling", "mass-abundance scaling")
  keywords$kw_final<-str_replace(keywords$kw_final, "match-mismatch", "match-mismatch")
  keywords$kw_final<-str_replace(keywords$kw_final, "match-mismatch", "match-mismatch")
  keywords$kw_final<-str_replace(keywords$kw_final, "mating system transitions", "mating system transition")
  keywords$kw_final<-str_replace(keywords$kw_final, "matrix correlation", "correlation matrix")
  keywords$kw_final<-str_replace(keywords$kw_final, "matrix projection", "projection matrix")
  keywords$kw_final<-str_replace(keywords$kw_final, "maximum likelihood estimates", "maximum likelihood estimate")
  keywords$kw_final<-str_replace(keywords$kw_final, "maërl beds", "maerl beds")
  keywords$kw_final<-str_replace(keywords$kw_final, "mean variance relationship", "mean-variance relationship")
  keywords$kw_final<-str_replace(keywords$kw_final, "mean variance scaling", "mean-variance scaling")
  keywords$kw_final<-str_replace(keywords$kw_final, "mega herbivore", "megaherbivore")
  keywords$kw_final<-str_replace(keywords$kw_final, "mega herbivores", "megaherbivores")
  keywords$kw_final<-str_replace(keywords$kw_final, "melanargia galathea l", "melanargia galathea")
  keywords$kw_final<-str_replace(keywords$kw_final, "melanoplus femurrubrum femurrubrum", "melanoplus femurrubrum")
  keywords$kw_final<-str_replace(keywords$kw_final, "meso herbivore", "mesoherbivore")
  keywords$kw_final<-str_replace(keywords$kw_final, "meso herbivores", "mesoherbivore")
  keywords$kw_final<-str_replace(keywords$kw_final, "meta frontier", "metafrontier")
  keywords$kw_final<-str_replace(keywords$kw_final, "meta plasticity", "metaplasticity")
  keywords$kw_final<-str_replace(keywords$kw_final, "meta population ecology", "metapopulation ecology")
  keywords$kw_final<-str_replace(keywords$kw_final, "metapopulation dynamic", "metapopulation dynamics")
  keywords$kw_final<-str_replace(keywords$kw_final, "metopeurum fuscoviride", "metopeurum fusco viride")
  keywords$kw_final<-str_replace(keywords$kw_final, "michoacán", "michoacan")
  keywords$kw_final<-str_replace(keywords$kw_final, "micro climate", "microclimate")
  keywords$kw_final<-str_replace(keywords$kw_final, "micro ecosystem", "microecosystem")
  keywords$kw_final<-str_replace(keywords$kw_final, "micro environment", "microenvironment")
  keywords$kw_final<-str_replace(keywords$kw_final, "micro environmental conditions", "microenvironmental conditions")
  keywords$kw_final<-str_replace(keywords$kw_final, "micro environmental variation", "microenvironmental variation")
  keywords$kw_final<-str_replace(keywords$kw_final, "micro habitat", "microhabitat")
  keywords$kw_final<-str_replace(keywords$kw_final, "micro satellites", "microsatellites")
  keywords$kw_final<-str_replace(keywords$kw_final, "micro topography", "microtopography")
  keywords$kw_final<-str_replace(keywords$kw_final, "microbe host interactions", "host-microbe interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "microbial n biomass", "microbial biomass n")
  keywords$kw_final<-str_replace(keywords$kw_final, "microscopy electronic", "electronic microscopy")
  keywords$kw_final<-str_replace(keywords$kw_final, "microsporid ia", "microsporidia")
  keywords$kw_final<-str_replace(keywords$kw_final, "mineralization: nitrogen", "nitrogen mineralization")
  keywords$kw_final<-str_replace(keywords$kw_final, "mini barcode", "minibarcode")
  keywords$kw_final<-str_replace(keywords$kw_final, "mini rhizotrons", "minirhizotrons")
  keywords$kw_final<-str_replace(keywords$kw_final, "mito nuclear discordance", "mitonuclear discordance")
  keywords$kw_final<-str_replace(keywords$kw_final, "mixed linear model", "linear mixed model")
  keywords$kw_final<-str_replace(keywords$kw_final, "mixed model effects", "mixed effects model")
  keywords$kw_final<-str_replace(keywords$kw_final, "mixed species stands", "mixed species stand")
  keywords$kw_final<-str_replace(keywords$kw_final, "mixedgrass prairie", "mixed-grass prairie")
  keywords$kw_final<-str_replace(keywords$kw_final, "mixedwoods", "mixed woods")
  keywords$kw_final<-str_replace(keywords$kw_final, "mixing litter", "litter mixing")
  keywords$kw_final<-str_replace(keywords$kw_final, "mo nogamy", "monogamy")
  keywords$kw_final<-str_replace(keywords$kw_final, "model aggregation", "aggregation model")
  keywords$kw_final<-str_replace(keywords$kw_final, "model data comparison", "model-data comparison")
  keywords$kw_final<-str_replace(keywords$kw_final, "model population", "population model")
  keywords$kw_final<-str_replace(keywords$kw_final, "modeling distribution", "distribution modeling")
  keywords$kw_final<-str_replace(keywords$kw_final, "modeling population dynamics", "population dynamics modeling")
  keywords$kw_final<-str_replace(keywords$kw_final, "mono culture plantation", "monoculture plantation")
  keywords$kw_final<-str_replace(keywords$kw_final, "monocot ", "monocot")
  keywords$kw_final<-str_replace(keywords$kw_final, "monsoons", "monsoon")
  keywords$kw_final<-str_replace(keywords$kw_final, "mosaic landscape", "landscape mosaic")
  keywords$kw_final<-str_replace(keywords$kw_final, "mosquitos", "mosquito")
  keywords$kw_final<-str_replace(keywords$kw_final, "most productive institutions and authors", "most productive authors and institutions")
  keywords$kw_final<-str_replace(keywords$kw_final, "multi annual cycles", "multiannual cycles")
  keywords$kw_final<-str_replace(keywords$kw_final, "multi dimensional scaling", "multidimensional scaling")
  keywords$kw_final<-str_replace(keywords$kw_final, "multi element analysis", "multielement analysis")
  keywords$kw_final<-str_replace(keywords$kw_final, "multi functionality", "multifunctionality")
  keywords$kw_final<-str_replace(keywords$kw_final, "multi host system", "multihost system")
  keywords$kw_final<-str_replace(keywords$kw_final, "multi locus", "multilocus")
  keywords$kw_final<-str_replace(keywords$kw_final, "multi locus heterozygosity", "multilocus heterozygosity")
  keywords$kw_final<-str_replace(keywords$kw_final, "multi modal signaling", "multimodal signaling")
  keywords$kw_final<-str_replace(keywords$kw_final, "multi modal signals", "multimodal signal")
  keywords$kw_final<-str_replace(keywords$kw_final, "multi predator environments", "multipredator environments")
  keywords$kw_final<-str_replace(keywords$kw_final, "multi scale", "multiscale")
  keywords$kw_final<-str_replace(keywords$kw_final, "multi scale analysis", "multiscale analysis")
  keywords$kw_final<-str_replace(keywords$kw_final, "multi scaled random walk", "multiscaled random walk")
  keywords$kw_final<-str_replace(keywords$kw_final, "multi species communities", "multispecies communities")
  keywords$kw_final<-str_replace(keywords$kw_final, "multi species interaction", "multispecies interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "multi state capture-recapture models", "multistate capture recapture models")
  keywords$kw_final<-str_replace(keywords$kw_final, "multi state model", "multistate model")
  keywords$kw_final<-str_replace(keywords$kw_final, "multi stemmed trees", "multistemmed trees")
  keywords$kw_final<-str_replace(keywords$kw_final, "multi strata models", "multistrata models")
  keywords$kw_final<-str_replace(keywords$kw_final, "multi trophic communities", "multitrophic communities")
  keywords$kw_final<-str_replace(keywords$kw_final, "multi trophic interaction", "multitrophic interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "multi trophic models", "multitrophic models")
  keywords$kw_final<-str_replace(keywords$kw_final, "multi trophic networks", "multitrophic networks")
  keywords$kw_final<-str_replace(keywords$kw_final, "multiple stable state", "multiple stable states")
  keywords$kw_final<-str_replace(keywords$kw_final, "murray darling basin", "murray-darling basin")
  keywords$kw_final<-str_replace(keywords$kw_final, "mustela nivalis nivalis", "mustela nivalis")
  keywords$kw_final<-str_replace(keywords$kw_final, "mut ualism", "mutualism")
  keywords$kw_final<-str_replace(keywords$kw_final, "mutational melt down", "mutational meltdown")
  keywords$kw_final<-str_replace(keywords$kw_final, "mutual isms", "mutualisms")
  keywords$kw_final<-str_replace(keywords$kw_final, "myotis myotis", "myotis")
  keywords$kw_final<-str_replace(keywords$kw_final, "myrmechocory", "myrmecochory")
  keywords$kw_final<-str_replace(keywords$kw_final, "n : p ratio", "n:p ratio")
  keywords$kw_final<-str_replace(keywords$kw_final, "n and p co limitation", "n:p colimitation")
  keywords$kw_final<-str_replace(keywords$kw_final, "n and p colimitation", "n:p colimitation")
  keywords$kw_final<-str_replace(keywords$kw_final, "n p ratios", "n:p ratios")
  keywords$kw_final<-str_replace(keywords$kw_final, "n: p ratios", "n:p ratios")
  keywords$kw_final<-str_replace(keywords$kw_final, "nasua nasua", "nasua")
  keywords$kw_final<-str_replace(keywords$kw_final, "native vs introduced species", "introduced vs native species")
  keywords$kw_final<-str_replace(keywords$kw_final, "natural abundance δ n 15", "natural abundance δ n15")
  keywords$kw_final<-str_replace(keywords$kw_final, "nature reserves", "nature reserve")
  keywords$kw_final<-str_replace(keywords$kw_final, "nearest neighbor distances", "nearest neighbor distance")
  keywords$kw_final<-str_replace(keywords$kw_final, "nectar spurs", "nectar spur")
  keywords$kw_final<-str_replace(keywords$kw_final, "neo endemism", "neoendemism")
  keywords$kw_final<-str_replace(keywords$kw_final, "neo tropics", "neotropics")
  keywords$kw_final<-str_replace(keywords$kw_final, "nest predation rate", "nest-predation rate")
  keywords$kw_final<-str_replace(keywords$kw_final, "nest predation rates", "nest-predation rate")
  keywords$kw_final<-str_replace(keywords$kw_final, "nest sites", "nest site")
  keywords$kw_final<-str_replace(keywords$kw_final, "nesting sites", "nesting site")
  keywords$kw_final<-str_replace(keywords$kw_final, "nh   immobilization 4 +", "nh4+ immobilization")
  keywords$kw_final<-str_replace(keywords$kw_final, "nh  immobilization 4+", "nh4+ immobilization")
  keywords$kw_final<-str_replace(keywords$kw_final, "niche neutrality continuum", "niche-neutrality continuum")
  keywords$kw_final<-str_replace(keywords$kw_final, "niche-neutrality continuum", "niche-neutrality continuum")
  keywords$kw_final<-str_replace(keywords$kw_final, "night time transpiration", "night-time transpiration")
  keywords$kw_final<-str_replace(keywords$kw_final, "nighttime transpiration", "night-time transpiration")
  keywords$kw_final<-str_replace(keywords$kw_final, "nitrogen and light availability", "light and nitrogen availability")
  keywords$kw_final<-str_replace(keywords$kw_final, "nitrogen to phosphorus ratio", "n:p ratio")
  keywords$kw_final<-str_replace(keywords$kw_final, "nitrogen to-phosphorus ratio", "n:p ratio")
  keywords$kw_final<-str_replace(keywords$kw_final, "no  immobilization 3 ", "no3 immobilization")
  keywords$kw_final<-str_replace(keywords$kw_final, "no  immobilization 3  ", "no3 immobilization")
  keywords$kw_final<-str_replace(keywords$kw_final, "no 2", "no2")
  keywords$kw_final<-str_replace(keywords$kw_final, "non additive selection", "nonadditive selection")
  keywords$kw_final<-str_replace(keywords$kw_final, "non additivity", "nonadditivity")
  keywords$kw_final<-str_replace(keywords$kw_final, "non breeders", "nonbreeders")
  keywords$kw_final<-str_replace(keywords$kw_final, "non breeding", "nonbreeding")
  keywords$kw_final<-str_replace(keywords$kw_final, "non consumptive interactions", "nonconsumptive interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "non destructive method", "nondestructive method")
  keywords$kw_final<-str_replace(keywords$kw_final, "non equilibrium dynamics", "nonequilibrium dynamics")
  keywords$kw_final<-str_replace(keywords$kw_final, "non human primates", "nonhuman primates")
  keywords$kw_final<-str_replace(keywords$kw_final, "non independence", "nonindependence")
  keywords$kw_final<-str_replace(keywords$kw_final, "non invasive monitoring", "noninvasive monitoring")
  keywords$kw_final<-str_replace(keywords$kw_final, "non linear models", "nonlinear models")
  keywords$kw_final<-str_replace(keywords$kw_final, "non linearity", "nonlinearity")
  keywords$kw_final<-str_replace(keywords$kw_final, "non metric multi-dimensional scaling", "nmds")
  keywords$kw_final<-str_replace(keywords$kw_final, "non native plant", "nonnative plant")
  keywords$kw_final<-str_replace(keywords$kw_final, "non parametric covariance function", "nonparametric covariance function")
  keywords$kw_final<-str_replace(keywords$kw_final, "non parametric estimators", "nonparametric estimators")
  keywords$kw_final<-str_replace(keywords$kw_final, "non pollinating fig wasps", "nonpollinating fig wasps")
  keywords$kw_final<-str_replace(keywords$kw_final, "north eastern north america", "eastern north america")
  keywords$kw_final<-str_replace(keywords$kw_final, "north west argentina", "northwest argentina")
  keywords$kw_final<-str_replace(keywords$kw_final, "north west himalaya", "northwest himalaya")
  keywords$kw_final<-str_replace(keywords$kw_final, "north west territories", "northwest territories")
  keywords$kw_final<-str_replace(keywords$kw_final, "north western europe", "northwestern europe")
  keywords$kw_final<-str_replace(keywords$kw_final, "north western mediterranean", "northwestern mediterranean")
  keywords$kw_final<-str_replace(keywords$kw_final, "northern québec", "northern quebec")
  keywords$kw_final<-str_replace(keywords$kw_final, "nuclear cytoplasmic interaction", "cytoplasmic nuclear interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "nullmodel", "null model")
  keywords$kw_final<-str_replace(keywords$kw_final, "numerical and functional responses", "functional and numerical responses")
  keywords$kw_final<-str_replace(keywords$kw_final, "nutr ient dynamics", "nutrient dynamics")
  keywords$kw_final<-str_replace(keywords$kw_final, "nutrient  allelochemical interactions", "nutrient allelochemical interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "nutrient hotspots", "nutrient hotspot")
  keywords$kw_final<-str_replace(keywords$kw_final, "nutrient phytoplankton-zooplankton npz model", "nutrient-phytoplankton-zooplankton model")
  keywords$kw_final<-str_replace(keywords$kw_final, "nutrientphytoplankton zooplankton npz model", "nutrient-phytoplankton-zooplankton model")
  keywords$kw_final<-str_replace(keywords$kw_final, "n : p ratio", "n:p ratio")
  keywords$kw_final<-str_replace(keywords$kw_final, "o rthoptera", "orthoptera")
  keywords$kw_final<-str_replace(keywords$kw_final, "o u process", "ou process")
  keywords$kw_final<-str_replace(keywords$kw_final, "oak quercus", "quercus oak")
  keywords$kw_final<-str_replace(keywords$kw_final, "occupancy abundance relationship", "occupancy-abundance relationship")
  keywords$kw_final<-str_replace(keywords$kw_final, "ol faction", "olfaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "oldgrowth forest", "old-growth forest")
  keywords$kw_final<-str_replace(keywords$kw_final, "ontogenetic diet shifts", "ontogenetic diet shift")
  keywords$kw_final<-str_replace(keywords$kw_final, "ornament evolution", "evolution ornament")
  keywords$kw_final<-str_replace(keywords$kw_final, "osa península", "osa peninsula")
  keywords$kw_final<-str_replace(keywords$kw_final, "outbreak pest", "pest outbreak")
  keywords$kw_final<-str_replace(keywords$kw_final, "over dominance", "overdominance")
  keywords$kw_final<-str_replace(keywords$kw_final, "over fishing", "overfishing")
  keywords$kw_final<-str_replace(keywords$kw_final, "over fitting", "overfitting")
  keywords$kw_final<-str_replace(keywords$kw_final, "over produced esterases", "overproduced esterase")
  keywords$kw_final<-str_replace(keywords$kw_final, "over winter survival", "overwinter survival")
  keywords$kw_final<-str_replace(keywords$kw_final, "over wintering", "overwintering")
  keywords$kw_final<-str_replace(keywords$kw_final, "overproduced esterases", "overproduced esterase")
  keywords$kw_final<-str_replace(keywords$kw_final, "oyster catcher", "oystercatcher")
  keywords$kw_final<-str_replace(keywords$kw_final, "p  st", "pst")
  keywords$kw_final<-str_replace(keywords$kw_final, "pacific panamá", "pacific panama")
  keywords$kw_final<-str_replace(keywords$kw_final, "paircorrelation function", "pair-correlation function")
  keywords$kw_final<-str_replace(keywords$kw_final, "paleo ecology", "paleoecology")
  keywords$kw_final<-str_replace(keywords$kw_final, "pamana", "panama")
  keywords$kw_final<-str_replace(keywords$kw_final, "parasite host ecology", "host-parasite ecology")
  keywords$kw_final<-str_replace(keywords$kw_final, "parasite host interaction", "host-parasite interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "parasite host relationship", "host-parasite relationship")
  keywords$kw_final<-str_replace(keywords$kw_final, "parasite parasite interactions", "parasite-parasite interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "parasitoid host dynamics", "host-parasitoid dynamics")
  keywords$kw_final<-str_replace(keywords$kw_final, "parasitoid host interaction", "host-parasitoid interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "paren tal care", "parental care")
  keywords$kw_final<-str_replace(keywords$kw_final, "partitioning habitat", "habitat partitioning")
  keywords$kw_final<-str_replace(keywords$kw_final, "pathogen host interactions", "host-pathogen interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "peatbog", "peat bog")
  keywords$kw_final<-str_replace(keywords$kw_final, "pennaeus vannamei", "penaeus vannamei")
  keywords$kw_final<-str_replace(keywords$kw_final, "perennial bunch grass", "perennial bunchgrass")
  keywords$kw_final<-str_replace(keywords$kw_final, "perennial native grasses", "native perennial grasses")
  keywords$kw_final<-str_replace(keywords$kw_final, "performance trait", "trait performance")
  keywords$kw_final<-str_replace(keywords$kw_final, "phaseolus lunatus l", "phaseolus lunatus")
  keywords$kw_final<-str_replace(keywords$kw_final, "phenotypicselection analysis", "phenotypic selection analysis")
  keywords$kw_final<-str_replace(keywords$kw_final, "phosp horus", "phosphorus")
  keywords$kw_final<-str_replace(keywords$kw_final, "photosyn thesis", "photosynthesis")
  keywords$kw_final<-str_replace(keywords$kw_final, "phyloge nomics", "phylogenomics")
  keywords$kw_final<-str_replace(keywords$kw_final, "phylogenetic bayesian multilevel models", "bayesian phylogenetic multilevel models")
  keywords$kw_final<-str_replace(keywords$kw_final, "physico chemical", "physicochemical")
  keywords$kw_final<-str_replace(keywords$kw_final, "physico chemical factors", "physicochemical factors")
  keywords$kw_final<-str_replace(keywords$kw_final, "physico chemical parameters", "physicochemical parameters")
  keywords$kw_final<-str_replace(keywords$kw_final, "phyto plankton", "phytoplankton")
  keywords$kw_final<-str_replace(keywords$kw_final, "phytop lankton", "phytoplankton")
  keywords$kw_final<-str_replace(keywords$kw_final, "pinus ponderosa ponderosa", "pinus ponderosa")
  keywords$kw_final<-str_replace(keywords$kw_final, "pinus sp", "pinus")
  keywords$kw_final<-str_replace(keywords$kw_final, "pinussp", "pinus")
  keywords$kw_final<-str_replace(keywords$kw_final, "pit fall traps", "pitfall traps")
  keywords$kw_final<-str_replace(keywords$kw_final, "piñon", "pinon")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant  animal interactions", "plant-animal interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant  insect interaction", "insect-plant interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant anim al interactions", "plant-animal interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant environment interactions", "plant-environment interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant fungi interactions", "plant-fungi interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant fungus interactions", "plant-fungus interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant herbivore dynamics", "plant-herbivore dynamics")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant indirect defense", "indirect plant defense")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant induced defense", "induced plant defense")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant induced defenses", "induced plant defense")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant insect association", "insect-plant association")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant insect-pathogen interactions", "plant-insect-pathogen interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant interference", "plant-plant interference")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant invasive", "invasive plant")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant mineral nutrition", "mineral plant nutrition")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant plant interference", "plant-plant interference")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant resources", "plant resource")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant soil biota interactions", "plant-soil-biota interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant soil microbe interactions", "plant soil-microbe interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant soil system", "plant-soil system")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant soil-biota interactions", "plant-soil-biota interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant-herbivore interactions", "plant-herbivore interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "plantanimal interactions", "plant-animal interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "plantation forest", "forest plantation")
  keywords$kw_final<-str_replace(keywords$kw_final, "plantinsect interactions", "plant-insect interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "plantsoil interactions", "plant-soil interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant-insect-pathogen interactions", "plant-insect-pathogen interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant-pathogen-insect interactions", "plant-insect-pathogen interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "plant-pollinator network", "plant-pollinator network")
  keywords$kw_final<-str_replace(keywords$kw_final, "pleist ocene", "pleistocene")
  keywords$kw_final<-str_replace(keywords$kw_final, "poisson log normal", "poisson lognormal")
  keywords$kw_final<-str_replace(keywords$kw_final, "policy making", "policymaking")
  keywords$kw_final<-str_replace(keywords$kw_final, "pollen : ovule ratio", "pollen:ovule ratio")
  keywords$kw_final<-str_replace(keywords$kw_final, "pollen carry over", "pollen carryover")
  keywords$kw_final<-str_replace(keywords$kw_final, "pollen-ovule ratio", "pollen:ovule ratio")
  keywords$kw_final<-str_replace(keywords$kw_final, "pollinating fig wasp", "fig pollinating wasp")
  keywords$kw_final<-str_replace(keywords$kw_final, "pollinator plant interactions", "plant-pollinator interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "poly morphism", "polymorphism")
  keywords$kw_final<-str_replace(keywords$kw_final, "pond breeding amphibian", "pondbreeding amphibian")
  keywords$kw_final<-str_replace(keywords$kw_final, "population estimates", "population estimate")
  keywords$kw_final<-str_replace(keywords$kw_final, "population matrix model", "matrix population model")
  keywords$kw_final<-str_replace(keywords$kw_final, "population matrix models", "matrix population models")
  keywords$kw_final<-str_replace(keywords$kw_final, "population responses", "population response")
  keywords$kw_final<-str_replace(keywords$kw_final, "population s", "populations")
  keywords$kw_final<-str_replace(keywords$kw_final, "population size structures", "population size structure")
  keywords$kw_final<-str_replace(keywords$kw_final, "porcellionides p rui nos us", "porcellionides pruinosus")
  keywords$kw_final<-str_replace(keywords$kw_final, "porites porites", "porites")
  keywords$kw_final<-str_replace(keywords$kw_final, "positive plant plant interactions", "positive plant interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "post dispersal", "postdispersal")
  keywords$kw_final<-str_replace(keywords$kw_final, "post dispersal mortality", "postdispersal mortality")
  keywords$kw_final<-str_replace(keywords$kw_final, "post fire recovery", "postfire recovery")
  keywords$kw_final<-str_replace(keywords$kw_final, "post fire regeneration", "postfire regeneration")
  keywords$kw_final<-str_replace(keywords$kw_final, "post fire resprouting", "postfire resprouting")
  keywords$kw_final<-str_replace(keywords$kw_final, "post glacial dispersal", "postglacial dispersal")
  keywords$kw_final<-str_replace(keywords$kw_final, "post glacial expansion", "postglacial expansion")
  keywords$kw_final<-str_replace(keywords$kw_final, "post glacial range expansion", "postglacial range expansion")
  keywords$kw_final<-str_replace(keywords$kw_final, "post glacial recolonization", "postglacial recolonization")
  keywords$kw_final<-str_replace(keywords$kw_final, "post ingestive feedback", "postingestive feedback")
  keywords$kw_final<-str_replace(keywords$kw_final, "post mating", "post-mating")
  keywords$kw_final<-str_replace(keywords$kw_final, "post mating female response", "post-mating female response")
  keywords$kw_final<-str_replace(keywords$kw_final, "post mating prezygotic barriers", "postmating prezygotic barriers")
  keywords$kw_final<-str_replace(keywords$kw_final, "postcopulatory", "post-copulatory")
  keywords$kw_final<-str_replace(keywords$kw_final, "potential evapo transpiration", "potential evapotranspiration")
  keywords$kw_final<-str_replace(keywords$kw_final, "power lines", "powerlines")
  keywords$kw_final<-str_replace(keywords$kw_final, "powerlaw scaling", "power law scaling")
  keywords$kw_final<-str_replace(keywords$kw_final, "pre dispersal seed predators", "predispersal seed predator")
  keywords$kw_final<-str_replace(keywords$kw_final, "pre dispersal seed-predator", "predispersal seed predator")
  keywords$kw_final<-str_replace(keywords$kw_final, "pre emptive competition", "preemptive competition")
  keywords$kw_final<-str_replace(keywords$kw_final, "pre montane", "premontane")
  keywords$kw_final<-str_replace(keywords$kw_final, "pre zygotic", "prezygotic")
  keywords$kw_final<-str_replace(keywords$kw_final, "precipitation extreme", "extreme precipitation")
  keywords$kw_final<-str_replace(keywords$kw_final, "preda tor", "predator")
  keywords$kw_final<-str_replace(keywords$kw_final, "predati on", "predation")
  keywords$kw_final<-str_replace(keywords$kw_final, "predator  prey dynamics", "predator-prey dynamics")
  keywords$kw_final<-str_replace(keywords$kw_final, "predator : prey ratio", "predator:prey ratio")
  keywords$kw_final<-str_replace(keywords$kw_final, "predator non consumptive effects", "predator nonconsumptive effects")
  keywords$kw_final<-str_replace(keywords$kw_final, "predator prey ecology", "predator-prey ecology")
  keywords$kw_final<-str_replace(keywords$kw_final, "predatorprey", "predator-prey")
  keywords$kw_final<-str_replace(keywords$kw_final, "predispersal seed predators", "predispersal seed predator")
  keywords$kw_final<-str_replace(keywords$kw_final, "preferred speeds", "preferred speed")
  keywords$kw_final<-str_replace(keywords$kw_final, "premontane moist forest", "moist premontane forest")
  keywords$kw_final<-str_replace(keywords$kw_final, "presence absence map", "presence-absence map")
  keywords$kw_final<-str_replace(keywords$kw_final, "presence only model", "presence-only model")
  keywords$kw_final<-str_replace(keywords$kw_final, "presence-absence map", "presence-absence map")
  keywords$kw_final<-str_replace(keywords$kw_final, "presenceonly model", "presence-only model")
  keywords$kw_final<-str_replace(keywords$kw_final, "prey predator dynamics", "predator-prey dynamics")
  keywords$kw_final<-str_replace(keywords$kw_final, "prey predator interaction", "predator-prey interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "prey predator ratio", "predator:prey ratio")
  keywords$kw_final<-str_replace(keywords$kw_final, "prey predator system", "prey-predator system")
  keywords$kw_final<-str_replace(keywords$kw_final, "prey spider", "spider prey")
  keywords$kw_final<-str_replace(keywords$kw_final, "pro ovigeny", "proovigeny")
  keywords$kw_final<-str_replace(keywords$kw_final, "procrustes distances", "procrustes distance")
  keywords$kw_final<-str_replace(keywords$kw_final, "produ ctivity", "productivity")
  keywords$kw_final<-str_replace(keywords$kw_final, "productivity diversity", "productivity-diversity")
  keywords$kw_final<-str_replace(keywords$kw_final, "pseudo autosomal region", "pseudoautosomal region")
  keywords$kw_final<-str_replace(keywords$kw_final, "pseudo replication", "pseudoreplication")
  keywords$kw_final<-str_replace(keywords$kw_final, "q  -f st st", "q-f st")
  keywords$kw_final<-str_replace(keywords$kw_final, "quality signal", "signal quality")
  keywords$kw_final<-str_replace(keywords$kw_final, "quasi cycles", "quasicycles")
  keywords$kw_final<-str_replace(keywords$kw_final, "quasi species", "quasispecies")
  keywords$kw_final<-str_replace(keywords$kw_final, "r  0", "r0")
  keywords$kw_final<-str_replace(keywords$kw_final, "r -k-selection", "r-k selection")
  keywords$kw_final<-str_replace(keywords$kw_final, "r h whittaker", "rh whittaker")
  # keywords$kw_final<-str_replace(keywords$kw_final, "r* theory*", "r* theory")
  keywords$kw_final<-str_replace(keywords$kw_final, "r-k selection", "r-k selection")
  keywords$kw_final<-str_replace(keywords$kw_final, "r: fr", "red:far red ratio")
  keywords$kw_final<-str_replace(keywords$kw_final, "rain fall", "rainfall")
  keywords$kw_final<-str_replace(keywords$kw_final, "random ampliÔ¨Åed polymorphic dna", "rapd")
  keywords$kw_final<-str_replace(keywords$kw_final, "range restricted species", "restricted range species")
  keywords$kw_final<-str_replace(keywords$kw_final, "rare vs common species", "common vs rare species")
  keywords$kw_final<-str_replace(keywords$kw_final, "razor fish", "razorfish")
  keywords$kw_final<-str_replace(keywords$kw_final, "re burn", "reburn")
  keywords$kw_final<-str_replace(keywords$kw_final, "re fuge", "refuge")
  keywords$kw_final<-str_replace(keywords$kw_final, "re introduction", "reintroduction")
  keywords$kw_final<-str_replace(keywords$kw_final, "re mating", "remating")
  keywords$kw_final<-str_replace(keywords$kw_final, "re productive suppression", "reproductive suppression")
  keywords$kw_final<-str_replace(keywords$kw_final, "reaction  diffusion", "reaction-diffusion")
  keywords$kw_final<-str_replace(keywords$kw_final, "recessive alleles", "recessive allele")
  keywords$kw_final<-str_replace(keywords$kw_final, "red : far red ratio", "red:far red ratio")
  keywords$kw_final<-str_replace(keywords$kw_final, "red far red ratio", "red:far red ratio")
  keywords$kw_final<-str_replace(keywords$kw_final, "red fox  vulpes vulpes ", "red fox, vulpes vulpes ")
  keywords$kw_final<-str_replace(keywords$kw_final, "red fox vulpes vulpes", "red fox, vulpes vulpes ")
  keywords$kw_final<-str_replace(keywords$kw_final, "red jungle fowl", "red junglefowl")
  keywords$kw_final<-str_replace(keywords$kw_final, "red-far red ratio", "red:far red ratio")
  keywords$kw_final<-str_replace(keywords$kw_final, "red: far red ratio", "red:far red ratio")
  keywords$kw_final<-str_replace(keywords$kw_final, "reduction oxidation potential", "oxidation reduction potential")
  keywords$kw_final<-str_replace(keywords$kw_final, "reef coral", "coral reef")
  keywords$kw_final<-str_replace(keywords$kw_final, "regional vs local dynamics", "local vs regional dynamics")
  keywords$kw_final<-str_replace(keywords$kw_final, "regression trees", "regression tree")
  keywords$kw_final<-str_replace(keywords$kw_final, "relative species abundances", "relative species abundance")
  keywords$kw_final<-str_replace(keywords$kw_final, "remotesensing", "remote sensing")
  keywords$kw_final<-str_replace(keywords$kw_final, "reproduc tion", "reproduction")
  keywords$kw_final<-str_replace(keywords$kw_final, "reproductive life span", "reproductive lifespan")
  keywords$kw_final<-str_replace(keywords$kw_final, "reproductivebiology", "reproductive biology")
  keywords$kw_final<-str_replace(keywords$kw_final, "residence patch", "patch residence")
  keywords$kw_final<-str_replace(keywords$kw_final, "residues", "residue")
  keywords$kw_final<-str_replace(keywords$kw_final, "resistance surfaces", "resistance surface")
  keywords$kw_final<-str_replace(keywords$kw_final, "resource waves", "resource wave")
  keywords$kw_final<-str_replace(keywords$kw_final, "response surfaces", "response surface")
  keywords$kw_final<-str_replace(keywords$kw_final, "responses", "response")
  keywords$kw_final<-str_replace(keywords$kw_final, "amplified fragment length polymorphism aflp", "aflp")
  keywords$kw_final<-str_replace(keywords$kw_final, "aflp markers", "aflp")
  keywords$kw_final<-str_replace(keywords$kw_final, "aflp analysis", "aflp")
  keywords$kw_final<-str_replace(keywords$kw_final, "amplified fragment length polymorphisms", "aflp")
  keywords$kw_final<-str_replace(keywords$kw_final, "amplified fragment length polymorphism", "aflp")
  keywords$kw_final<-str_replace(keywords$kw_final, "methylation sensitive amplified fragment length polymorphism ms-aflp", "ms-aflp")
  keywords$kw_final<-str_replace(keywords$kw_final, "fluorescent fragment length polymorphism fflp", "fflp")
  keywords$kw_final<-str_replace(keywords$kw_final, "terminal restriction fragment length polymorphism", "trflp")
  keywords$kw_final<-str_replace(keywords$kw_final, "t rflp", "trflp")
  keywords$kw_final<-str_replace(keywords$kw_final, "rflp analysis", "rflp")
  keywords$kw_final<-str_replace(keywords$kw_final, "restriction fragment length polymorphism rflp", "rflp")
  keywords$kw_final<-str_replace(keywords$kw_final, "restriction fragment length polymorphisms rflp", "rflp")
  keywords$kw_final<-str_replace(keywords$kw_final, "rflp restriction fragment length polymorphism", "rflp")
  keywords$kw_final<-str_replace(keywords$kw_final, "richness specific", "species richness")
  keywords$kw_final<-str_replace(keywords$kw_final, "risk  forage trade-off", "risk-forage trade-off")
  keywords$kw_final<-str_replace(keywords$kw_final, "risk forage trade-off", "risk-forage trade-off")
  keywords$kw_final<-str_replace(keywords$kw_final, "river  watershed exchange", "river watershed exchange")
  keywords$kw_final<-str_replace(keywords$kw_final, "river floodplain", "floodplain river")
  keywords$kw_final<-str_replace(keywords$kw_final, "river paraná", "parana river")
  keywords$kw_final<-str_replace(keywords$kw_final, "rna dna ratio", "rna:dna ratio")
  keywords$kw_final<-str_replace(keywords$kw_final, "rna interference rnai", "rna interference")
  keywords$kw_final<-str_replace(keywords$kw_final, "rna-dna", "rna:dna")
  keywords$kw_final<-str_replace(keywords$kw_final, "rna: dna", "rna:dna")
  keywords$kw_final<-str_replace(keywords$kw_final, "rna : dna", "rna:dna")
  keywords$kw_final<-str_replace(keywords$kw_final, "rna-dna", "rna:dna")
  keywords$kw_final<-str_replace(keywords$kw_final, "road kills", "roadkill")
  keywords$kw_final<-str_replace(keywords$kw_final, "rock paper-scissors competition", "rock-paper-scissors competition")
  keywords$kw_final<-str_replace(keywords$kw_final, "rocky intertidal shores", "intertidal rocky shores")
  keywords$kw_final<-str_replace(keywords$kw_final, "root shoot ratios", "root:shoot ratios")
  keywords$kw_final<-str_replace(keywords$kw_final, "root to-shoot ratios", "root:shoot ratios")
  keywords$kw_final<-str_replace(keywords$kw_final, "root vertical distribution", "vertical root distribution")
  keywords$kw_final<-str_replace(keywords$kw_final, "root-ratio", "root:shoot ratio")
  keywords$kw_final<-str_replace(keywords$kw_final, "rupicapra pyrenaica pyrenaica", "rupicapra pyrenaica")
  keywords$kw_final<-str_replace(keywords$kw_final, "río negro", "rio negro")
  keywords$kw_final<-str_replace(keywords$kw_final, "s patial scale", "spatial scale")
  keywords$kw_final<-str_replace(keywords$kw_final, "s peciation", "speciation")
  keywords$kw_final<-str_replace(keywords$kw_final, "sagitta ria lancifolia", "sagittaria lancifolia")
  keywords$kw_final<-str_replace(keywords$kw_final, "salamandra salamandra", "salamandra")
  keywords$kw_final<-str_replace(keywords$kw_final, "san andrés island", "san andres island")
  keywords$kw_final<-str_replace(keywords$kw_final, "savanna-forest boundary", "savanna-forest boundary")
  keywords$kw_final<-str_replace(keywords$kw_final, "scale transitions", "scale transition")
  keywords$kw_final<-str_replace(keywords$kw_final, "scatter hoarding rodents", "scatterhoarding rodents")
  keywords$kw_final<-str_replace(keywords$kw_final, "scinc idae", "scincidae")
  keywords$kw_final<-str_replace(keywords$kw_final, "sea bird", "seabird")
  keywords$kw_final<-str_replace(keywords$kw_final, "sea birds", "seabirds")
  keywords$kw_final<-str_replace(keywords$kw_final, "sea water temperature", "seawater temperature")
  keywords$kw_final<-str_replace(keywords$kw_final, "seasonal burning", "aseasonal burning")
  keywords$kw_final<-str_replace(keywords$kw_final, "secondary plant compounds", "plant secondary compounds")
  keywords$kw_final<-str_replace(keywords$kw_final, "secondgrowth forest", "second growth forest")
  keywords$kw_final<-str_replace(keywords$kw_final, "second-ary forest", "secondary forest")
  keywords$kw_final<-str_replace(keywords$kw_final, "seed : ovule ratio", "seed:ovule ratio")
  keywords$kw_final<-str_replace(keywords$kw_final, "seed predation and dispersal", "seed dispersal and predation")
  keywords$kw_final<-str_replace(keywords$kw_final, "seed predation", "seed predation")
  keywords$kw_final<-str_replace(keywords$kw_final, "seed: ovule ratio", "seed:ovule ratio")
  keywords$kw_final<-str_replace(keywords$kw_final, "seedbank", "seed bank")
  keywords$kw_final<-str_replace(keywords$kw_final, "seedling survival and growth", "seedling growth and survival")
  keywords$kw_final<-str_replace(keywords$kw_final, "seedset", "seed set")
  keywords$kw_final<-str_replace(keywords$kw_final, "selection  artificial", "artificial selection")
  keywords$kw_final<-str_replace(keywords$kw_final, "selection  experimental", "experimental selection")
  keywords$kw_final<-str_replace(keywords$kw_final, "selection  group-kin", "group-kin selection  ")
  keywords$kw_final<-str_replace(keywords$kw_final, "selection-natural", "natural selection")
  keywords$kw_final<-str_replace(keywords$kw_final, "selection-sexual", "sexual selection")
  keywords$kw_final<-str_replace(keywords$kw_final, "self  fertilization", "self-fertilization")
  keywords$kw_final<-str_replace(keywords$kw_final, "self nonself recognition", "self-nonself recognition")
  keywords$kw_final<-str_replace(keywords$kw_final, "self-non self recognition", "self-nonself recognition")
  keywords$kw_final<-str_replace(keywords$kw_final, "selforganization", "self-organization")
  keywords$kw_final<-str_replace(keywords$kw_final, "semi arid grassland", "semiarid grassland")
  keywords$kw_final<-str_replace(keywords$kw_final, "semi arid woodland", "semiarid woodland")
  keywords$kw_final<-str_replace(keywords$kw_final, "semi balanus balanoides", "semibalanus balanoides")
  keywords$kw_final<-str_replace(keywords$kw_final, "semi deciduous", "semideciduous")
  keywords$kw_final<-str_replace(keywords$kw_final, "semi deciduous tropical forest", "tropical semideciduous forest")
  keywords$kw_final<-str_replace(keywords$kw_final, "semideciduous tropical forest", "tropical semideciduous forest")
  keywords$kw_final<-str_replace(keywords$kw_final, "seroepidemiolology", "seroepidemiology")
  keywords$kw_final<-str_replace(keywords$kw_final, "serpentin e", "serpentine")
  keywords$kw_final<-str_replace(keywords$kw_final, "sesquiterpene lactones", "sesquiterpene lactone")
  keywords$kw_final<-str_replace(keywords$kw_final, "sessile invertebrates", "sessile invertebrate")
  keywords$kw_final<-str_replace(keywords$kw_final, "sexlimited polymorphism", "sex limited polymorphism")
  keywords$kw_final<-str_replace(keywords$kw_final, "sexspecific population dynamics", "sex specific population dynamics")
  keywords$kw_final<-str_replace(keywords$kw_final, "sexual ", "sexual")
  keywords$kw_final<-str_replace(keywords$kw_final, "sexually antagonistic co evolution", "sexually antagonistic coevolution")
  keywords$kw_final<-str_replace(keywords$kw_final, "sexually transmitted infections", "sexually transmitted infection")
  keywords$kw_final<-str_replace(keywords$kw_final, "shell fish", "shellfish")
  keywords$kw_final<-str_replace(keywords$kw_final, "short grass steppe", "shortgrass steppe")
  keywords$kw_final<-str_replace(keywords$kw_final, "shrimp by catch", "shrimp bycatch")
  keywords$kw_final<-str_replace(keywords$kw_final, "shrub desert", "desert shrub")
  keywords$kw_final<-str_replace(keywords$kw_final, "shrub grass competition", "grass-shrub competition")
  keywords$kw_final<-str_replace(keywords$kw_final, "shrub-steppe", "shrub-steppe")
  keywords$kw_final<-str_replace(keywords$kw_final, "sib ship reconstruction", "sibship reconstruction")
  keywords$kw_final<-str_replace(keywords$kw_final, "sierra nevada california", "california sierra nevada")
  keywords$kw_final<-str_replace(keywords$kw_final, "signal color", "color signal")
  keywords$kw_final<-str_replace(keywords$kw_final, "signaling-courtship", "signaling-courtship")
  keywords$kw_final<-str_replace(keywords$kw_final, "sink source", "source-sink")
  keywords$kw_final<-str_replace(keywords$kw_final, "sink source relationships", "source-sink relationships")
  keywords$kw_final<-str_replace(keywords$kw_final, "siskiyou klamath", "klamath siskiyou")
  keywords$kw_final<-str_replace(keywords$kw_final, "size ", "size")
  keywords$kw_final<-str_replace(keywords$kw_final, "size and age at maturity", "age and size at maturity")
  keywords$kw_final<-str_replace(keywords$kw_final, "size at-age", "size at age")
  keywords$kw_final<-str_replace(keywords$kw_final, "size-number trade off", "size number trade-off")
  keywords$kw_final<-str_replace(keywords$kw_final, "sizebias", "size bias")
  keywords$kw_final<-str_replace(keywords$kw_final, "size-number trade off", "size number trade-off")
  keywords$kw_final<-str_replace(keywords$kw_final, "slow growth-high-mortality hypothesis", "slow growth-high-mortality hypothesis")
  keywords$kw_final<-str_replace(keywords$kw_final, "small mam mals", "small mammals")
  keywords$kw_final<-str_replace(keywords$kw_final, "small mouth bass", "smallmouth bass")
  keywords$kw_final<-str_replace(keywords$kw_final, "snake bite", "snakebite")
  keywords$kw_final<-str_replace(keywords$kw_final, "socio ecological system", "socioecological system")
  keywords$kw_final<-str_replace(keywords$kw_final, "soft and hard selection", "hard and soft selection")
  keywords$kw_final<-str_replace(keywords$kw_final, "soft scale insect", "soft-scale insect")
  keywords$kw_final<-str_replace(keywords$kw_final, "soft scale insects", "soft-scale insect")
  keywords$kw_final<-str_replace(keywords$kw_final, "soil borne diseases", "soilborne diseases")
  keywords$kw_final<-str_replace(keywords$kw_final, "soil seedbank", "soil seed bank")
  keywords$kw_final<-str_replace(keywords$kw_final, "source sink population dynamics", "source-sink population dynamics")
  keywords$kw_final<-str_replace(keywords$kw_final, "source sink relationships", "source-sink relationships")
  keywords$kw_final<-str_replace(keywords$kw_final, "source-sink dynamics", "source sink dynamics")
  keywords$kw_final<-str_replace(keywords$kw_final, "south east brazil", "southeast brazil")
  keywords$kw_final<-str_replace(keywords$kw_final, "south eastern pacific", "eastern south pacific")
  keywords$kw_final<-str_replace(keywords$kw_final, "south western nigeria", "southwestern nigeria")
  keywords$kw_final<-str_replace(keywords$kw_final, "sp atial pattern", "spatial pattern")
  keywords$kw_final<-str_replace(keywords$kw_final, "space for time substitution", "space-for-time substitution")
  keywords$kw_final<-str_replace(keywords$kw_final, "spatial auto regression", "spatial autoregression")
  keywords$kw_final<-str_replace(keywords$kw_final, "spatial capturerecapture", "spatial capture-recapture")
  keywords$kw_final<-str_replace(keywords$kw_final, "spatial genetic autocorrelation", "genetic spatial autocorrelation")
  keywords$kw_final<-str_replace(keywords$kw_final, "spatial pattern distribution", "spatial distribution pattern")
  keywords$kw_final<-str_replace(keywords$kw_final, "spatial temporal variability", "spatial-temporal variability")
  keywords$kw_final<-str_replace(keywords$kw_final, "spatio temporal heterogeneity", "spatiotemporal heterogeneity")
  keywords$kw_final<-str_replace(keywords$kw_final, "spatio temporal model", "spatiotemporal model")
  keywords$kw_final<-str_replace(keywords$kw_final, "spe cies richness", "species richness")
  keywords$kw_final<-str_replace(keywords$kw_final, "specialist generalist trade-offs", "generalist-specialist trade-offs")
  keywords$kw_final<-str_replace(keywords$kw_final, "specialist generalist tradeoffs", "generalist-specialist trade-offs")
  keywords$kw_final<-str_replace(keywords$kw_final, "specialist vs generalist", "generalist vs specialist")
  keywords$kw_final<-str_replace(keywords$kw_final, "specialists vsgeneralists", "specialists vs generalists")
  keywords$kw_final<-str_replace(keywords$kw_final, "species archetype", "species archetypes")
  keywords$kw_final<-str_replace(keywords$kw_final, "species area hypothesis", "species-area hypothesis")
  keywords$kw_final<-str_replace(keywords$kw_final, "species coexistences", "species coexistence")
  keywords$kw_final<-str_replace(keywords$kw_final, "species cooccurrences", "species cooccurrence")
  keywords$kw_final<-str_replace(keywords$kw_final, "species core", "core species")
  keywords$kw_final<-str_replace(keywords$kw_final, "species indicator", "indicator species")
  keywords$kw_final<-str_replace(keywords$kw_final, "species individual curve", "species-individual curve")
  keywords$kw_final<-str_replace(keywords$kw_final, "species individual-curves", "species-individual curve")
  keywords$kw_final<-str_replace(keywords$kw_final, "species phylogenetics", "phylogenetic species")
  keywords$kw_final<-str_replace(keywords$kw_final, "species range edges", "species range edge")
  keywords$kw_final<-str_replace(keywords$kw_final, "species tree distribution", "tree species distribution")
  keywords$kw_final<-str_replace(keywords$kw_final, "species -area relationship", "species-area relationship")
  keywords$kw_final<-str_replace(keywords$kw_final, "speciesabundance distributions", "species-abundance distributions")
  keywords$kw_final<-str_replace(keywords$kw_final, "species-energy", "species energy")
  keywords$kw_final<-str_replace(keywords$kw_final, "specific richness", "species richness")
  keywords$kw_final<-str_replace(keywords$kw_final, "spermcompetition", "sperm competition")
  keywords$kw_final<-str_replace(keywords$kw_final, "spider silks", "silk spiders")
  keywords$kw_final<-str_replace(keywords$kw_final, "spill over", "spillover")
  keywords$kw_final<-str_replace(keywords$kw_final, "stable carbon and nitrogen isotopes", "carbon and nitrogen stable isotopes")
  keywords$kw_final<-str_replace(keywords$kw_final, "stable carbon isotope", "carbon stable isotope")
  keywords$kw_final<-str_replace(keywords$kw_final, "stable carbon isotope ratios", "stable carbon-isotope ratios")
  keywords$kw_final<-str_replace(keywords$kw_final, "stable hydrogen isotopes", "hydrogen stable isotopes")
  keywords$kw_final<-str_replace(keywords$kw_final, "stable isotope  n 15", "15n stable isotope")
  keywords$kw_final<-str_replace(keywords$kw_final, "stable nitrogen isotopes", "nitrogen stable isotopes")
  keywords$kw_final<-str_replace(keywords$kw_final, "stage age structure", "age-stage structure")
  keywords$kw_final<-str_replace(keywords$kw_final, "stage specific vital rate", "stage-specific vital rate")
  keywords$kw_final<-str_replace(keywords$kw_final, "stage specific vital rates", "stage-specific vital rate")
  keywords$kw_final<-str_replace(keywords$kw_final, "stage structured", "stage-structured")
  keywords$kw_final<-str_replace(keywords$kw_final, "stage structured populations", "stage-structured populations")
  keywords$kw_final<-str_replace(keywords$kw_final, "stagestructured", "stage-structured")
  keywords$kw_final<-str_replace(keywords$kw_final, "stagestructured models", "stage-structured models")
  keywords$kw_final<-str_replace(keywords$kw_final, "stagestructured populations", "stage-structured populations")
  keywords$kw_final<-str_replace(keywords$kw_final, "state and-transition model", "state and transition model")
  keywords$kw_final<-str_replace(keywords$kw_final, "stink bugs", "stinkbugs")
  keywords$kw_final<-str_replace(keywords$kw_final, "stochastic blockmodel", "stochastic block model")
  keywords$kw_final<-str_replace(keywords$kw_final, "stress hormones", "stress hormone")
  keywords$kw_final<-str_replace(keywords$kw_final, "stress responses", "stress response")
  keywords$kw_final<-str_replace(keywords$kw_final, "strix occidentalis occidentalis", "strix occidentalis")
  keywords$kw_final<-str_replace(keywords$kw_final, "structural equation modelingsem", "structural equation modeling")
  keywords$kw_final<-str_replace(keywords$kw_final, "structure community", "community structure")
  keywords$kw_final<-str_replace(keywords$kw_final, "structure function", "structure-function")
  keywords$kw_final<-str_replace(keywords$kw_final, "structure population", "population structure")
  keywords$kw_final<-str_replace(keywords$kw_final, "structure size", "size structure")
  keywords$kw_final<-str_replace(keywords$kw_final, "structure vegetation", "vegetation structure")
  keywords$kw_final<-str_replace(keywords$kw_final, "sub antarctic", "subantarctic")
  keywords$kw_final<-str_replace(keywords$kw_final, "sub humid tropics", "subhumid tropics")
  keywords$kw_final<-str_replace(keywords$kw_final, "sub lethal effects", "sublethal effects")
  keywords$kw_final<-str_replace(keywords$kw_final, "subarctic salt marsh", "subarctic saltmarsh")
  keywords$kw_final<-str_replace(keywords$kw_final, "subtropical evergreen broad leaved forest", "subtropical evergreen broadleaved forest")
  keywords$kw_final<-str_replace(keywords$kw_final, "sub-saharan africa", "sub-saharan africa")
  keywords$kw_final<-str_replace(keywords$kw_final, "succession rates", "succession rate")
  keywords$kw_final<-str_replace(keywords$kw_final, "super population", "superpopulation")
  keywords$kw_final<-str_replace(keywords$kw_final, "superspreaders", "superspreader")
  keywords$kw_final<-str_replace(keywords$kw_final, "surface area to-volume ratio", "surface area to volume ratio")
  keywords$kw_final<-str_replace(keywords$kw_final, "t ropics", "tropics")
  keywords$kw_final<-str_replace(keywords$kw_final, "tall grass prairie", "tallgrass prairie")
  keywords$kw_final<-str_replace(keywords$kw_final, "taylors powerlaw", "taylors power law")
  keywords$kw_final<-str_replace(keywords$kw_final, "tehuacán valley", "tehuacan valley")
  keywords$kw_final<-str_replace(keywords$kw_final, "temperate tropical comparison", "temperate vs tropical")
  keywords$kw_final<-str_replace(keywords$kw_final, "temperature  dependent sex determination", "temperature-dependent sex determination")
  keywords$kw_final<-str_replace(keywords$kw_final, "temperature size-rule", "temperature size rule")
  keywords$kw_final<-str_replace(keywords$kw_final, "temporal and spatial distribution", "spatial and temporal distribution")
  keywords$kw_final<-str_replace(keywords$kw_final, "temporal and spatial scale", "spatial and temporal scale")
  keywords$kw_final<-str_replace(keywords$kw_final, "temporal and spatial variation", "spatial and temporal variation")
  keywords$kw_final<-str_replace(keywords$kw_final, "terraba sierpe", "sierpe térraba")
  keywords$kw_final<-str_replace(keywords$kw_final, "terrestrial gasteropods", "terrestrial gastropods")
  keywords$kw_final<-str_replace(keywords$kw_final, "territorial signals", "territorial signal")
  keywords$kw_final<-str_replace(keywords$kw_final, "territory establishment and quality", "territory quality and establishment")
  keywords$kw_final<-str_replace(keywords$kw_final, "testoster one", "testosterone")
  keywords$kw_final<-str_replace(keywords$kw_final, "tetrahymena tetrahymena", "tetrahymena")
  keywords$kw_final<-str_replace(keywords$kw_final, "th ermoregulation", "thermoregulation")
  keywords$kw_final<-str_replace(keywords$kw_final, "thermal co adaptation", "thermal coadaptation")
  keywords$kw_final<-str_replace(keywords$kw_final, "thermo tolerance", "thermotolerance")
  keywords$kw_final<-str_replace(keywords$kw_final, "time scale", "timescale")
  keywords$kw_final<-str_replace(keywords$kw_final, "time scales", "timescale")
  keywords$kw_final<-str_replace(keywords$kw_final, "time-activity budgets", "time activity budgets")
  keywords$kw_final<-str_replace(keywords$kw_final, "tolerance fecundity trade-off", "tolerance-fecundity trade off")
  keywords$kw_final<-str_replace(keywords$kw_final, "top down vs bottom up effects", "top=down vs bottom-up effects")
  keywords$kw_final<-str_replace(keywords$kw_final, "top down vs bottom-up effects", "top=down vs bottom-up effects")
  keywords$kw_final<-str_replace(keywords$kw_final, "top kill", "topkill")
  keywords$kw_final<-str_replace(keywords$kw_final, "top soil", "topsoil")
  keywords$kw_final<-str_replace(keywords$kw_final, "topdown forces", "top-down forces")
  keywords$kw_final<-str_replace(keywords$kw_final, "topdown limitation", "top-down limitation")
  keywords$kw_final<-str_replace(keywords$kw_final, "toppredator", "top predator")
  keywords$kw_final<-str_replace(keywords$kw_final, "torus translations", "torus translation")
  keywords$kw_final<-str_replace(keywords$kw_final, "tracking climate", "climate tracking")
  keywords$kw_final<-str_replace(keywords$kw_final, "trade offs of food and safety", "tradeoffs of food and safety")
  keywords$kw_final<-str_replace(keywords$kw_final, "trait environment interaction", "trait-environment interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "trait shifts", "trait shift")
  keywords$kw_final<-str_replace(keywords$kw_final, "traitmediated indirect interaction", "trait-mediated indirect interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "trans generation", "transgeneration")
  keywords$kw_final<-str_replace(keywords$kw_final, "transfer functions", "transfer function")
  keywords$kw_final<-str_replace(keywords$kw_final, "transformations", "transformation")
  keywords$kw_final<-str_replace(keywords$kw_final, "transgressive over yielding", "transgressive overyielding")
  keywords$kw_final<-str_replace(keywords$kw_final, "translocations", "translocation")
  keywords$kw_final<-str_replace(keywords$kw_final, "transmis sion", "transmission")
  keywords$kw_final<-str_replace(keywords$kw_final, "transmission virulence tradeoff", "transmission virulence trade-off")
  keywords$kw_final<-str_replace(keywords$kw_final, "tree grass balance", "tree-grass coexistance")
  keywords$kw_final<-str_replace(keywords$kw_final, "tree liana competition", "liana-tree competition")
  keywords$kw_final<-str_replace(keywords$kw_final, "tree liana interaction", "liana-tree interaction")
  keywords$kw_final<-str_replace(keywords$kw_final, "tree regression analysis", "regression tree analysis")
  keywords$kw_final<-str_replace(keywords$kw_final, "tree survival and growth", "tree growth and survival")
  keywords$kw_final<-str_replace(keywords$kw_final, "tree-grass balance", "tree-grass coexistance")
  keywords$kw_final<-str_replace(keywords$kw_final, "tree-grass co existence", "tree-grass coexistence")
  keywords$kw_final<-str_replace(keywords$kw_final, "tree-grass interactions", "tree-grass interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "tree-grass interactions", "tree-grass interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "tri trophic", "tritrophic")
  keywords$kw_final<-str_replace(keywords$kw_final, "tri trophic system", "tritrophic system")
  keywords$kw_final<-str_replace(keywords$kw_final, "tropical agro ecosystem", "tropical agroecosystem")
  keywords$kw_final<-str_replace(keywords$kw_final, "tropical high mountains", "high tropical mountains")
  keywords$kw_final<-str_replace(keywords$kw_final, "tropical primary forest", "primary tropical forest")
  keywords$kw_final<-str_replace(keywords$kw_final, "tropical sub montane forest", "tropical submontane forest")
  keywords$kw_final<-str_replace(keywords$kw_final, "truss box", "box truss")
  keywords$kw_final<-str_replace(keywords$kw_final, "tube worm", "tubeworm")
  keywords$kw_final<-str_replace(keywords$kw_final, "térraba", "terraba")
  keywords$kw_final<-str_replace(keywords$kw_final, "túngara frog", "tungara frog")
  keywords$kw_final<-str_replace(keywords$kw_final, "ultra structure", "ultrastructure")
  keywords$kw_final<-str_replace(keywords$kw_final, "under dispersion", "underdispersion")
  keywords$kw_final<-str_replace(keywords$kw_final, "under sampling", "undersampling")
  keywords$kw_final<-str_replace(keywords$kw_final, "up scaling", "scaling up")
  keywords$kw_final<-str_replace(keywords$kw_final, "urban rural gradient", "urban-rural gradient")
  keywords$kw_final<-str_replace(keywords$kw_final, "variance mean relationship", "mean-variance relationship")
  keywords$kw_final<-str_replace(keywords$kw_final, "viability ", "viability")
  keywords$kw_final<-str_replace(keywords$kw_final, "walkingsticks", "walking sticks")
  keywords$kw_final<-str_replace(keywords$kw_final, "water borne cues", "waterborne cues")
  keywords$kw_final<-str_replace(keywords$kw_final, "water frogs", "waterfrogs")
  keywords$kw_final<-str_replace(keywords$kw_final, "water holes", "waterholes")
  keywords$kw_final<-str_replace(keywords$kw_final, "waterstress", "water stress")
  keywords$kw_final<-str_replace(keywords$kw_final, "weaver spiders", "weaver spider")
  keywords$kw_final<-str_replace(keywords$kw_final, "weddell seals", "weddell seal")
  keywords$kw_final<-str_replace(keywords$kw_final, "weight length relationship", "length-weight relationship")
  keywords$kw_final<-str_replace(keywords$kw_final, "west central méxico", "west central mexico")
  keywords$kw_final<-str_replace(keywords$kw_final, "wet evergreen tropical forest", "tropical wet evergreen forest")
  keywords$kw_final<-str_replace(keywords$kw_final, "white spruce  picea glauca ", "white spruce picea glauca ")
  keywords$kw_final<-str_replace(keywords$kw_final, "white spruce picea glauca", "white spruce picea glauca ")
  keywords$kw_final<-str_replace(keywords$kw_final, "wild fires", "wildfires")
  keywords$kw_final<-str_replace(keywords$kw_final, "wind speeds", "wind speed")
  keywords$kw_final<-str_replace(keywords$kw_final, "wind storm", "windstorm")
  keywords$kw_final<-str_replace(keywords$kw_final, "wind throw", "windthrow")
  keywords$kw_final<-str_replace(keywords$kw_final, "wing thorax ratio", "wing:thorax ratio")
  keywords$kw_final<-str_replace(keywords$kw_final, "winner loser effect", "winner-loser effect")
  keywords$kw_final<-str_replace(keywords$kw_final, "within host in-teractions", "within host interactions")
  keywords$kw_final<-str_replace(keywords$kw_final, "withinhost dynamics", "within-host dynamics")
  keywords$kw_final<-str_replace(keywords$kw_final, "within-hostdynamics", "within-host dynamics")
  keywords$kw_final<-str_replace(keywords$kw_final, "wood ", "wood")
  keywords$kw_final<-str_replace(keywords$kw_final, "x y recombination", "xy recombination")
  keywords$kw_final<-str_replace(keywords$kw_final, "xel há", "xel ha")
  keywords$kw_final<-str_replace(keywords$kw_final, "y ield", "yield")
  keywords$kw_final<-str_replace(keywords$kw_final, "yplant", "y plant")
  keywords$kw_final<-str_replace(keywords$kw_final, "yucatán península", "yucatan peninsula")
  keywords$kw_final<-str_replace(keywords$kw_final, "yucca yucca moth", "yucca moth")
  keywords$kw_final<-str_replace(keywords$kw_final, "z imbabwe", "zimbabwe")
  keywords$kw_final<-str_replace(keywords$kw_final, "zvariegatus", "z variegatus")
  keywords$kw_final<-str_replace(keywords$kw_final, "δ n natural abundance 15", "natural abundance δ n15")
  keywords$kw_final<-str_replace(keywords$kw_final, "ecosystem functioning", "ecosystem function")
  # keywords$kw_final<-str_replace(keywords$kw_final, "amazon", "amazonia")
  keywords$kw_final<-str_replace(keywords$kw_final, "modeling", "model")
  keywords$kw_final<-str_replace(keywords$kw_final, "biodiversity and ecosystem function", "biodiversity-ecosystem function")
  keywords$kw_final<-str_replace(keywords$kw_final, "biodiversity and ecosystem service", "biodiversity-ecosystem function")
  keywords$kw_final<-str_replace(keywords$kw_final, "biodiversity and ecosystem function relationship", "biodiversity-ecosystem function")
  keywords$kw_final<-str_replace(keywords$kw_final, "biodiversity and ecosystem functioning theory", "biodiversity-ecosystem function")
  keywords$kw_final<-str_replace(keywords$kw_final, "biodiversity ecosystem function bdef", "biodiversity-ecosystem function")
  keywords$kw_final<-str_replace(keywords$kw_final, "biodiversity-ecosystem function bef", "biodiversity-ecosystem function")
  keywords$kw_final<-str_replace(keywords$kw_final, "yasuni ecological research station", "yasuni")
  keywords$kw_final<-str_replace(keywords$kw_final, "la selva biological station", "la selva")
  keywords$kw_final<-str_replace(keywords$kw_final, "santa rosa", "santa rosa national park")
  keywords$kw_final<-str_replace(keywords$kw_final, "trade offs", "tradeoff")
  keywords$kw_final<-str_replace(keywords$kw_final, "trade off", "tradeoff")
  
  
  
#  end try ----------------------------------------------------------------

  
  
  unique_kw_summary <- keywords %>%
    group_by(kw_final) %>%
    tally() %>%
    arrange(desc(n))
  unique_kw_summary
# refined3<- read_csv("./bibliometrics/data_intermediate/refined3_edit.csv") %>% select(-n)
# 
# keywords <- keywords %>%
#   mutate(kw_final = gsub("life history", "life-history", kw_final)) %>%
#   mutate(kw_final = gsub("trade offs", "tradeoff", kw_final)) %>% 
#   # kw_final =="trade offs"~"tradeoffs", kw_final)) %>%
#   mutate(kw_final = gsub("trade off", "tradeoff", kw_final)) %>%
#   mutate(kw_final = gsub("life history evolution", "life-history evolution", kw_final)) %>%
#   mutate(kw_final = gsub("meta analysis", "metaanalysis", kw_final)) %>%
#   mutate(kw_final = gsub("plant herbivore interactions", "plant-herbivore interaction", kw_final)) %>%
#   # kw_final =="plant herbivore interactions"~"plant-herbivore interactions", kw_final)) %>%
#   mutate(kw_final = gsub("predator prey interactions", "predator-prey interaction", kw_final)) %>%
#   # kw_final =="predator prey interactions"~"predator-prey interactions", kw_final)) %>%
#   mutate(kw_final = gsub("life history traits", "life-history trait", kw_final)) %>%
#   # kw_final =="life history traits"~"life-history traits", kw_final)) %>%
#   mutate(kw_final = gsub("plant insect interactions", "plant-insect interaction", kw_final)) %>%
#   mutate(kw_final = gsub("self fertilization", "self-fertilization", kw_final)) %>%
#   mutate(kw_final = gsub("home range", "homerange", kw_final)) %>%
#   mutate(kw_final = gsub("frequency dependent selection", "frequency-dependent selection", kw_final)) %>%
#   mutate(kw_final = gsub("predator prey", "predator-prey", kw_final)) %>%
#   mutate(kw_final = gsub("phylogenetic comparative methods", "comparative phylogenetic method", kw_final)) %>%
#   # kw_final =="phylogenetic comparative methods"~"comparative phylogenetic methods", kw_final)) %>%
#   mutate(kw_final = gsub("plant soil belowground interactions", "plant-soil belowground interaction", kw_final)) %>%
#   # kw_final =="plant soil belowground interactions"~"plant-soil belowground interactions", kw_final)) %>%
#   mutate(kw_final = gsub("individual based model", "individual-based model", kw_final)) %>%
#   mutate(kw_final = gsub("plant animal interactions", "plant-animal interactions", kw_final)) %>%
#   mutate(kw_final = gsub("plant climate interactions", "plant-climate interactions", kw_final)) %>%
#   mutate(kw_final = gsub("host parasite interactions", "host-parasite interactions", kw_final)) %>%
#   mutate(kw_final = gsub("méxico", "mexico", kw_final)) %>%
#   mutate(kw_final = gsub("salt marsh", "saltmarsh", kw_final)) %>%
#   mutate(kw_final = gsub("plant plant interactions", "plant interactions", kw_final)) %>%
#   mutate(kw_final = gsub("land use", "land-use", kw_final)) %>%
#   mutate(kw_final = gsub("plant-herbivore interactions", "plant-herbivore interactions", kw_final)) %>%
#   mutate(kw_final = gsub("host parasite interaction", "host-parasite interaction", kw_final)) %>%
#   mutate(kw_final = gsub("capture recapture", "capture-recapture", kw_final)) %>%
#   mutate(kw_final = gsub("plant diversity", "diversity plant", kw_final)) %>%
#   mutate(kw_final = gsub("mark recapture", "mark-recapture", kw_final)) %>%
#   mutate(kw_final = gsub("eco evolutionary dynamics", "eco-evolutionary dynamics", kw_final)) %>%
#   mutate(kw_final = gsub("isolation by distance", "isolation-by-distance", kw_final)) %>%
#   mutate(kw_final = gsub("top down control", "top-down control", kw_final)) %>%
#   mutate(kw_final = gsub("life history trade-offs", "life-history tradeoffs", kw_final)) %>%
#   mutate(kw_final = gsub("species area relationship", "species-area relationship", kw_final)) %>%
#   mutate(kw_final = gsub("life span", "lifespan", kw_final)) %>%
#   mutate(kw_final = gsub("genotype by-environment interaction", "gxe", kw_final)) %>%
#   mutate(kw_final = gsub("plant functional traits", "functional plant traits", kw_final)) %>%
#   mutate(kw_final = gsub("plant-plant interactions", "ant-plant interactions", kw_final)) %>%
#   mutate(kw_final = gsub("predator prey interaction", "predator-prey interaction", kw_final)) %>%
#   mutate(kw_final = gsub("janzen connell hypothesis", "janzen-connell hypothesis", kw_final)) %>%
#   mutate(kw_final = gsub("el niño", "enso", kw_final)) %>%
#   mutate(kw_final = gsub("host parasite coevolution", "host-parasite coevolution", kw_final)) %>%
#   mutate(kw_final = gsub("long distance dispersal", "long-distance dispersal", kw_final)) %>%
#   mutate(kw_final = gsub("parent offspring conflict", "parent-offspring conflict", kw_final)) %>%
#   mutate(kw_final = gsub("insect plant interactions", "plant-insect interaction", kw_final)) %>%
#   mutate(kw_final = gsub("tropical montane forest", "montane tropical forest", kw_final)) %>%
#   mutate(kw_final = gsub("top down", "top-down", kw_final)) %>%
#   mutate(kw_final = gsub("host parasite", "host-parasite", kw_final)) %>%
#   mutate(kw_final = gsub("co evolution", "coevolution", kw_final)) %>%
#   mutate(kw_final = gsub("host pathogen interactions", "host-pathogen interactions", kw_final)) %>%
#   mutate(kw_final = gsub("old growth forest", "old-growth forest", kw_final)) %>%
#   mutate(kw_final = gsub("stage structure", "age-stage structure", kw_final)) %>%
#   mutate(kw_final = gsub("ant plant interactions", "ant-plant interactions", kw_final)) %>%
#   mutate(kw_final = gsub("male male competition", "male-male competition", kw_final)) %>%
#   mutate(kw_final = gsub("plant soil feedback", "plant-soil feedback", kw_final))
# 
# keywords <- keywords %>%
#   mutate(kw_final = gsub("predator prey dynamics", "predator-prey dynamics", kw_final)) %>%
#   mutate(kw_final = gsub("co occurrence", "cooccurrence", kw_final)) %>%
#   mutate(kw_final = gsub("population genetic structure", "genetic population structure", kw_final)) %>%
#   mutate(kw_final = gsub("predator-prey interactions", "predator-prey interactions", kw_final)) %>%
#   mutate(kw_final = gsub("plant herbivore interaction", "plant-herbivore interaction", kw_final)) %>%
#   mutate(kw_final = gsub("plant insect interaction", "insect-plant interaction", kw_final)) %>%
#   mutate(kw_final = gsub("state space model", "state-space model", kw_final)) %>%
#   mutate(kw_final = gsub("15 n", "15n", kw_final)) %>%
#   mutate(kw_final = gsub("artemisia tridentata", "artemisia tridentate", kw_final)) %>%
#   mutate(kw_final = gsub("biodiversity ecosystem functioning", "biodiversity-ecosystem functioning", kw_final)) %>%
#   mutate(kw_final = gsub("elevated co 2", "elevated co2", kw_final)) %>%
#   mutate(kw_final = gsub("f st", "fst", kw_final)) %>%
#   mutate(kw_final = gsub("poeciliidae", "poeciilidae", kw_final)) %>%
#   mutate(kw_final = gsub("life history trade-off", "life-history trade-off", kw_final)) %>%
#   mutate(kw_final = gsub("plant animal interaction", "plant-animal interaction", kw_final)) %>%
#   mutate(kw_final = gsub("resources", "resource", kw_final)) %>%
#   mutate(kw_final = gsub("self organization", "self-organization", kw_final)) %>%
#   mutate(kw_final = gsub("source sink", "source-sink", kw_final)) %>%
#   mutate(kw_final = gsub("biodiversity hotspot", "biodiversity hotspots", kw_final)) %>%
#   mutate(kw_final = gsub("el niño southern oscillation", "enso", kw_final)) %>%
#   mutate(kw_final = gsub("extra pair paternity", "extrapair paternity", kw_final)) %>%
#   mutate(kw_final = gsub("phylogenetic comparative method", "comparative phylogenetic method", kw_final)) %>%
#   mutate(kw_final = gsub("plant pollinator interactions", "plant-pollinator interactions", kw_final)) %>%
#   mutate(kw_final = gsub("alces alces", "alces", kw_final)) %>%
#   mutate(kw_final = gsub("life table response experiments", "life-table response experiments", kw_final)) %>%
#   mutate(kw_final = gsub("long term monitoring", "long-term monitoring", kw_final)) %>%
#   mutate(kw_final = gsub("plant-soil interactions", "plant-soil interactions", kw_final)) %>%
#   mutate(kw_final = gsub("radio tracking", "radiotracking", kw_final)) %>%
#   mutate(kw_final = gsub("scatter hoarding", "scatterhoarding", kw_final)) %>%
#   mutate(kw_final = gsub("selection natural", "natural selection", kw_final)) %>%
#   mutate(kw_final = gsub("species distribution modeling", "modeling species distribution", kw_final)) %>%
#   mutate(kw_final = gsub("tropical eastern pacific", "eastern tropical pacific", kw_final)) %>%
#   mutate(kw_final = gsub("leaf life span", "leaf lifespan", kw_final)) %>%
#   mutate(kw_final = gsub("long term data", "long-term data", kw_final)) %>%
#   mutate(kw_final = gsub("plant soil interactions", "plant-soil interactions", kw_final)) %>%
#   mutate(kw_final = gsub("plant-animal interactions", "plant-animal interactions", kw_final)) %>%
#   mutate(kw_final = gsub("trait mediated indirect interactions", "indirect trait mediated interactions", kw_final)) %>%
#   mutate(kw_final = gsub("biodiversity ecosystem function", "biodiversity-ecosystem function", kw_final)) %>%
#   mutate(kw_final = gsub("bird song", "birdsong", kw_final)) %>%
#   mutate(kw_final = gsub("enemy free space", "enemy-free space", kw_final)) %>%
#   mutate(kw_final = gsub("evo devo", "evo-devo", kw_final)) %>%
#   mutate(kw_final = gsub("negative frequency dependent selection", "negative frequency-dependent selection", kw_final)) %>%
#   mutate(kw_final = gsub("non timber forest products", "nontimber forest products", kw_final)) %>%
#   mutate(kw_final = gsub("semi arid", "semiarid", kw_final)) %>%
#   mutate(kw_final = gsub("co 2", "co2", kw_final)) %>%
#   mutate(kw_final = gsub("plant soil feedbacks", "plant-soil feedbacks", kw_final)) %>%
#   mutate(kw_final = gsub("plant-pollinator interactions", "plant-pollinator interactions", kw_final)) %>%
#   mutate(kw_final = gsub("radio telemetry", "radiotelemetry", kw_final)) %>%
#   mutate(kw_final = gsub("south east asia", "southeast asia", kw_final)) %>%
#   mutate(kw_final = gsub("tree line", "treeline", kw_final)) %>%
#   mutate(kw_final = gsub("ant plant mutualism", "ant-plant mutualism", kw_final)) %>%
#   mutate(kw_final = gsub("giving up density", "giving-up density", kw_final)) %>%
#   mutate(kw_final = gsub("janzen-connell hypothesis", "janzen-connell hypothesis", kw_final)) %>%
#   mutate(kw_final = gsub("spatiotemporal variation", "spatio-temporal variation", kw_final)) %>%
#   mutate(kw_final = gsub("agent based model", "agent-based model", kw_final)) %>%
#   mutate(kw_final = gsub("bumble bee", "bumblebee", kw_final)) %>%
#   mutate(kw_final = gsub("consumer resource interactions", "consumer-resource interactions", kw_final)) %>%
#   mutate(kw_final = gsub("galápagos", "galapagos", kw_final)) %>%
#   mutate(kw_final = gsub("gene for-gene", "gene-for-gene", kw_final)) %>%
#   mutate(kw_final = gsub("plant microbe interactions", "plant-microbe interactions", kw_final)) %>%
#   mutate(kw_final = gsub("plant pathogen interactions", "plant-pathogen interactions", kw_final)) %>%
#   mutate(kw_final = gsub("plant-insect interactions", "plant-insect interaction", kw_final)) %>%
#   mutate(kw_final = gsub("genotype by-environment interactions", "gxe", kw_final)) %>%
#   mutate(kw_final = gsub("gonadosomatic index", "gonado-somatic index", kw_final)) %>%
#   mutate(kw_final = gsub("host-parasite interactions", "host-parasite interactions", kw_final)) %>%
#   mutate(kw_final = gsub("invasive alien species", "alien invasive species", kw_final)) %>%
#   mutate(kw_final = gsub("stingless bees", "stingless bee", kw_final)) %>%
#   mutate(kw_final = gsub("biodiversity-ecosystem functioning", "biodiversity-ecosystem functioning", kw_final)) %>%
#   mutate(kw_final = gsub("host parasitoid interactions", "host-parasitoid interactions", kw_final)) %>%
#   mutate(kw_final = gsub("plant pollinator interaction", "plant-pollinator interaction", kw_final)) %>%
#   mutate(kw_final = gsub("plant secondary metabolites", "secondary plant metabolites", kw_final)) %>%
#   mutate(kw_final = gsub("temperature dependent sex determination", "temperature-dependent sex determination", kw_final)) %>%
#   mutate(kw_final = gsub("tri trophic interactions", "tritrophic interactions", kw_final)) %>%
#   mutate(kw_final = gsub("anti predator behavior", "antipredator behavior", kw_final)) %>%
#   mutate(kw_final = gsub("consumer resource dynamics", "consumer-resource dynamics", kw_final)) %>%
#   mutate(kw_final = gsub("dry tropical forest", "tropical dry forest", kw_final)) %>%
#   mutate(kw_final = gsub("host pathogen interaction", "host-pathogen interaction", kw_final)) %>%
#   mutate(kw_final = gsub("multi trophic interactions", "multitrophic interactions", kw_final)) %>%
#   mutate(kw_final = gsub("post copulatory sexual selection", "postcopulatory sexual selection", kw_final)) %>%
#   mutate(kw_final = gsub("post zygotic isolation", "postzygotic isolation", kw_final)) %>%
#   mutate(kw_final = gsub("pre dispersal seed predation", "predispersal seed predation", kw_final)) %>%
#   mutate(kw_final = gsub("predator prey model", "predator-prey model", kw_final)) %>%
#   mutate(kw_final = gsub("predator-prey interaction", "predator-prey interaction", kw_final)) %>%
#   mutate(kw_final = gsub("várzea", "varzea", kw_final)) %>%
#   mutate(kw_final = gsub("alternate stable states", "alternate stable state", kw_final)) %>%
#   mutate(kw_final = gsub("co infection", "coinfection", kw_final)) %>%
#   mutate(kw_final = gsub("insect plant interaction", "insect-plant interaction", kw_final)) %>%
#   mutate(kw_final = gsub("isolation by-distance", "isolation-by-distance", kw_final))
# 
# keywords <- keywords %>%
#   mutate(kw_final = gsub("müllerian mimicry", "mullerian mimicry", kw_final)) %>%
#   mutate(kw_final = gsub("non native species", "nonnative species", kw_final)) %>%
#   mutate(kw_final = gsub("phylogenetic comparative analysis", "comparative phylogenetic analysis", kw_final)) %>%
#   mutate(kw_final = gsub("predator-prey", "predator-prey", kw_final)) %>%
#   mutate(kw_final = gsub("resource pulses", "resource pulse", kw_final)) %>%
#   mutate(kw_final = gsub("species tree", "tree species", kw_final)) %>%
#   mutate(kw_final = gsub("trait mediated indirect interaction", "trait-mediated indirect interaction", kw_final)) %>%
#   mutate(kw_final = gsub("within host dynamics", "within-host dynamics", kw_final)) %>%
#   mutate(kw_final = gsub("distance decay", "distance-decay", kw_final)) %>%
#   mutate(kw_final = gsub("eco evolutionary feedback", "ecoevolutionary feedback", kw_final)) %>%
#   mutate(kw_final = gsub("floodplain forest", "flood plain forest", kw_final)) %>%
#   mutate(kw_final = gsub("genotype by environment interaction", "gxe", kw_final)) %>%
#   mutate(kw_final = gsub("life history trade offs", "life-history tradeoffs", kw_final)) %>%
#   mutate(kw_final = gsub("non consumptive effects", "nonconsumptive effects", kw_final)) %>%
#   mutate(kw_final = gsub("plant-herbivore interaction", "plant-herbivore interaction", kw_final)) %>%
#   mutate(kw_final = gsub("sexual signals", "sexual signal", kw_final)) %>%
#   mutate(kw_final = gsub("species co occurrence", "species cooccurrence", kw_final)) %>%
#   mutate(kw_final = gsub("bayesian hierarchical model", "hierarchical bayesian model", kw_final)) %>%
#   mutate(kw_final = gsub("bumble bees", "bumblebees", kw_final)) %>%
#   mutate(kw_final = gsub("correlated response", "correlated responses", kw_final)) %>%
#   mutate(kw_final = gsub("el nino", "enso", kw_final)) %>%
#   mutate(kw_final = gsub("life history trade off", "life-history trade-off", kw_final)) %>%
#   mutate(kw_final = gsub("non additive effects", "nonadditive effects", kw_final)) %>%
#   mutate(kw_final = gsub("plant-pathogen interactions", "plant-pathogen interactions", kw_final)) %>%
#   mutate(kw_final = gsub("radseq", "rad seq", kw_final)) %>%
#   mutate(kw_final = gsub("rattus rattus", "rattus", kw_final)) %>%
#   mutate(kw_final = gsub("signaling-courtship", "signaling-courtship", kw_final)) %>%
#   mutate(kw_final = gsub("yucatán", "yucatan", kw_final)) %>%
#   mutate(kw_final = gsub("anti herbivore defense", "antiherbivore defense", kw_final)) %>%
#   mutate(kw_final = gsub("côte divoire", "ivory coast", kw_final)) %>%
#   mutate(kw_final = gsub("eastern himalaya", "eastern himalayas", kw_final)) %>%
#   mutate(kw_final = gsub("host pathogen", "host-pathogen", kw_final)) %>%
#   mutate(kw_final = gsub("host-parasite interaction", "host-parasite interaction", kw_final)) %>%
#   mutate(kw_final = gsub("human wildlife conflict", "human-wildlife conflict", kw_final)) %>%
#   mutate(kw_final = gsub("long term", "longterm", kw_final)) %>%
#   mutate(kw_final = gsub("lotka volterra", "lotka-volterra", kw_final)) %>%
#   mutate(kw_final = gsub("n 15", "15n", kw_final)) %>%
#   mutate(kw_final = gsub("non structural carbohydrates", "nonstructural carbohydrates", kw_final)) %>%
#   mutate(kw_final = gsub("ornstein uhlenbeck", "ornstein-uhlenbeck", kw_final)) %>%
#   mutate(kw_final = gsub("pair correlation function", "pair-correlation function", kw_final)) %>%
#   mutate(kw_final = gsub("pesticides", "pesticide", kw_final)) %>%
#   mutate(kw_final = gsub("post dispersal seed predation", "postdispersal seed predation", kw_final)) %>%
#   mutate(kw_final = gsub("queen worker conflict", "queen-worker conflict", kw_final)) %>%
#   mutate(kw_final = gsub("species area relationships", "species-area relationships", kw_final)) %>%
#   mutate(kw_final = gsub("stable carbon isotopes", "carbon stable isotopes", kw_final)) %>%
#   mutate(kw_final = gsub("wet tropical forest", "tropical wet forest", kw_final)) %>%
#   mutate(kw_final = gsub("aboveground belowground interactions", "aboveground-belowground interactions", kw_final)) %>%
#   mutate(kw_final = gsub("aboveground-belowground interactions", "aboveground-belowground interactions", kw_final)) %>%
#   mutate(kw_final = gsub("consumer resource interaction", "consumer-resource interactions", kw_final)) %>%
#   mutate(kw_final = gsub("dead wood", "deadwood", kw_final)) %>%
#   mutate(kw_final = gsub("density mediated indirect interactions", "density-mediated indirect interactions", kw_final)) %>%
#   mutate(kw_final = gsub("ecoimmunology", "eco immunology", kw_final)) %>%
#   mutate(kw_final = gsub("evolutionary transitions", "evolutionary transition", kw_final)) %>%
#   mutate(kw_final = gsub("forest tundra", "forest-tundra", kw_final))
# library(stringr)
# keywords <- keywords$kw_final <-str_replace(keywords$kw_final,"honey bee", "honeybee")
# keywords <- keywords %>%
#   mutate(kw_final = gsub("honey bee", "honeybee", kw_final)) %>%
#   mutate(kw_final = gsub("host feeding", "host-feeding", kw_final)) %>%
#   mutate(kw_final = gsub("leaf fall", "leaf-fall", kw_final)) %>%
#   mutate(kw_final = gsub("lotka volterra model", "lotka-volterra model", kw_final)) %>%
#   mutate(kw_final = gsub("male competition", "male-male competition", kw_final)) %>%
#   mutate(kw_final = gsub("marine reserves", "marine reserve", kw_final)) %>%
#   mutate(kw_final = gsub("meta ecosystem", "metaecosystem", kw_final)) %>%
#   mutate(kw_final = gsub("non native", "nonnative", kw_final)) %>%
#   mutate(kw_final = gsub("ornstein uhlenbeck model", "ornstein-uhlenbeck model", kw_final)) %>%
#   mutate(kw_final = gsub("safe sites", "safe site", kw_final)) %>%
#   mutate(kw_final = gsub("slash and-burn agriculture", "slash and burn agriculture", kw_final)) %>%
#   mutate(kw_final = gsub("stage structured models", "stage-structured models", kw_final)) %>%
#   mutate(kw_final = gsub("time lag", "lag time", kw_final)) %>%
#   mutate(kw_final = gsub("tree fall gaps", "treefall gaps", kw_final)) %>%
#   mutate(kw_final = gsub("tropical soils", "tropical soil", kw_final)) %>%
#   mutate(kw_final = gsub("yasuní national park", "yasuni", kw_final)) %>%
#   mutate(kw_final = gsub("yasuni national park", "yasuni", kw_final)) %>%
#   mutate(kw_final = gsub("bison bison", "bison", kw_final)) %>%
#   mutate(kw_final = gsub("capture-mark-recapture", "capture mark-recapture", kw_final)) %>%
#   mutate(kw_final = gsub("carry over effect", "carryover effect", kw_final)) %>%
#   mutate(kw_final = gsub("competition colonization trade-off", "competition-colonization trade-off", kw_final)) %>%
#   mutate(kw_final = gsub("consumer resource model", "consumer-resource model", kw_final)) %>%
#   mutate(kw_final = gsub("cormack jolly-seber model", "cormack-jolly-seber model", kw_final)) %>%
#   mutate(kw_final = gsub("cost benefit analysis", "cost-benefit analysis", kw_final)) %>%
#   mutate(kw_final = gsub("critical transitions", "critical transition", kw_final)) %>%
#   mutate(kw_final = gsub("eco evolutionary feedbacks", "eco-evolutionary feedbacks", kw_final)) %>%
#   mutate(kw_final = gsub("food resources", "food resource", kw_final)) %>%
#   mutate(kw_final = gsub("forest types", "forest type", kw_final)) %>%
#   mutate(kw_final = gsub("honey bees", "honeybees", kw_final)) %>%
#   mutate(kw_final = gsub("host parasite dynamics", "host-parasite dynamics", kw_final)) %>%
#   mutate(kw_final = gsub("host pathogen dynamics", "host-pathogen dynamics", kw_final)) %>%
#   mutate(kw_final = gsub("host shifts", "host shift", kw_final)) %>%
#   mutate(kw_final = gsub("host-pathogen interactions", "host-pathogen interactions", kw_final)) %>%
#   mutate(kw_final = gsub("leaf miner", "leafminer", kw_final)) %>%
#   mutate(kw_final = gsub("mixed grass prairie", "mixed-grass prairie", kw_final)) %>%
#   mutate(kw_final = gsub("panamá", "panama", kw_final)) %>%
#   mutate(kw_final = gsub("predator prey system", "prey-predator system", kw_final)) %>%
#   mutate(kw_final = gsub("predator-prey dynamics", "predator-prey dynamics", kw_final)) %>%
#   mutate(kw_final = gsub("presence absence", "presence-absence", kw_final)) %>%
#   mutate(kw_final = gsub("páramo", "paramo", kw_final)) %>%
#   mutate(kw_final = gsub("q st", "qst", kw_final)) %>%
#   mutate(kw_final = gsub("selection experimental", "experimental selection", kw_final)) %>%
#   mutate(kw_final = gsub("size structured populations", "size-structured populations", kw_final)) %>%
#   mutate(kw_final = gsub("soil resources", "soil resource", kw_final)) %>%
#   mutate(kw_final = gsub("spatial capture-recapture", "spatial capture-recapture", kw_final)) %>%
#   mutate(kw_final = gsub("species abundance distributions", "species-abundance distributions", kw_final)) %>%
#   mutate(kw_final = gsub("species area", "species-area", kw_final)) %>%
#   mutate(kw_final = gsub("species area curve", "species-area curve", kw_final)) %>%
#   mutate(kw_final = gsub("species area curves", "species-area curve", kw_final)) %>%
#   mutate(kw_final = gsub("tree grass interactions", "tree-grass interactions", kw_final)) %>%
#   mutate(kw_final = gsub("tri trophic interaction", "tritrophic interaction", kw_final)) %>%
#   mutate(kw_final = gsub("tropical lowland rainforest", "lowland tropical rainforest", kw_final)) %>%
#   mutate(kw_final = gsub("above ground biomass", "aboveground biomass", kw_final)) %>%
#   mutate(kw_final = gsub("allocation trade offs", "allocation tradeoffs", kw_final)) %>%
#   mutate(kw_final = gsub("ancestral states", "ancestral state", kw_final)) %>%
#   mutate(kw_final = gsub("bayesian hierarchical models", "hierarchical bayesian models", kw_final)) %>%
#   mutate(kw_final = gsub("behavioral responses", "behavioral response", kw_final)) %>%
#   mutate(kw_final = gsub("benthic pelagic coupling", "benthic-pelagic coupling", kw_final)) %>%
#   mutate(kw_final = gsub("bio logging", "biologging", kw_final)) %>%
#   mutate(kw_final = gsub("birth weight", "birthweight", kw_final)) %>%
#   mutate(kw_final = gsub("consumer resource", "consumer-resource", kw_final)) %>%
#   mutate(kw_final = gsub("deer mice", "deermice", kw_final)) %>%
#   mutate(kw_final = gsub("el nino southern oscillation", "enso", kw_final)) %>%
#   mutate(kw_final = gsub("fast slow continuum", "fast-slow continuum", kw_final)) %>%
#   mutate(kw_final = gsub("forest fires", "forest fire", kw_final)) %>%
#   mutate(kw_final = gsub("geographic information system gis", "gis", kw_final)) %>%
#   mutate(kw_final = gsub("heterozygosity fitness correlation", "heterozygosity-fitness correlation", kw_final)) %>%
#   mutate(kw_final = gsub("host parasitoid", "host-parasitoid", kw_final)) %>%
#   mutate(kw_final = gsub("leaf functional traits", "functional leaf traits", kw_final)) %>%
#   mutate(kw_final = gsub("length weight relationship", "length-weight relationship", kw_final)) %>%
#   mutate(kw_final = gsub("parasite host interactions", "host-parasite interactions", kw_final)) %>%
#   mutate(kw_final = gsub("perú", "peru", kw_final)) %>%
#   mutate(kw_final = gsub("post fledging survival", "postfledging survival", kw_final)) %>%
#   mutate(kw_final = gsub("range expansions", "range expansion", kw_final)) %>%
#   mutate(kw_final = gsub("reproductive trade offs", "reproductive tradeoffs", kw_final)) %>%
#   mutate(kw_final = gsub("selection sexual", "sexual selection", kw_final)) %>%
#   mutate(kw_final = gsub("semi deciduous forest", "semideciduous forest", kw_final)) %>%
#   mutate(kw_final = gsub("spatio temporal variation", "spatio-temporal variation", kw_final)) %>%
#   mutate(kw_final = gsub("above belowground interactions", "above-belowground interactions", kw_final)) %>%
#   mutate(kw_final = gsub("allochthonous resources", "allochthonous resource", kw_final)) %>%
#   mutate(kw_final = gsub("alpine tree line", "alpine treeline", kw_final)) %>%
#   mutate(kw_final = gsub("ant-plant interactions", "ant-plant interactions", kw_final)) %>%
#   mutate(kw_final = gsub("aquatic terrestrial linkages", "terrestrial-aquatic linkages", kw_final)) %>%
#   mutate(kw_final = gsub("by catch", "bycatch", kw_final)) %>%
#   mutate(kw_final = gsub("carbon nutrient balance", "c:n balance", kw_final)) %>%
#   mutate(kw_final = gsub("co adaptation", "coadaptation", kw_final)) %>%
#   mutate(kw_final = gsub("co existence", "coexistence", kw_final)) %>%
#   mutate(kw_final = gsub("co limitation", "colimitation", kw_final)) %>%
#   mutate(kw_final = gsub("condition dependent dispersal", "condition-dependent dispersal", kw_final)) %>%
#   mutate(kw_final = gsub("epichloë", "epichloe", kw_final)) %>%
#   mutate(kw_final = gsub("evolution of co operation", "evolution of cooperation", kw_final)) %>%
#   mutate(kw_final = gsub("f  st", "fst", kw_final)) %>%
#   mutate(kw_final = gsub("g x e", "gxe", kw_final)) 
# 
# keywords <- keywords %>%
#   mutate(kw_final = gsub("genotype by-genotype interactions", "gxe", kw_final)) %>%
#   mutate(kw_final = gsub("genotyping by-sequencing", "genotyping by sequencing", kw_final)) %>%
#   mutate(kw_final = gsub("helpers at-the-nest", "helpers at the nest", kw_final)) %>%
#   mutate(kw_final = gsub("host parasitoid dynamics", "host-parasitoid dynamics", kw_final)) %>%
#   mutate(kw_final = gsub("host-parasite", "host-parasite", kw_final)) %>%
#   mutate(kw_final = gsub("hotspot", "hotspots", kw_final)) %>%
#   mutate(kw_final = gsub("life stages", "life stage", kw_final)) %>%
#   mutate(kw_final = gsub("long distance migration", "long-distance migration", kw_final)) %>%
#   mutate(kw_final = gsub("major transitions", "major transition", kw_final)) %>%
#   mutate(kw_final = gsub("mark release-recapture", "mark-release-recapture", kw_final)) %>%
#   mutate(kw_final = gsub("mark-recapture", "mark-recapture", kw_final)) %>%
#   mutate(kw_final = gsub("menidia menidia", "menidia", kw_final)) %>%
#   mutate(kw_final = gsub("p st", "pst", kw_final)) %>%
#   mutate(kw_final = gsub("pace of-life", "pace of life", kw_final)) %>%
#   mutate(kw_final = gsub("plant-animal interaction", "plant-animal interaction", kw_final)) %>%
#   mutate(kw_final = gsub("r", "r* theory", kw_final)) %>%
#   mutate(kw_final = gsub("r and k-selection", "r and k selection", kw_final)) %>%
#   mutate(kw_final = gsub("r*", "r* theory", kw_final)) %>%
#   mutate(kw_final = gsub("regional species pool", "regional species pools", kw_final)) %>%
#   mutate(kw_final = gsub("rutilus rutilus", "rutilus", kw_final)) %>%
#   mutate(kw_final = gsub("sea stars", "seastars", kw_final)) %>%
#   mutate(kw_final = gsub("semi arid ecosystem", "semiarid ecosystem", kw_final)) %>%
#   mutate(kw_final = gsub("space for-time substitution", "space-for-time substitution", kw_final)) %>%
#   mutate(kw_final = gsub("species assemblages", "species assemblage", kw_final)) %>%
#   mutate(kw_final = gsub("species ranges", "species range", kw_final)) %>%
#   mutate(kw_final = gsub("trait mediated interaction", "trait-mediated interaction", kw_final)) %>%
#   mutate(kw_final = gsub("tropical semi deciduous forest", "tropical semideciduous forest", kw_final)) %>%
#   mutate(kw_final = gsub("vulpes vulpes", "vulpes", kw_final)) %>%
#   mutate(kw_final = gsub("aboveground belowground linkages", "aboveground-belowground linkages", kw_final)) %>%
#   mutate(kw_final = gsub("agro ecosystem", "agroecosystem", kw_final)) %>%
#   mutate(kw_final = gsub("ant aphid mutualism", "ant-aphid mutualism", kw_final)) %>%
#   mutate(kw_final = gsub("carbon nutrient balance hypothesis", "carbon:nutrient balance hypothesis", kw_final)) %>%
#   mutate(kw_final = gsub("consumer-resource interactions", "consumer-resource interactions", kw_final)) %>%
#   mutate(kw_final = gsub("curaçao", "curacao", kw_final)) %>%
#   mutate(kw_final = gsub("developmental mode", "developmental model", kw_final)) %>%
#   mutate(kw_final = gsub("diversity-productivity relationship", "diversity-productivity relationship", kw_final)) %>%
#   mutate(kw_final = gsub("female competition", "female-female competition", kw_final)) %>%
#   mutate(kw_final = gsub("genotype by environment", "gxe", kw_final)) %>%
#   mutate(kw_final = gsub("gxe interaction", "gxe", kw_final)) %>%
#   mutate(kw_final = gsub("habitat shifts", "habitat shift", kw_final)) %>%
#   mutate(kw_final = gsub("hill robertson interference", "hill-robertson interference", kw_final)) %>%
#   mutate(kw_final = gsub("host-microbe interactions", "host-microbe interactions", kw_final)) %>%
#   mutate(kw_final = gsub("igapó", "igapo", kw_final)) %>%
#   mutate(kw_final = gsub("individual based simulation", "individual-based simulation", kw_final)) %>%
#   mutate(kw_final = gsub("insect plant relationships", "insect-plant relationships", kw_final)) %>%
#   mutate(kw_final = gsub("insect-plant interactions", "insect-plant interactions", kw_final)) %>%
#   mutate(kw_final = gsub("isolation by environment", "isolation-by-environment", kw_final)) %>%
#   mutate(kw_final = gsub("leafminers", "leaf miners", kw_final)) %>%
#   mutate(kw_final = gsub("limiting resources", "limiting resource", kw_final)) %>%
#   mutate(kw_final = gsub("lévy flight", "levy flight", kw_final)) %>%
#   mutate(kw_final = gsub("lévy walk", "levy walk", kw_final)) %>%
#   mutate(kw_final = gsub("meta community", "metacommunity", kw_final)) %>%
#   mutate(kw_final = gsub("micro organisms", "microorganisms", kw_final)) %>%
#   mutate(kw_final = gsub("multistate mark recapture", "multistate capture mark-recapture", kw_final)) %>%
#   mutate(kw_final = gsub("mus musculus musculus", "mus musculus", kw_final)) %>%
#   mutate(kw_final = gsub("non breeding season", "nonbreeding season", kw_final)) %>%
#   mutate(kw_final = gsub("non equilibrium", "nonequilibrium", kw_final)) %>%
#   mutate(kw_final = gsub("non indigenous", "nonindigenous", kw_final)) %>%
#   mutate(kw_final = gsub("non lethal effects", "nonlethal effects", kw_final)) %>%
#   mutate(kw_final = gsub("north east india", "northeast india", kw_final)) %>%
#   mutate(kw_final = gsub("parasite interactions", "parasite-parasite interactions", kw_final)) %>%
#   mutate(kw_final = gsub("plant plant interaction", "plant-plant interaction", kw_final)) %>%
#   mutate(kw_final = gsub("plant pollinator network", "plant-pollinator network", kw_final)) %>%
#   mutate(kw_final = gsub("plant soil interaction", "plant-soil interaction", kw_final)) %>%
#   mutate(kw_final = gsub("post copulatory", "post-copulatory", kw_final)) %>%
#   mutate(kw_final = gsub("predator prey theory", "predator-prey theory", kw_final)) %>%
#   mutate(kw_final = gsub("presence absence data", "presence-absence data", kw_final)) %>%
#   mutate(kw_final = gsub("pungitius pungitius", "pungitius", kw_final)) %>%
#   mutate(kw_final = gsub("robertson price identity", "robertson-price identity", kw_final)) %>%
#   mutate(kw_final = gsub("selection  natural", "natural selection", kw_final)) %>%
#   mutate(kw_final = gsub("selection group-kin", "group-kin selection  ", kw_final)) %>%
#   mutate(kw_final = gsub("semi arid savanna", "semiarid savanna", kw_final)) %>%
#   mutate(kw_final = gsub("slow fast continuum", "fast-slow continuum", kw_final)) %>%
#   mutate(kw_final = gsub("soil borne pathogens", "soilborne pathogens", kw_final)) %>%
#   mutate(kw_final = gsub("spatial capture recapture", "spatial capture-recapture", kw_final)) %>%
#   mutate(kw_final = gsub("spatially explicit capture recapture", "spatially explicit capture-recapture", kw_final)) %>%
#   mutate(kw_final = gsub("sub arctic", "subarctic", kw_final)) %>%
#   mutate(kw_final = gsub("successional stages", "successional stage", kw_final)) %>%
#   mutate(kw_final = gsub("temperature-size rule", "temperature size rule", kw_final)) %>%
#   mutate(kw_final = gsub("trivers willard hypothesis", "trivers-willard hypothesis", kw_final)) %>%
#   mutate(kw_final = gsub("tropical seasonal forest", "seasonal tropical forest", kw_final)) %>%
#   mutate(kw_final = gsub("visual signals", "visual signal", kw_final)) %>%
#   mutate(kw_final = gsub("waste water", "wastewater", kw_final)) %>%
#   mutate(kw_final = gsub("15 n isotope", "15n isotope", kw_final)) %>%
#   mutate(kw_final = gsub("abundance distribution", "distribution-abundance", kw_final)) %>%
#   mutate(kw_final = gsub("animal plant interactions", "plant-animal interactions", kw_final)) %>%
#   mutate(kw_final = gsub("arid lands", "aridlands", kw_final)) %>%
#   mutate(kw_final = gsub("bio indicator", "bioindicator", kw_final)) %>%
#   mutate(kw_final = gsub("boosted regression trees", "boosted regression tree", kw_final)) %>%
#   mutate(kw_final = gsub("cave fish", "cavefish", kw_final)) %>%
#   mutate(kw_final = gsub("chocó", "choco", kw_final)) %>%
#   mutate(kw_final = gsub("community phylogenetic structure", "phylogenetic community structure", kw_final)) %>%
#   mutate(kw_final = gsub("compensatory mutations", "compensatory mutation", kw_final)) %>%
#   mutate(kw_final = gsub("competition colonization", "competition-colonization", kw_final)) %>%
#   mutate(kw_final = gsub("condensed tannin", "condensed tannins", kw_final)) %>%
#   mutate(kw_final = gsub("cormack jolly-seber", "cormack-jolly-seber", kw_final)) %>%
#   mutate(kw_final = gsub("cost benefit", "cost-benefit", kw_final)) %>%
#   mutate(kw_final = gsub("counter gradient variation", "countergradient variation", kw_final)) %>%
#   mutate(kw_final = gsub("creosotebush", "creosote bush", kw_final)) %>%
#   mutate(kw_final = gsub("ctmax", "ct max", kw_final)) %>%
#   mutate(kw_final = gsub("data integration for population models special feature", "special feature: data integration for population models", kw_final)) %>%
#   mutate(kw_final = gsub("dispersal vectors", "dispersal vector", kw_final)) %>%
#   mutate(kw_final = gsub("diversity stability", "diversity-stability", kw_final)) %>%
#   mutate(kw_final = gsub("drip tips", "driptips", kw_final)) %>%
#   mutate(kw_final = gsub("el niño southern oscillation enso", "enso", kw_final)) %>%
#   mutate(kw_final = gsub("el niño-southern oscillation", "enso", kw_final)) %>%
#   mutate(kw_final = gsub("environmental stress models", "environmental stress model", kw_final)) %>%
#   mutate(kw_final = gsub("extra pair fertilization", "extrapair fertilization", kw_final)) %>%
#   mutate(kw_final = gsub("extreme climate", "climate extreme", kw_final)) %>%
#   mutate(kw_final = gsub("functional responses", "functional response", kw_final)) %>%
#   mutate(kw_final = gsub("g x e", "gxe", kw_final)) %>%
#   mutate(kw_final = gsub("galápagos islands", "galapagos islands", kw_final)) %>%
#   mutate(kw_final = gsub("gene for gene", "gene-for-gene", kw_final)) %>%
#   mutate(kw_final = gsub("genotype phenotype map", "genotype-phenotype map", kw_final)) %>%
#   mutate(kw_final = gsub("gorilla gorilla gorilla", "gorilla", kw_final)) %>%
#   mutate(kw_final = gsub("growth defense trade-off", "growth-defense tradeoff", kw_final)) %>%
#   mutate(kw_final = gsub("heat wave", "heatwave", kw_final)) %>%
#   mutate(kw_final = gsub("host parasite system", "host-parasite system", kw_final)) %>%
#   mutate(kw_final = gsub("host-parasitoid interactions", "host-parasitoid interactions", kw_final)) %>%
#   mutate(kw_final = gsub("human-elephant conflict", "human-elephant conflict", kw_final)) %>%
#   mutate(kw_final = gsub("hydrography", "hydrograph", kw_final)) %>%
#   mutate(kw_final = gsub("induced responses", "induced response", kw_final)) %>%
#   mutate(kw_final = gsub("kernel density", "density kernel", kw_final)) %>%
#   mutate(kw_final = gsub("life history tradeoffs", "life-history tradeoffs", kw_final)) %>%
#   mutate(kw_final = gsub("lychnis flos cuculi", "lychnis floscuculi", kw_final)) %>%
#   mutate(kw_final = gsub("macro evolution", "macroevolution", kw_final)) %>%
#   mutate(kw_final = gsub("male-male competition", "male-male competition", kw_final)) %>%
#   mutate(kw_final = gsub("mark recapture models", "capture mark-recapture models", kw_final)) %>%
#   mutate(kw_final = gsub("mark resight", "mark-resight", kw_final)) %>%
#   mutate(kw_final = gsub("marmota marmota", "marmota", kw_final)) %>%
#   mutate(kw_final = gsub("mercenaria mercenaria", "mercenaria", kw_final)) %>%
#   mutate(kw_final = gsub("meta regression", "metaregression", kw_final)) %>%
#   mutate(kw_final = gsub("moist tropical forest", "tropical moist forest", kw_final)) %>%
#   mutate(kw_final = gsub("multi host pathogens", "multihost pathogens", kw_final)) %>%
#   mutate(kw_final = gsub("non consumptive effect", "nonconsumptive effect", kw_final)) %>%
#   mutate(kw_final = gsub("non trophic interactions", "nontrophic interactions", kw_final)) %>%
#   mutate(kw_final = gsub("paraná river", "parana river", kw_final)) %>%
#   mutate(kw_final = gsub("plant water relations", "plant-water relations", kw_final)) %>%
#   mutate(kw_final = gsub("plant-insect interaction", "plant-insect interaction", kw_final)) %>%
#   mutate(kw_final = gsub("pre adaptation", "preadaptation", kw_final)) %>%
#   mutate(kw_final = gsub("prey predator interactions", "predator-prey interactions", kw_final)) %>%
#   mutate(kw_final = gsub("projection matrix model", "matrix projection model", kw_final)) %>%
#   mutate(kw_final = gsub("r:fr", "red:far red ratio", kw_final)) %>%
#   mutate(kw_final = gsub("reaction diffusion", "reaction-diffusion", kw_final)) %>%
#   mutate(kw_final = gsub("reserves", "reserve", kw_final)) %>%
#   mutate(kw_final = gsub("root shoot ratio", "root:shoot ratio", kw_final)) %>%
#   mutate(kw_final = gsub("root to-shoot ratio", "root:shoot ratios", kw_final)) %>%
#   mutate(kw_final = gsub("selection  sexual", "sexual selection", kw_final)) %>%
#   mutate(kw_final = gsub("semi arid environment", "semiarid environment", kw_final)) %>%
#   mutate(kw_final = gsub("semi arid region", "semiarid region", kw_final)) %>%
#   mutate(kw_final = gsub("semi natural grassland", "seminatural grassland", kw_final)) %>%
#   mutate(kw_final = gsub("slash and-burn", "slash-and-burn", kw_final)) %>%
#   mutate(kw_final = gsub("slave making ants", "slavemaking ants", kw_final)) %>%
#   mutate(kw_final = gsub("species abundances", "species abundance", kw_final)) %>%
#   mutate(kw_final = gsub("species-area relationship", "species-area relationship", kw_final)) %>%
#   mutate(kw_final = gsub("structural equation modeling sem", "structural equation modeling", kw_final)) %>%
#   mutate(kw_final = gsub("taï national park", "tai national park", kw_final)) %>%
#   mutate(kw_final = gsub("top down forces", "top-down forces", kw_final)) %>%
#   mutate(kw_final = gsub("transposons", "transposon", kw_final)) %>%
#   mutate(kw_final = gsub("tree falls", "treefalls", kw_final)) %>%
#   mutate(kw_final = gsub("tree grass coexistence", "tree-grass coexistence", kw_final)) %>%
#   mutate(kw_final = gsub("tropical lowland forest", "lowland tropical forest", kw_final)) %>%
#   mutate(kw_final = gsub("túngara frogs", "tungara frogs", kw_final)) %>%
#   mutate(kw_final = gsub("abiotic and biotic factors", "biotic and abiotic factors", kw_final)) %>%
#   mutate(kw_final = gsub("abundance occupancy relationship", "occupancy-abundance relationship", kw_final)) %>%
#   mutate(kw_final = gsub("behavioral indirect effects", "indirect behavioral effects", kw_final)) %>%
#   mutate(kw_final = gsub("bia≈Çowie≈ºa forest", "bia≈Çowieza forest", kw_final)) %>%
#   mutate(kw_final = gsub("big leaf mahogany", "bigleaf mahogany", kw_final)) %>%
#   mutate(kw_final = gsub("bio indicators", "bioindicators", kw_final)) %>%
#   mutate(kw_final = gsub("biodiversity ecosystem functioning relationship", "biodiversity-ecosystem functioning relationship", kw_final)) %>%
#   mutate(kw_final = gsub("biodiversity-ecosystem function", "biodiversity-ecosystem function", kw_final)) %>%
#   mutate(kw_final = gsub("buteo buteo", "buteo", kw_final)) %>%
#   mutate(kw_final = gsub("c 13", "13 c", kw_final)) %>%
#   mutate(kw_final = gsub("c 3", "c3", kw_final)) %>%
#   mutate(kw_final = gsub("c 4", "c4", kw_final)) %>%
#   mutate(kw_final = gsub("check list", "checklist", kw_final)) %>%
#   mutate(kw_final = gsub("clarkia xantiana ssp xantiana", "clarkia xantiana", kw_final)) %>%
#   mutate(kw_final = gsub("clear cutting", "clearcutting", kw_final)) %>%
#   mutate(kw_final = gsub("co  2", "co2", kw_final)) %>%
#   mutate(kw_final = gsub("co flowering", "coflowering", kw_final)) %>%
#   mutate(kw_final = gsub("cost-benefit", "cost-benefit", kw_final)) %>%
#   mutate(kw_final = gsub("density mediated indirect effect", "density-mediated indirect effect", kw_final)) %>%
#   mutate(kw_final = gsub("density mediated indirect interaction", "density-mediated indirect interactions", kw_final)) %>%
#   mutate(kw_final = gsub("die back", "dieback", kw_final)) %>%
#   mutate(kw_final = gsub("differential selection", "selection differential", kw_final)) %>%
#   mutate(kw_final = gsub("dispersal barriers", "dispersal barrier", kw_final)) %>%
#   mutate(kw_final = gsub("distribution and abundance", "distribution-abundance", kw_final)) %>%
#   mutate(kw_final = gsub("diversity productivity", "productivity-diversity", kw_final)) %>%
#   mutate(kw_final = gsub("diversity productivity relationship", "diversity-productivity relationship", kw_final)) %>%
#   mutate(kw_final = gsub("dobzhansky muller incompatibility", "dobzhansky-muller incompatibility", kw_final)) %>%
#   mutate(kw_final = gsub("doñana national park", "donana", kw_final)) %>%
#   mutate(kw_final = gsub("earlywood", "early wood", kw_final)) %>%
#   mutate(kw_final = gsub("eco epidemiology", "ecoepidemiology", kw_final)) %>%
#   mutate(kw_final = gsub("ecoevolutionary dynamics", "eco-evolutionary dynamics", kw_final)) %>%
#   mutate(kw_final = gsub("f  q st st", "q-f st", kw_final)) %>%
#   mutate(kw_final = gsub("fitness landscapes", "fitness landscape", kw_final)) %>%
#   mutate(kw_final = gsub("foodweb", "food web", kw_final)) %>%
#   mutate(kw_final = gsub("foraging-predation risk trade off", "foraging predation risk trade-off", kw_final)) %>%
#   mutate(kw_final = gsub("forest edges", "forest edge", kw_final)) %>%
#   mutate(kw_final = gsub("forest trees", "forest tree", kw_final)) %>%
#   mutate(kw_final = gsub("free air co  enrichment face 2", "face", kw_final)) %>%
#   mutate(kw_final = gsub("fresh water", "freshwater", kw_final)) %>%
#   mutate(kw_final = gsub("freshwater macrophytes", "freshwater macrophyte", kw_final)) %>%
#   mutate(kw_final = gsub("genotype by environment interactions", "gxe", kw_final)) %>%
#   mutate(kw_final = gsub("ground layer", "groundlayer", kw_final)) %>%
#   mutate(kw_final = gsub("habitat transitions", "habitat transition", kw_final)) %>%
#   mutate(kw_final = gsub("haplo diploidy", "haplodiploidy", kw_final)) %>%
#   mutate(kw_final = gsub("herbivore plant interactions", "plant-herbivore interactions", kw_final)) %>%
#   mutate(kw_final = gsub("host-parasitoid", "host-parasitoid", kw_final)) %>%
#   mutate(kw_final = gsub("human-wildlife interactions", "human-wildlife interactions", kw_final)) %>%
#   mutate(kw_final = gsub("iguana iguana", "iguana", kw_final)) %>%
#   mutate(kw_final = gsub("individual species area relationship", "individual species-area relationship", kw_final)) %>%
#   mutate(kw_final = gsub("inflorescences", "inflorescence", kw_final)) %>%
#   mutate(kw_final = gsub("isolation by-resistance", "isolation by resistance", kw_final)) %>%
#   mutate(kw_final = gsub("janzen connell effect", "janzen-connell effect", kw_final)) %>%
#   mutate(kw_final = gsub("keystone resources", "keystone resource", kw_final)) %>%
#   mutate(kw_final = gsub("landuse", "land-use", kw_final)) %>%
#   mutate(kw_final = gsub("large scale disturbance", "large-scale disturbance", kw_final)) %>%
#   mutate(kw_final = gsub("leaf cutter ants", "leafcutter ants", kw_final)) %>%
#   mutate(kw_final = gsub("live bearing", "livebearing", kw_final)) 
# 
# keywords <- keywords %>%
#   mutate(kw_final = gsub("lock and-key", "lock-and-key", kw_final)) %>%
#   mutate(kw_final = gsub("log normal", "lognormal", kw_final)) %>%
#   mutate(kw_final = gsub("luquillo experimental-forest", "luquillo experimental forest", kw_final)) %>%
#   mutate(kw_final = gsub("lynx lynx", "lynx", kw_final)) %>%
#   mutate(kw_final = gsub("maracá", "maraca", kw_final)) %>%
#   mutate(kw_final = gsub("match mismatch", "match-mismatch", kw_final)) %>%
#   mutate(kw_final = gsub("match mismatch hypothesis", "match-mismatch hypothesis", kw_final)) %>%
#   mutate(kw_final = gsub("maternal environmental effects", "environmental maternal effects", kw_final)) %>%
#   mutate(kw_final = gsub("mean d 2", "mean d2", kw_final)) %>%
#   mutate(kw_final = gsub("micro evolution", "microevolution", kw_final)) %>%
#   mutate(kw_final = gsub("montane tropical rainforest", "tropical montane rainforest", kw_final)) %>%
#   mutate(kw_final = gsub("monte carlo markov chain", "markov chain monte carlo", kw_final)) %>%
#   mutate(kw_final = gsub("multi level selection", "multilevel selection", kw_final)) %>%
#   mutate(kw_final = gsub("multimodal signals", "multimodal signal", kw_final)) %>%
#   mutate(kw_final = gsub("multiple spatial scales", "multiple spatial scale", kw_final)) %>%
#   mutate(kw_final = gsub("multivariate regression trees", "multivariate regression tree", kw_final)) %>%
#   mutate(kw_final = gsub("n  fixation 2", "nitrogen fixation", kw_final)) %>%
#   mutate(kw_final = gsub("non genetic inheritance", "nongenetic inheritance", kw_final)) %>%
#   mutate(kw_final = gsub("non indigenous species", "nonindigenous species", kw_final)) %>%
#   mutate(kw_final = gsub("non lethal predator effects", "nonlethal predator effects", kw_final)) %>%
#   mutate(kw_final = gsub("non metric multidimensional scaling", "nmds", kw_final)) %>%
#   mutate(kw_final = gsub("non random species loss", "nonrandom species loss", kw_final)) %>%
#   mutate(kw_final = gsub("over exploitation", "overexploitation", kw_final)) %>%
#   mutate(kw_final = gsub("p 2", "p2", kw_final)) %>%
#   mutate(kw_final = gsub("palm oil", "oil palm", kw_final)) %>%
#   mutate(kw_final = gsub("pan troglodytes troglodytes", "pan troglodytes", kw_final)) %>%
#   mutate(kw_final = gsub("parasite host dynamics", "host-parasite dynamics", kw_final)) %>%
#   mutate(kw_final = gsub("pea aphid acyrthosiphon pisum", "acyrthosiphon pisum, pea aphid", kw_final)) %>%
#   mutate(kw_final = gsub("performance trade off", "performance tradeoff", kw_final)) %>%
#   mutate(kw_final = gsub("phylogenetic comparative analyses", "comparative phylogenetic analyses", kw_final)) %>%
#   mutate(kw_final = gsub("pine oak forest", "oak pine forest", kw_final)) %>%
#   mutate(kw_final = gsub("pitvipers", "pit vipers", kw_final)) %>%
#   mutate(kw_final = gsub("plant animal mutualism", "plant-animal mutualism", kw_final)) %>%
#   mutate(kw_final = gsub("plant frugivore networks", "plant-frugivore networks", kw_final)) %>%
#   mutate(kw_final = gsub("plant fungal interactions", "plant-fungal interactions", kw_final)) %>%
#   mutate(kw_final = gsub("plant life span", "plant lifespan", kw_final)) %>%
#   mutate(kw_final = gsub("plant pollinator networks", "plant-pollinator networks", kw_final)) %>%
#   mutate(kw_final = gsub("postmating", "post-mating", kw_final)) %>%
#   mutate(kw_final = gsub("pre dispersal seed predator", "predispersal seed predator", kw_final)) %>%
#   mutate(kw_final = gsub("predator prey ratios", "predator:prey ratios", kw_final)) %>%
#   mutate(kw_final = gsub("productivity diversity relationship", "diversity-productivity relationship", kw_final)) %>%
#   mutate(kw_final = gsub("q  st", "qst", kw_final)) %>%
#   mutate(kw_final = gsub("r 0", "r0", kw_final)) %>%
#   mutate(kw_final = gsub("random amplified polymorphic dna", "rapd", kw_final)) %>%
#   mutate(kw_final = gsub("range extensions", "range extension", kw_final)) %>%
#   mutate(kw_final = gsub("rangifer tarandus caribou", "caribou rangifer tarandus", kw_final)) %>%
#   mutate(kw_final = gsub("red-far red", "red:far red", kw_final)) %>%
#   mutate(kw_final = gsub("respiration rates", "respiration rate", kw_final)) %>%
#   mutate(kw_final = gsub("rnaseq", "rna seq", kw_final)) %>%
#   mutate(kw_final = gsub("rock paper-scissors", "rock-paper-scissors", kw_final)) %>%
#   mutate(kw_final = gsub("root-shoot ratio", "root:shoot ratio", kw_final)) %>%
#   mutate(kw_final = gsub("sea grass", "seagrass", kw_final)) %>%
#   mutate(kw_final = gsub("secondary tropical forest", "tropical secondary forest", kw_final)) %>%
#   mutate(kw_final = gsub("seed bank persistence", "seedbank persistence", kw_final)) %>%
#   mutate(kw_final = gsub("selection artificial", "artificial selection", kw_final)) %>%
#   mutate(kw_final = gsub("selection—sexual", "sexual selection", kw_final)) %>%
#   mutate(kw_final = gsub("selective pressures", "selective pressure", kw_final)) %>%
#   mutate(kw_final = gsub("semi arid steppe", "semiarid steppe", kw_final)) %>%
#   mutate(kw_final = gsub("shrubsteppe", "shrub-steppe", kw_final)) %>%
#   mutate(kw_final = gsub("speciation with-gene-flow", "speciation with gene flow", kw_final)) %>%
#   mutate(kw_final = gsub("species accumulation curves", "species accumulation curve", kw_final)) %>%
#   mutate(kw_final = gsub("species co existence", "species coexistence", kw_final)) %>%
#   mutate(kw_final = gsub("species roles", "species role", kw_final)) %>%
#   mutate(kw_final = gsub("sub alpine forest", "subalpine forest", kw_final)) %>%
#   mutate(kw_final = gsub("sub saharan africa", "sub-saharan africa", kw_final)) %>%
#   mutate(kw_final = gsub("terrestrial aquatic linkages", "terrestrial-aquatic linkages", kw_final)) %>%
#   mutate(kw_final = gsub("tide pool", "tidepool", kw_final)) %>%
#   mutate(kw_final = gsub("tide pools", "tidepools", kw_final)) %>%
#   mutate(kw_final = gsub("timescales", "timescale", kw_final)) %>%
#   mutate(kw_final = gsub("tree frogs", "treefrogs", kw_final)) %>%
#   mutate(kw_final = gsub("tree hole", "treehole", kw_final)) %>%
#   mutate(kw_final = gsub("tropical humid forest", "humid tropical forest", kw_final)) %>%
#   mutate(kw_final = gsub("tropical vs temperate", "temperate vs tropical", kw_final)) %>%
#   mutate(kw_final = gsub("virulence transmission trade-off", "transmission virulence trade-off", kw_final)) %>%
#   mutate(kw_final = gsub("yucatán peninsula", "yucatan peninsula", kw_final)) %>%
#   mutate(kw_final = gsub("above ground biomass agb", "aboveground biomass agb", kw_final)) %>%
#   mutate(kw_final = gsub("aboveground net primary productivity anpp", "anpp", kw_final)) %>%
#   mutate(kw_final = gsub("age stage structure", "age-stage structure", kw_final)) %>%
#   mutate(kw_final = gsub("agro ecology", "agroecology", kw_final)) %>%
#   mutate(kw_final = gsub("animal signals", "animal signal", kw_final)) %>%
#   mutate(kw_final = gsub("anser caerulescens-caerulescens", "anser caerulescens", kw_final)) %>%
#   mutate(kw_final = gsub("anti herbivore defenses", "antiherbivore defenses", kw_final)) %>%
#   mutate(kw_final = gsub("anti oxidants", "antioxidants", kw_final)) %>%
#   mutate(kw_final = gsub("anti predator strategy", "antipredator strategy", kw_final)) %>%
#   mutate(kw_final = gsub("anti predatory behavior", "antipredatory behavior", kw_final)) %>%
#   mutate(kw_final = gsub("antlion", "ant lion", kw_final)) %>%
#   mutate(kw_final = gsub("antlions", "ant lions", kw_final)) %>%
#   mutate(kw_final = gsub("aphid ant mutualism", "ant-aphid mutualism", kw_final)) %>%
#   mutate(kw_final = gsub("aquatic-terrestrial linkages", "aquatic-terrestrial linkage", kw_final)) %>%
#   mutate(kw_final = gsub("atmospheric co 2", "atmospheric co2", kw_final)) %>%
#   mutate(kw_final = gsub("automated radio telemetry", "automated radiotelemetry", kw_final)) %>%
#   mutate(kw_final = gsub("baja california-sur", "baja california sur", kw_final)) %>%
#   mutate(kw_final = gsub("bat flies", "batflies", kw_final)) %>%
#   mutate(kw_final = gsub("bayesian hierarchical modeling", "hierarchical bayesian modeling", kw_final)) %>%
#   mutate(kw_final = gsub("benefits and costs", "costs and benefits", kw_final)) %>%
#   mutate(kw_final = gsub("bi parental care", "biparental care", kw_final)) %>%
#   mutate(kw_final = gsub("bi stability", "bistability", kw_final)) %>%
#   mutate(kw_final = gsub("biodiversity data set", "biodiversity dataset", kw_final)) %>%
#   mutate(kw_final = gsub("blackwater", "black water", kw_final)) %>%
#   mutate(kw_final = gsub("bud burst", "budburst", kw_final)) %>%
#   mutate(kw_final = gsub("bufo bufo", "bufo", kw_final)) %>%
#   mutate(kw_final = gsub("capture mark-recapture analysis", "capture-mark-recapture analysis", kw_final)) %>%
#   mutate(kw_final = gsub("carbon-nutrient balance", "c:n balance", kw_final)) %>%
#   mutate(kw_final = gsub("cestodes", "cestode", kw_final)) %>%
#   mutate(kw_final = gsub("ch 4", "ch4", kw_final)) %>%
#   mutate(kw_final = gsub("chaetognatha", "chaetognath", kw_final)) %>%
#   mutate(kw_final = gsub("chemical signals", "chemical signal", kw_final)) %>%
#   mutate(kw_final = gsub("chen caerulescens caerulescens", "chen caerulescens", kw_final)) %>%
#   mutate(kw_final = gsub("climate growth relationship", "climate-growth relationship", kw_final)) %>%
#   mutate(kw_final = gsub("co gradient variation", "cogradient variation", kw_final)) %>%
#   mutate(kw_final = gsub("co operation", "cooperation", kw_final)) %>%
#   mutate(kw_final = gsub("co operative breeding", "cooperative breeding", kw_final)) %>%
#   mutate(kw_final = gsub("co speciation", "cospeciation", kw_final)) %>%
#   mutate(kw_final = gsub("coefficient of additive genetic variation", "additive genetic coefficient of variation", kw_final)) %>%
#   mutate(kw_final = gsub("colonization competition", "competition-colonization", kw_final)) %>%
#   mutate(kw_final = gsub("colonization competition trade-off", "competition-colonization trade-off", kw_final)) %>%
#   mutate(kw_final = gsub("colonization extinction dynamics", "colonization-extinction dynamics", kw_final)) %>%
#   mutate(kw_final = gsub("color ornament", "ornament color", kw_final)) %>%
#   mutate(kw_final = gsub("community forest", "forest community", kw_final)) %>%
#   mutate(kw_final = gsub("compensatory responses", "compensatory response", kw_final)) %>%
#   mutate(kw_final = gsub("competitive responses", "competitive response", kw_final)) %>%
#   mutate(kw_final = gsub("consumer resource cycles", "consumer resource cycle", kw_final)) %>%
#   mutate(kw_final = gsub("consumer-resource interaction", "consumer-resource interactions", kw_final)) %>%
#   mutate(kw_final = gsub("crown of-thorns starfish", "crown-of-thorns starfish", kw_final)) %>%
#   mutate(kw_final = gsub("ctmin", "ct min", kw_final)) %>%
#   mutate(kw_final = gsub("data bases", "databases", kw_final)) %>%
#   mutate(kw_final = gsub("deep water", "deepwater", kw_final)) %>%
#   mutate(kw_final = gsub("deforestation rates", "deforestation rate", kw_final)) %>%
#   mutate(kw_final = gsub("dendro ecology", "dendroecology", kw_final)) %>%
#   mutate(kw_final = gsub("diet expansions", "diet expansion", kw_final)) %>%
#   mutate(kw_final = gsub("diet shifts", "diet shift", kw_final)) %>%
#   mutate(kw_final = gsub("dispersers", "disperser", kw_final)) %>%
#   mutate(kw_final = gsub("distribution abundance relationship", "distribution-abundance relationship", kw_final)) %>%
#   mutate(kw_final = gsub("diversity stability relationship", "diversity-stability relationship", kw_final)) %>%
#   mutate(kw_final = gsub("dose response curve", "dose-response curve", kw_final)) %>%
#   mutate(kw_final = gsub("doñana", "donana", kw_final)) %>%
#   mutate(kw_final = gsub("eco metabolomics", "ecometabolomics", kw_final)) %>%
#   mutate(kw_final = gsub("eco physiology", "ecophysiology", kw_final)) %>%
#   mutate(kw_final = gsub("ecoevolutionary feedbacks", "eco-evolutionary feedbacks", kw_final)) %>%
#   mutate(kw_final = gsub("elaphe obsoleta obsoleta", "elaphe obsoleta", kw_final)) %>%
#   mutate(kw_final = gsub("elevated co  2", "elevated co2", kw_final)) %>%
#   mutate(kw_final = gsub("evodevo", "evo-devo", kw_final)) %>%
#   mutate(kw_final = gsub("extinction colonization", "extinction-colonization", kw_final)) %>%
#   mutate(kw_final = gsub("extra floral nectaries", "extrafloral nectaries", kw_final)) %>%
#   mutate(kw_final = gsub("extra pair copulation", "extrapair copulation", kw_final)) %>%
#   mutate(kw_final = gsub("extra pair mating", "extra-pair mating", kw_final)) %>%
#   mutate(kw_final = gsub("extrapair mating", "extra-pair mating", kw_final)) %>%
#   mutate(kw_final = gsub("fast-slow continuum", "fast-slow continuum", kw_final)) %>%
#   mutate(kw_final = gsub("field work", "fieldwork", kw_final)) %>%
#   mutate(kw_final = gsub("fishfauna", "fish fauna", kw_final)) %>%
#   mutate(kw_final = gsub("fission fusion", "fission-fusion", kw_final)) %>%
#   mutate(kw_final = gsub("fitness difference", "fitness differences", kw_final)) %>%
#   mutate(kw_final = gsub("fitness surfaces", "fitness surface", kw_final)) %>%
#   mutate(kw_final = gsub("floral resource", "floral resources", kw_final)) %>%
#   mutate(kw_final = gsub("fluctuating resources", "fluctuating resource", kw_final)) %>%
#   mutate(kw_final = gsub("fluorescent dyes", "fluorescent dye", kw_final)) %>%
#   mutate(kw_final = gsub("forest reserves", "forest reserve", kw_final)) %>%
#   mutate(kw_final = gsub("frequencydependent selection", "frequency-dependent selection", kw_final)) %>%
#   mutate(kw_final = gsub("fresh water snail", "freshwater snail", kw_final)) %>%
#   mutate(kw_final = gsub("fresh water snails", "freshwater snails", kw_final)) %>%
#   mutate(kw_final = gsub("freshwater tropical fish", "tropical freshwater fish", kw_final)) %>%
#   mutate(kw_final = gsub("freshwater turtles", "freshwater turtle", kw_final)) %>%
#   mutate(kw_final = gsub("fuel wood", "fuelwood", kw_final)) %>%
#   mutate(kw_final = gsub("functional trade offs", "functional tradeoffs", kw_final)) %>%
#   mutate(kw_final = gsub("g st", "gst", kw_final)) %>%
#   mutate(kw_final = gsub("gallus gallus", "gallus", kw_final)) %>%
#   mutate(kw_final = gsub("gene by environment interaction", "gxe", kw_final)) %>%
#   mutate(kw_final = gsub("genetic resource", "genetic resources", kw_final)) %>%
#   mutate(kw_final = gsub("genotype x environment interaction", "gxe", kw_final)) %>%
#   mutate(kw_final = gsub("germline", "germ line", kw_final)) %>%
#   mutate(kw_final = gsub("gorilla beringei beringei", "gorilla beringei", kw_final)) %>%
#   mutate(kw_final = gsub("gorilla gorilla", "gorilla", kw_final)) %>%
#   mutate(kw_final = gsub("grass land", "grassland", kw_final)) %>%
#   mutate(kw_final = gsub("grass shrub competition", "grass-shrub competition", kw_final)) %>%
#   mutate(kw_final = gsub("green space", "greenspace", kw_final)) %>%
#   mutate(kw_final = gsub("grime tilman debate", "grime-tilman debate", kw_final)) %>%
#   mutate(kw_final = gsub("gulf of-mexico", "gulf of mexico", kw_final)) %>%
#   mutate(kw_final = gsub("guánica forest", "guanica forest", kw_final)) %>%
#   mutate(kw_final = gsub("heat waves", "heatwaves", kw_final)) %>%
#   mutate(kw_final = gsub("hemi epiphytes", "hemiepiphytes", kw_final)) %>%
#   mutate(kw_final = gsub("hitch hiking", "hitchhiking", kw_final)) %>%
#   mutate(kw_final = gsub("homegarden", "home garden", kw_final)) %>%
#   mutate(kw_final = gsub("host ", "host", kw_final)) %>%
#   mutate(kw_final = gsub("host microbe interactions", "host-microbe interactions", kw_final)) %>%
#   mutate(kw_final = gsub("host parasite co-evolution", "host-parasite coevolution", kw_final)) %>%
#   mutate(kw_final = gsub("host parasite ecology", "host-parasite ecology", kw_final)) %>%
#   mutate(kw_final = gsub("host parasitoid interaction", "host-parasitoid interaction", kw_final)) %>%
#   mutate(kw_final = gsub("host-parasite interactions", "host-parasite interactions", kw_final)) %>%
#   mutate(kw_final = gsub("hostparasite interaction", "host-parasite interaction", kw_final)) %>%
#   mutate(kw_final = gsub("host-pathogen dynamics", "host-pathogen dynamics", kw_final)) %>%
#   mutate(kw_final = gsub("human elephant conflict", "human-elephant conflict", kw_final)) %>%
#   mutate(kw_final = gsub("identity by-descent", "identity by descent", kw_final)) %>%
#   mutate(kw_final = gsub("iguazú national park", "iguazu national park", kw_final)) %>%
#   mutate(kw_final = gsub("induced plant defenses", "induced plant defense", kw_final)) %>%
#   mutate(kw_final = gsub("inter annual variation", "interannual variation", kw_final)) %>%
#   mutate(kw_final = gsub("inter patch movement", "interpatch movement", kw_final)) %>%
#   mutate(kw_final = gsub("intra guild competition", "intraguild competition", kw_final)) %>%
#   mutate(kw_final = gsub("intra guild predation", "intraguild predation", kw_final)) %>%
#   mutate(kw_final = gsub("intraspecific and interspecific competition", "interspecific and intraspecific competition", kw_final)) %>%
#   mutate(kw_final = gsub("isolation with-migration", "isolation with migration", kw_final)) %>%
#   mutate(kw_final = gsub("keystone plant resources", "keystone plant resource", kw_final)) %>%
#   mutate(kw_final = gsub("la selva-biological-station", "la selva", kw_final)) %>%
#   mutate(kw_final = gsub("lagopus lagopus", "lagopus", kw_final)) %>%
#   mutate(kw_final = gsub("land birds", "landbirds", kw_final)) %>%
#   mutate(kw_final = gsub("landsnails", "land snails", kw_final)) %>%
#   mutate(kw_final = gsub("late successional trees", "late successional tree", kw_final)) %>%
#   mutate(kw_final = gsub("leaffall", "leaf-fall", kw_final)) %>%
#   mutate(kw_final = gsub("levels of-selection", "levels of selection", kw_final)) %>%
#   mutate(kw_final = gsub("life history covariation", "life-history covariation", kw_final)) %>%
#   mutate(kw_final = gsub("life history tradeoff", "life-history trade-off", kw_final)) %>%
#   mutate(kw_final = gsub("life history transition", "life-history transition", kw_final)) %>%
#   mutate(kw_final = gsub("life history transitions", "life-history transition", kw_final)) %>%
#   mutate(kw_final = gsub("lifehistory evolution", "life-history evolution", kw_final)) %>%
#   mutate(kw_final = gsub("litter bags", "litterbags", kw_final)) %>%
#   mutate(kw_final = gsub("log normal distribution", "lognormal distribution", kw_final)) %>%
#   mutate(kw_final = gsub("longdistance dispersal", "long-distance dispersal", kw_final)) %>%
#   mutate(kw_final = gsub("lychnis flos-cuculi", "lychnis floscuculi", kw_final)) %>%
#   mutate(kw_final = gsub("mainland island", "island mainland", kw_final)) %>%
#   mutate(kw_final = gsub("marine sponges", "marine sponge", kw_final)) %>%
#   mutate(kw_final = gsub("mark release recapture", "mark-release-recapture", kw_final)) %>%
#   mutate(kw_final = gsub("martes martes", "martes", kw_final)) %>%
#   mutate(kw_final = gsub("maternal genetic effects", "genetic maternal effects", kw_final)) %>%
#   mutate(kw_final = gsub("mesocarnivores", "mesocarnivore", kw_final)) %>%
#   mutate(kw_final = gsub("mesoherbivores", "mesoherbivore", kw_final)) %>%
#   mutate(kw_final = gsub("meta communities", "metacommunities", kw_final)) %>%
#   mutate(kw_final = gsub("meta population", "metapopulation", kw_final)) %>%
#   mutate(kw_final = gsub("meta populations", "metapopulations", kw_final)) %>%
#   mutate(kw_final = gsub("mexican central pacific", "central mexican pacific", kw_final)) %>%
#   mutate(kw_final = gsub("micro arthropods", "microarthropods", kw_final)) %>%
#   mutate(kw_final = gsub("microct", "micro ct", kw_final)) %>%
#   mutate(kw_final = gsub("mito nuclear", "mitonuclear", kw_final)) %>%
#   mutate(kw_final = gsub("multi event model", "multievent model", kw_final)) %>%
#   mutate(kw_final = gsub("multi event models", "multievent models", kw_final)) %>%
#   mutate(kw_final = gsub("multi model inference", "multimodel inference", kw_final)) %>%
#   mutate(kw_final = gsub("multi scale ordination", "multiscale ordination", kw_final)) %>%
#   mutate(kw_final = gsub("multi species", "multispecies", kw_final)) %>%
#   mutate(kw_final = gsub("multi state models", "multistate models", kw_final)) %>%
#   mutate(kw_final = gsub("multi trophic", "multitrophic", kw_final)) %>%
#   mutate(kw_final = gsub("multievent capture recapture models", "multi event capture-recapture models", kw_final)) %>%
#   mutate(kw_final = gsub("multiple scales", "multiple scale", kw_final)) %>%
#   mutate(kw_final = gsub("myco heterotrophy", "mycoheterotrophy", kw_final)) %>%
#   mutate(kw_final = gsub("n 2 fixation", "nitrogen fixation", kw_final)) %>%
#   mutate(kw_final = gsub("n o 2", "no2", kw_final)) %>%
#   mutate(kw_final = gsub("n p ratio", "n:p ratio", kw_final)) %>%
#   mutate(kw_final = gsub("n-p ratio", "n:p ratio", kw_final)) %>%
#   mutate(kw_final = gsub("nestling growth rates", "nestling growth rate", kw_final)) %>%
#   mutate(kw_final = gsub("niche dimensions", "niche dimension", kw_final)) %>%
#   mutate(kw_final = gsub("non consumptive predator effects", "predator nonconsumptive effects", kw_final)) %>%
#   mutate(kw_final = gsub("non stationarity", "nonstationarity", kw_final)) %>%
#   mutate(kw_final = gsub("non stationary", "nonstationary", kw_final)) %>%
#   mutate(kw_final = gsub("non target effects", "nontarget effects", kw_final)) %>%
#   mutate(kw_final = gsub("non trophic interaction", "nontrophic interaction", kw_final)) %>%
#   mutate(kw_final = gsub("nonconsumptive predator effects", "predator nonconsumptive effects", kw_final)) %>%
#   mutate(kw_final = gsub("nucleotide sequences", "nucleotide sequence", kw_final)) %>%
#   mutate(kw_final = gsub("nutrient co limitation", "nutrient colimitation", kw_final)) %>%
#   mutate(kw_final = gsub("ornstein uhlenbeck models", "ornstein-uhlenbeck models", kw_final)) %>%
#   mutate(kw_final = gsub("over compensation", "overcompensation", kw_final)) %>%
#   mutate(kw_final = gsub("over dispersion", "overdispersion", kw_final)) %>%
#   mutate(kw_final = gsub("over yielding", "overyielding", kw_final)) %>%
#   mutate(kw_final = gsub("paloverde", "palo verde", kw_final)) %>%
#   mutate(kw_final = gsub("parasite host", "host-parasite", kw_final)) %>%
#   mutate(kw_final = gsub("parasitoid host", "host-parasitoid", kw_final)) %>%
#   mutate(kw_final = gsub("parasitoid host interactions", "host-parasitoid interactions", kw_final)) %>%
#   mutate(kw_final = gsub("partitioning diversity", "diversity partitioning", kw_final)) %>%
#   mutate(kw_final = gsub("pattern diversity", "diversity pattern", kw_final)) %>%
#   mutate(kw_final = gsub("pelagic benthic coupling", "benthic-pelagic coupling", kw_final)) %>%
#   mutate(kw_final = gsub("pennsylvanian", "pennsylvania", kw_final)) %>%
#   mutate(kw_final = gsub("phenol oxidase", "phenoloxidase", kw_final)) %>%
#   mutate(kw_final = gsub("phenotypic responses", "phenotypic response", kw_final)) %>%
#   mutate(kw_final = gsub("photoidentification", "photo identification", kw_final)) %>%
#   mutate(kw_final = gsub("plant ", "plant", kw_final)) %>%
#   mutate(kw_final = gsub("plant  animal interaction", "plant-animal interaction", kw_final)) %>%
#   mutate(kw_final = gsub("plant ant interactions", "ant-plant interactions", kw_final)) %>%
#   mutate(kw_final = gsub("plant insect relationships", "insect-plant relationships", kw_final)) %>%
#   mutate(kw_final = gsub("plant interaction", "plant-plant interaction", kw_final)) %>%
#   mutate(kw_final = gsub("plant microbe interaction", "plant-microbe interaction", kw_final)) %>%
#   mutate(kw_final = gsub("plant microbial interactions", "plant-microbial interactions", kw_final)) %>%
#   mutate(kw_final = gsub("plant plant communication", "plant communication", kw_final)) %>%
#   mutate(kw_final = gsub("plant responses", "plant response", kw_final)) %>%
#   mutate(kw_final = gsub("plant-plant interaction", "plant-plant interaction", kw_final)) %>%
#   mutate(kw_final = gsub("pollen ovule ratio", "pollen:ovule ratio", kw_final)) %>%
#   mutate(kw_final = gsub("poró", "poro", kw_final)) %>%
#   mutate(kw_final = gsub("post copulatory female choice", "postcopulatory female choice", kw_final)) %>%
#   mutate(kw_final = gsub("post mating isolation", "postmating isolation", kw_final)) %>%
#   mutate(kw_final = gsub("post mating sexual selection", "postmating sexual selection", kw_final)) %>%
#   mutate(kw_final = gsub("post settlement processes", "postsettlement processes", kw_final)) %>%
#   mutate(kw_final = gsub("post zygotic", "postzygotic", kw_final)) %>%
#   mutate(kw_final = gsub("post zygotic reproductive isolation", "postzygotic reproductive isolation", kw_final)) %>%
#   mutate(kw_final = gsub("pre mating isolation", "premating isolation", kw_final)) %>%
#   mutate(kw_final = gsub("pre zygotic isolation", "prezygotic isolation", kw_final)) %>%
#   mutate(kw_final = gsub("predatorprey interaction", "predator-prey interaction", kw_final)) %>%
#   mutate(kw_final = gsub("predatorprey interactions", "predator-prey interactions", kw_final)) %>%
#   mutate(kw_final = gsub("presence-absence", "presence-absence", kw_final)) %>%
#   mutate(kw_final = gsub("presence-absence data", "presence-absence data", kw_final)) %>%
#   mutate(kw_final = gsub("presence-absence", "presence-absence", kw_final)) %>%
#   mutate(kw_final = gsub("prey predator", "predator-prey", kw_final)) %>%
#   mutate(kw_final = gsub("producer scrounger", "producer-scrounger", kw_final)) %>%
#   mutate(kw_final = gsub("projection matrix models", "matrix projection models", kw_final)) %>%
#   mutate(kw_final = gsub("prédation", "predation", kw_final)) %>%
#   mutate(kw_final = gsub("pseudoarrhenotoky", "pseudo arrhenotoky", kw_final)) %>%
#   mutate(kw_final = gsub("q  f st st", "q-f st", kw_final)) %>%
#   mutate(kw_final = gsub("qst f-st comparison", "qst fst comparison", kw_final)) %>%
#   mutate(kw_final = gsub("québec", "quebec", kw_final)) %>%
#   mutate(kw_final = gsub("rain ", "rain", kw_final)) %>%
#   mutate(kw_final = gsub("rare and common species", "common and rare species", kw_final)) %>%
#   mutate(kw_final = gsub("recessive deleterious mutations", "deleterious recessive mutations", kw_final)) %>%
#   mutate(kw_final = gsub("regional vs local", "local vs regional", kw_final)) %>%
#   mutate(kw_final = gsub("reserve forest", "forest reserve", kw_final)) %>%
#   mutate(kw_final = gsub("residence times", "residence time", kw_final)) %>%
#   mutate(kw_final = gsub("resistance genes", "resistance gene", kw_final)) %>%
#   mutate(kw_final = gsub("resource consumer interactions", "consumer-resource interactions", kw_final)) %>%
#   mutate(kw_final = gsub("response curves", "response curve", kw_final)) %>%
#   mutate(kw_final = gsub("ricefields", "rice fields", kw_final)) %>%
#   mutate(kw_final = gsub("river ganga", "ganga river", kw_final)) %>%
#   mutate(kw_final = gsub("roadkills", "roadkill", kw_final)) %>%
#   mutate(kw_final = gsub("robertsonian translocations", "robertsonian translocation", kw_final)) %>%
#   mutate(kw_final = gsub("root : shoot ratio", "root:shoot ratio", kw_final)) %>%
#   mutate(kw_final = gsub("root: shoot ratio", "root:shoot ratio", kw_final)) %>%
#   mutate(kw_final = gsub("run off", "runoff", kw_final)) %>%
#   mutate(kw_final = gsub("s pecies richness", "species richness", kw_final)) %>%
#   mutate(kw_final = gsub("savanna forest boundary", "savanna-forest boundary", kw_final)) %>%
#   mutate(kw_final = gsub("sea grasses", "seagrasses", kw_final)) %>%
#   mutate(kw_final = gsub("seed reserves", "seed reserve", kw_final)) %>%
#   mutate(kw_final = gsub("semiarid shrublands", "semiarid shrubland", kw_final)) %>%
#   mutate(kw_final = gsub("semideciduous seasonal forest", "seasonal semideciduous forest", kw_final)) %>%
#   mutate(kw_final = gsub("sex allocation ratio", "sex ratio allocation", kw_final)) %>%
#   mutate(kw_final = gsub("shoot root ratio", "root:shoot ratio", kw_final)) %>%
#   mutate(kw_final = gsub("shrub steppe", "shrub-steppe", kw_final)) %>%
#   mutate(kw_final = gsub("size effect", "effect size", kw_final)) %>%
#   mutate(kw_final = gsub("sizestructured populations", "size-structured populations", kw_final)) %>%
#   mutate(kw_final = gsub("skipping reproduction", "reproduction skipping", kw_final)) %>%
#   mutate(kw_final = gsub("slash and burn", "slash-and-burn", kw_final)) %>%
#   mutate(kw_final = gsub("soft sediments", "soft sediment", kw_final)) %>%
#   mutate(kw_final = gsub("soil micro organisms", "soil microorganisms", kw_final)) %>%
#   mutate(kw_final = gsub("source-sink", "source-sink", kw_final)) %>%
#   mutate(kw_final = gsub("south eastern brazil", "southeastern brazil", kw_final)) %>%
#   mutate(kw_final = gsub("spatially explicit capture-recapture", "spatially explicit capture-recapture", kw_final)) %>%
#   mutate(kw_final = gsub("spatio temporal", "spatiotemporal", kw_final)) %>%
#   mutate(kw_final = gsub("spatio temporal dynamics", "spatiotemporal dynamics", kw_final)) %>%
#   mutate(kw_final = gsub("spatio temporal models", "spatiotemporal models", kw_final)) %>%
#   mutate(kw_final = gsub("spatio temporal pattern", "spatiotemporal pattern", kw_final)) %>%
#   mutate(kw_final = gsub("spatio temporal scales", "spatiotemporal scales", kw_final)) %>%
#   mutate(kw_final = gsub("species area relationship sar", "species-area relationships", kw_final)) %>%
#   mutate(kw_final = gsub("species energy relationship", "species-energy relationship", kw_final)) %>%
#   mutate(kw_final = gsub("spider web", "web spider", kw_final)) %>%
#   mutate(kw_final = gsub("ssurdna", "ssu rdna", kw_final)) %>%
#   mutate(kw_final = gsub("stable nitrogen isotope", "nitrogen stable isotope", kw_final)) %>%
#   mutate(kw_final = gsub("stable states", "stable state", kw_final)) %>%
#   mutate(kw_final = gsub("statespace model", "state-space model", kw_final)) %>%
#   mutate(kw_final = gsub("sub alpine", "subalpine", kw_final)) %>%
#   mutate(kw_final = gsub("sub tropical", "subtropical", kw_final)) %>%
#   mutate(kw_final = gsub("sub tropical forest", "subtropical forest", kw_final)) %>%
#   mutate(kw_final = gsub("sugar cane", "sugarcane", kw_final)) %>%
#   mutate(kw_final = gsub("super hosts", "superhosts", kw_final)) %>%
#   mutate(kw_final = gsub("survival and growth", "growth and survival", kw_final)) %>%
#   mutate(kw_final = gsub("sus scrofa scrofa", "sus scrofa", kw_final)) %>%
#   mutate(kw_final = gsub("t cell mediated immune response", "t cell-mediated immune response", kw_final)) %>%
#   mutate(kw_final = gsub("terrestrial aquatic linkage", "aquatic-terrestrial linkage", kw_final)) %>%
#   mutate(kw_final = gsub("tiger salamanders", "tiger salamander", kw_final)) %>%
#   mutate(kw_final = gsub("tit for-tat", "tit for tat", kw_final)) %>%
#   mutate(kw_final = gsub("top down factors", "top-down factors", kw_final)) %>%
#   mutate(kw_final = gsub("top down limitation", "top-down limitation", kw_final)) %>%
#   mutate(kw_final = gsub("trait environment relationships", "trait-environment relationships", kw_final)) %>%
#   mutate(kw_final = gsub("traitmediated interaction", "trait-mediated interaction", kw_final)) %>%
#   mutate(kw_final = gsub("trans generational", "transgenerational", kw_final)) %>%
#   mutate(kw_final = gsub("transitions", "transition", kw_final)) %>%
#   mutate(kw_final = gsub("tree fall", "treefall", kw_final)) %>%
#   mutate(kw_final = gsub("tree fall gap", "treefall gap", kw_final)) %>%
#   mutate(kw_final = gsub("tree frog", "treefrog", kw_final)) %>%
#   mutate(kw_final = gsub("tree line ecotone", "treeline ecotone", kw_final)) %>%
#   mutate(kw_final = gsub("tree-grass coexistence", "tree-grass coexistence", kw_final)) %>%
#   mutate(kw_final = gsub("tropical temperate comparison", "temperate vs tropical", kw_final)) %>%
#   mutate(kw_final = gsub("térraba sierpe", "sierpe térraba", kw_final)) %>%
#   mutate(kw_final = gsub("unobservable states", "unobservable state", kw_final)) %>%
#   mutate(kw_final = gsub("water flea", "waterflea", kw_final)) %>%
#   mutate(kw_final = gsub("water resources", "water resource", kw_final)) %>%
#   mutate(kw_final = gsub("water strider", "waterstrider", kw_final)) %>%
#   mutate(kw_final = gsub("water striders", "waterstrider", kw_final)) %>%
#   mutate(kw_final = gsub("wildlife vehicle collision", "wildlife-vehicle collision", kw_final)) %>%
#   mutate(kw_final = gsub("wildlife vehicle collisions", "wildlife-vehicle collisions", kw_final)) %>%
#   mutate(kw_final = gsub("wing-thorax ratio", "wing:thorax ratio", kw_final)) %>%
#   mutate(kw_final = gsub(" 18s rdna", "18s rdna", kw_final)) %>%
#   mutate(kw_final = gsub(" proteobacteria", "proteobacteria", kw_final)) %>%
#   mutate(kw_final = gsub("%plant population and community dynamics", "plant population and community dynamics", kw_final)) %>%
#   mutate(kw_final = gsub("15  n", "15n", kw_final)) %>%
#   mutate(kw_final = gsub("15 n stable isotope", "15n stable isotope", kw_final)) %>%
#   mutate(kw_final = gsub("16srdna", "16s rdna", kw_final)) %>%
#   mutate(kw_final = gsub("a triplex patula", "atriplex patula", kw_final)) %>%
#   mutate(kw_final = gsub("above  and belowground herbivory", "above and belowground herbivory", kw_final)) %>%
#   mutate(kw_final = gsub("above ground", "aboveground", kw_final)) %>%
#   mutate(kw_final = gsub("above ground-below ground interactions", "aboveground-belowground interactions", kw_final)) %>%
#   mutate(kw_final = gsub("aboveground net primary productivity  anpp", "anpp", kw_final)) %>%
#   mutate(kw_final = gsub("aboveground-belowground interactions", "aboveground-belowground interactions", kw_final)) %>%
#   mutate(kw_final = gsub("abundance mass scaling", "mass-abundance scaling", kw_final)) %>%
#   mutate(kw_final = gsub("abundance occupancy", "abundance-occupancy", kw_final)) %>%
#   mutate(kw_final = gsub("abundance: ant", "ant abundance", kw_final)) %>%
#   mutate(kw_final = gsub("acalymma vittatum", "acalymma vitattum", kw_final)) %>%
#   mutate(kw_final = gsub("acquisition conservation trade-off", "acquisition-conservation trade off", kw_final)) %>%
#   mutate(kw_final = gsub("acyrthosiphon pisum pea aphid", "acyrthosiphon pisum, pea aphid", kw_final)) %>%
#   mutate(kw_final = gsub("adalia bipunctata l", "adalia bipunctata", kw_final)) %>%
#   mutate(kw_final = gsub("adaptive suites", "adaptive suite", kw_final)) %>%
#   mutate(kw_final = gsub("africanized honey bees", "africanized honeybees", kw_final)) %>%
#   mutate(kw_final = gsub("agama agama", "agama", kw_final)) %>%
#   mutate(kw_final = gsub("age structures", "age structure", kw_final)) %>%
#   mutate(kw_final = gsub("agrofo restry", "agroforestry", kw_final)) %>%
#   mutate(kw_final = gsub("algal", "alga", kw_final)) %>%
#   mutate(kw_final = gsub("alternative stable community states", "alternative community stable state", kw_final)) %>%
#   mutate(kw_final = gsub("amazonía", "amazonia", kw_final)) %>%
#   mutate(kw_final = gsub("ameiva ameiva", "ameiva", kw_final)) %>%
#   mutate(kw_final = gsub("ancestor reconstructions", "ancestor reconstruction", kw_final)) %>%
#   mutate(kw_final = gsub("ancestral reconstructions", "ancestral reconstruction", kw_final)) %>%
#   mutate(kw_final = gsub("and predators", "predators and", kw_final)) %>%
#   mutate(kw_final = gsub("anguilla anguilla", "anguilla", kw_final)) %>%
#   mutate(kw_final = gsub("anser anser", "anser", kw_final)) %>%
#   mutate(kw_final = gsub("anti fouling", "antifouling", kw_final)) %>%
#   mutate(kw_final = gsub("anti fungal compounds", "antifungal compounds", kw_final)) %>%
#   mutate(kw_final = gsub("anti inflammatory agent", "antiinflammatory agent", kw_final)) %>%
#   mutate(kw_final = gsub("anti inflammmatory agent", "antiinflammatory agent", kw_final)) %>%
#   mutate(kw_final = gsub("anti predator", "antipredator", kw_final)) %>%
#   mutate(kw_final = gsub("anti predator adaptation", "antipredator adaptation", kw_final)) %>%
#   mutate(kw_final = gsub("anti predator defenses", "antipredator defenses", kw_final)) %>%
#   mutate(kw_final = gsub("anti predator response", "antipredator response", kw_final)) %>%
#   mutate(kw_final = gsub("anti predator responses", "antipredator response", kw_final)) %>%
#   mutate(kw_final = gsub("antipredator responses", "antipredator response", kw_final)) %>%
#   mutate(kw_final = gsub("antipredator-antipredator strategy", "antipredator strategy", kw_final)) %>%
#   mutate(kw_final = gsub("ant-aphid mutualism", "ant-aphid mutualism", kw_final)) %>%
#   mutate(kw_final = gsub("aphis fabae fabae", "aphis fabae", kw_final)) %>%
#   mutate(kw_final = gsub("aquatic terrestrial linkage", "aquatic-terrestrial linkage", kw_final)) %>%
#   mutate(kw_final = gsub("aquatic-terrestrial linkage", "aquatic-terrestrial linkage", kw_final)) %>%
#   mutate(kw_final = gsub("arctic tree line", "arctic treeline", kw_final)) %>%
#   mutate(kw_final = gsub("aspirochidotida", "aspidochirotida", kw_final)) %>%
#   mutate(kw_final = gsub("associational plant defense", "plant associational defense", kw_final)) %>%
#   mutate(kw_final = gsub("atta ceph alotes", "atta cephalotes", kw_final)) %>%
#   mutate(kw_final = gsub("attraction production", "attraction-production", kw_final)) %>%
#   mutate(kw_final = gsub("auto correlation", "autocorrelation", kw_final)) %>%
#   mutate(kw_final = gsub("avian inter specific brood parasitism", "avian interspecific brood parasitism", kw_final)) %>%
#   mutate(kw_final = gsub("bahía de la ascensión", "bahia de la ascensión", kw_final)) %>%
#   mutate(kw_final = gsub("base line", "baseline", kw_final)) %>%
#   mutate(kw_final = gsub("bateson dobzhansky-müller incompatibility", "bateson dobzhansky-muller incompatibility", kw_final)) %>%
#   mutate(kw_final = gsub("behavioral trade off", "behavioral tradeoff", kw_final)) %>%
#   mutate(kw_final = gsub("below ground", "belowground", kw_final)) %>%
#   mutate(kw_final = gsub("benefit cost analysis", "cost-benefit analysis", kw_final)) %>%
#   mutate(kw_final = gsub("benefits of-philopatry", "benefits of philopatry", kw_final)) %>%
#   mutate(kw_final = gsub("betula papyri fera", "betula papyrifera", kw_final)) %>%
#   mutate(kw_final = gsub("bill fish", "billfish", kw_final)) %>%
#   mutate(kw_final = gsub("bio concentration", "bioconcentration", kw_final)) %>%
#   mutate(kw_final = gsub("bio invasion", "bioinvasion", kw_final)) %>%
#   mutate(kw_final = gsub("biodiversity ecosystem function relationships", "biodiversity-ecosystem function relationships", kw_final)) %>%
#   mutate(kw_final = gsub("biodiversity ecosystem-function", "biodiversity-ecosystem function", kw_final)) %>%
#   mutate(kw_final = gsub("biodiversity hot spot", "biodiversity hotspots", kw_final)) %>%
#   mutate(kw_final = gsub("biodiversity productivity relationships", "biodiversity-productivity relationships", kw_final)) %>%
#   mutate(kw_final = gsub("biodiversity rapid assessment", "rapid biodiversity assessment", kw_final)) %>%
#   mutate(kw_final = gsub("biodiversity-ecosystem function relationships", "biodiversity-ecosystem function relationships", kw_final)) %>%
#   mutate(kw_final = gsub("biodiversity-ecosystem functioning relationship", "biodiversity-ecosystem functioning relationship", kw_final)) %>%
#   mutate(kw_final = gsub("biodiversity-ecosystem functioning", "biodiversity-ecosystem functioning", kw_final)) %>%
#   mutate(kw_final = gsub("biogeographic chocó region", "chocó biogeographic region", kw_final)) %>%
#   mutate(kw_final = gsub("biosphere reserves", "biosphere reserve", kw_final)) %>%
#   mutate(kw_final = gsub("bird watching", "birdwatching", kw_final)) %>%
#   mutate(kw_final = gsub("black legged tick", "blacklegged tick", kw_final)) %>%
#   mutate(kw_final = gsub("blow flies", "blowflies", kw_final)) %>%
#   mutate(kw_final = gsub("blow fly", "blowfly", kw_final)) %>%
#   mutate(kw_final = gsub("boots trapping", "bootstrapping", kw_final)) %>%
#   mutate(kw_final = gsub("brackishwater", "brackish water", kw_final)) %>%
#   mutate(kw_final = gsub("branta bernicla-nigricans", "branta bernicla nigricans", kw_final)) %>%
#   mutate(kw_final = gsub("brazilianatlanticforest", "brazilian atlantic forest", kw_final)) %>%
#   mutate(kw_final = gsub("bumble bee pollination", "bumblebee pollination", kw_final)) %>%
#   mutate(kw_final = gsub("bunch grass", "bunchgrass", kw_final)) %>%
#   mutate(kw_final = gsub("buried seeds", "buried seed", kw_final)) %>%
#   mutate(kw_final = gsub("bush meat", "bushmeat", kw_final)) %>%
#   mutate(kw_final = gsub("bycatch shrimp", "shrimp bycatch", kw_final)) %>%
#   mutate(kw_final = gsub("bêche de-mer", "beche de-mer", kw_final)) %>%
#   mutate(kw_final = gsub("c  3", "c3", kw_final)) %>%
#   mutate(kw_final = gsub("c  4", "c4", kw_final)) %>%
#   mutate(kw_final = gsub("c  grassland 4", "c4 grassland", kw_final)) %>%
#   mutate(kw_final = gsub("c  grassland4", "c4 grassland", kw_final)) %>%
#   mutate(kw_final = gsub("c allocation", "allocation", kw_final)) %>%
#   mutate(kw_final = gsub("c n ratio", "c:n ratio", kw_final)) %>%
#   mutate(kw_final = gsub("c n ratios", "c:n ratios", kw_final)) %>%
#   mutate(kw_final = gsub("c-n", "c:n", kw_final)) %>%
#   mutate(kw_final = gsub("c-n ratio", "c:n ratio", kw_final)) %>%
#   mutate(kw_final = gsub("cacao theobroma cacao", "theobroma cacao", kw_final)) %>%
#   mutate(kw_final = gsub("calidris canutus canutus", "calidris canutus", kw_final)) %>%
#   mutate(kw_final = gsub("calling songs", "calling song", kw_final)) %>%
#   mutate(kw_final = gsub("cannon ball", "cannonball", kw_final)) %>%
#   mutate(kw_final = gsub("canopy turn over times", "canopy turnover times", kw_final)) %>%
#   mutate(kw_final = gsub("capture mark recapture", "capture mark-recapture", kw_final)) %>%
#   mutate(kw_final = gsub("capture-mark-recapture analysis", "capture-mark-recapture analysis", kw_final)) %>%
#   mutate(kw_final = gsub("capture-mark-recapture models", "capture mark-recapture models", kw_final)) %>%
#   mutate(kw_final = gsub("carbohydrate reserves", "carbohydrate reserve", kw_final)) %>%
#   mutate(kw_final = gsub("carbon : nitrogen ratio", "c:n ratio", kw_final)) %>%
#   mutate(kw_final = gsub("carbon to phosphorus ratio", "c:p ratio", kw_final)) %>%
#   mutate(kw_final = gsub("carbon to-phosphorus ratio", "c:p ratio", kw_final)) %>%
#   mutate(kw_final = gsub("carbon to-phosphorus ratios", "c:p ratio", kw_final)) %>%
#   mutate(kw_final = gsub("carbon-nitrogen ratio", "c:n ratio", kw_final)) %>%
#   mutate(kw_final = gsub("carbon: nitrogen ratio", "c:n ratio", kw_final)) %>%
#   mutate(kw_final = gsub("carbon:nitrogen ratio", "c:n ratio", kw_final)) %>%
#   mutate(kw_final = gsub("caryoph yllene oxide", "caryophyllene oxide", kw_final)) %>%
#   mutate(kw_final = gsub("case  control", "case control", kw_final)) %>%
#   mutate(kw_final = gsub("character transitions", "character transition", kw_final)) %>%
#   mutate(kw_final = gsub("characteristic timescales", "characteristic timescale", kw_final)) %>%
#   mutate(kw_final = gsub("chlorophylla", "chlorophyll a", kw_final)) %>%
#   mutate(kw_final = gsub("choco biogeographic region", "chocó biogeographic region", kw_final)) %>%
#   mutate(kw_final = gsub("chocó biogeográfico", "choco biogeografico", kw_final)) %>%
#   mutate(kw_final = gsub("clarkia xantiana ssp", "clarkia xantiana", kw_final)) %>%
#   mutate(kw_final = gsub("clear cut", "clearcut", kw_final)) %>%
#   mutate(kw_final = gsub("cloudforest", "cloud forest", kw_final)) %>%
#   mutate(kw_final = gsub("co diversification", "codiversification", kw_final)) %>%
#   mutate(kw_final = gsub("co extinction", "coextinction", kw_final)) %>%
#   mutate(kw_final = gsub("co limitation nutrient", "nutrient colimitation", kw_final)) %>%
#   mutate(kw_final = gsub("co variation", "covariation", kw_final)) %>%
#   mutate(kw_final = gsub("coalescence times", "coalescence time", kw_final)) %>%
#   mutate(kw_final = gsub("coastal atlantic forest", "atlantic coastal forest", kw_final)) %>%
#   mutate(kw_final = gsub("coevolutionary hot spot", "coevolutionary hotspot", kw_final)) %>%
#   mutate(kw_final = gsub("coevolutionary hotspots", "coevolutionary hotspot", kw_final)) %>%
#   mutate(kw_final = gsub("coffee agro ecosystem", "coffee agroecosystem", kw_final)) %>%
#   mutate(kw_final = gsub("cold frontsoutbreaks", "cold fronts outbreaks", kw_final)) %>%
#   mutate(kw_final = gsub("community and population dynamics", "population and community dynamics", kw_final)) %>%
#   mutate(kw_final = gsub("community weighted-mean", "community weighted mean", kw_final)) %>%
#   mutate(kw_final = gsub("competition  colonization tradeoff", "competition-colonization tradeoff", kw_final)) %>%
#   mutate(kw_final = gsub("competition colonization tradeoff", "competition-colonization tradeoff", kw_final)) %>%
#   mutate(kw_final = gsub("competition-colonization trade off", "competition-colonization tradeoff", kw_final)) %>%
#   mutate(kw_final = gsub("competition-colonization trade off", "competition-colonization tradeoff", kw_final)) %>%
#   mutate(kw_final = gsub("competition-colonization tradeoff", "competition-colonization tradeoff", kw_final)) %>%
#   mutate(kw_final = gsub("compound specific stable-isotope analysis", "compound specific stable isotope analysis", kw_final)) %>%
#   mutate(kw_final = gsub("conservation in situ", "in situ conservation", kw_final)) %>%
#   mutate(kw_final = gsub("conservation species", "species conservation", kw_final)) %>%
#   mutate(kw_final = gsub("conspecific interactions", "conspecific interaction", kw_final)) %>%
#   mutate(kw_final = gsub("cor egonus", "coregonus", kw_final)) %>%
#   mutate(kw_final = gsub("cormack jolly seber", "cormack-jolly-seber", kw_final)) %>%
#   mutate(kw_final = gsub("cormack jolly seber model", "cormack-jolly-seber model", kw_final)) %>%
#   mutate(kw_final = gsub("cormack-jolly-seber", "cormack-jolly-seber", kw_final)) %>%
#   mutate(kw_final = gsub("corona virus", "coronavirus", kw_final)) %>%
#   mutate(kw_final = gsub("corridor dispersal", "dispersal corridor", kw_final)) %>%
#   mutate(kw_final = gsub("cost benefit ratio", "cost-benefit ratio", kw_final)) %>%
#   mutate(kw_final = gsub("cost of-reproduction", "cost of reproduction", kw_final)) %>%
#   mutate(kw_final = gsub("cost-benefit analysis", "cost-benefit analysis", kw_final)) %>%
#   mutate(kw_final = gsub("cost-benefit ratio", "cost-benefit ratio", kw_final)) %>%
#   mutate(kw_final = gsub("cost: benefit analysis", "cost-benefit analysis", kw_final)) %>%
#   mutate(kw_final = gsub("costofreproduction", "cost of reproduction", kw_final)) %>%
#   mutate(kw_final = gsub("cote divoire", "ivory coast", kw_final)) %>%
#   mutate(kw_final = gsub("coyote  canis latrans ", "coyote canis latrans", kw_final)) %>%
#   mutate(kw_final = gsub("cross breeding", "crossbreeding", kw_final)) %>%
#   mutate(kw_final = gsub("cross dating", "crossdating", kw_final)) %>%
#   mutate(kw_final = gsub("cross talk", "crosstalk", kw_final)) %>%
#   mutate(kw_final = gsub("crossfostering", "cross fostering", kw_final)) %>%
#   mutate(kw_final = gsub("crotaphytus collaris collaris", "crotaphytus collaris", kw_final)) %>%
#   mutate(kw_final = gsub("crown of thorns starfish", "crown-of-thorns starfish", kw_final)) %>%
#   mutate(kw_final = gsub("ct  max", "ct max", kw_final)) %>%
#   mutate(kw_final = gsub("cvalue", "c value", kw_final)) %>%
#   mutate(kw_final = gsub("cycling population", "population cycling", kw_final)) %>%
#   mutate(kw_final = gsub("cyto nuclear coevolution", "cytonuclear coevolution", kw_final)) %>%
#   mutate(kw_final = gsub("cyto nuclear epistasis", "cytonuclear epistasis", kw_final)) %>%
#   mutate(kw_final = gsub("c¬∑n ratio", "c:n ratio", kw_final)) %>%
#   mutate(kw_final = gsub("cô", "co", kw_final)) %>%
#   mutate(kw_final = gsub("data base", "database", kw_final)) %>%
#   mutate(kw_final = gsub("data model comparison", "model-data comparison", kw_final)) %>%
#   mutate(kw_final = gsub("deep water fishery", "deepwater fishery", kw_final)) %>%
#   mutate(kw_final = gsub("defense syndromes", "defense syndrome", kw_final)) %>%
#   mutate(kw_final = gsub("degradedlands", "degraded lands", kw_final)) %>%
#   mutate(kw_final = gsub("delay time", "time delay", kw_final)) %>%
#   mutate(kw_final = gsub("delayed responses", "delayed response", kw_final)) %>%
#   mutate(kw_final = gsub("deleterious recessives", "deleterious recessive", kw_final)) %>%
#   mutate(kw_final = gsub("delta c 13", "delta c13", kw_final)) %>%
#   mutate(kw_final = gsub("delta c-13", "delta c13", kw_final)) %>%
#   mutate(kw_final = gsub("delta n 15", "delta n15", kw_final)) %>%
#   mutate(kw_final = gsub("density estimates", "density estimate", kw_final)) %>%
#   mutate(kw_final = gsub("densitymediated indirect effect", "density-mediated indirect effect", kw_final)) %>%
#   mutate(kw_final = gsub("development model", "model development", kw_final)) %>%
#   mutate(kw_final = gsub("di nitrogen fixation", "dinitrogen fixation", kw_final)) %>%
#   mutate(kw_final = gsub("diamond back moth", "diamondback moth", kw_final)) %>%
#   mutate(kw_final = gsub("diasporas", "diaspora", kw_final)) %>%
#   mutate(kw_final = gsub("diffusion advection model", "advection diffusion model", kw_final)) %>%
#   mutate(kw_final = gsub("dik dik", "dik-dik", kw_final)) %>%
#   mutate(kw_final = gsub("dikdik", "dik-dik", kw_final)) %>%
#   mutate(kw_final = gsub("dispersal condition dependent", "condition-dependent dispersal", kw_final)) %>%
#   mutate(kw_final = gsub("dispersal corridors", "dispersal corridor", kw_final)) %>%
#   mutate(kw_final = gsub("dissassortative mating", "disassortative mating", kw_final)) %>%
#   mutate(kw_final = gsub("distance dispersal", "dispersal distance", kw_final)) %>%
#   mutate(kw_final = gsub("distribution abundance", "distribution-abundance", kw_final)) %>%
#   mutate(kw_final = gsub("distribution probability", "probability distribution", kw_final)) %>%
#   mutate(kw_final = gsub("distribution range size", "range size distribution", kw_final)) %>%
#   mutate(kw_final = gsub("disturbance and soil biodiversity", "soil biodiversity and disturbance", kw_final)) %>%
#   mutate(kw_final = gsub("divergence with-gene flow", "divergence with gene flow", kw_final)) %>%
#   mutate(kw_final = gsub("diversity disease relationship", "diversity-disease relationship", kw_final)) %>%
#   mutate(kw_final = gsub("diversity disturbance relationship", "diversity-disturbance relationship", kw_final)) %>%
#   mutate(kw_final = gsub("diversity estimators", "diversity estimator", kw_final)) %>%
#   mutate(kw_final = gsub("diversity hotspots", "diversity hotspot", kw_final)) %>%
#   mutate(kw_final = gsub("diversity invasibility", "diversity-invasibility", kw_final)) %>%
#   mutate(kw_final = gsub("diversity invasibility hypothesis", "diversity-invasibility hypothesis", kw_final)) %>%
#   mutate(kw_final = gsub("diversity-disturbance relationship", "diversity-disturbance relationship", kw_final)) %>%
#   mutate(kw_final = gsub("diversity-invasibility hypothesis", "diversity-invasibility hypothesis", kw_final)) %>%
#   mutate(kw_final = gsub("dn-d(s)", "dn-ds", kw_final)) %>%
#   mutate(kw_final = gsub("dna bar coding", "dna barcoding", kw_final)) %>%
#   mutate(kw_final = gsub("dna dna hybridization", "dna hybridization", kw_final)) %>%
#   mutate(kw_final = gsub("dolphin fish", "dolphinfish", kw_final)) %>%
#   mutate(kw_final = gsub("dominance diversity", "dominance-diversity", kw_final)) %>%
#   mutate(kw_final = gsub("dominance-diversity", "dominance-diversity", kw_final)) %>%
#   mutate(kw_final = gsub("donana national park", "donana", kw_final)) %>%
#   mutate(kw_final = gsub("donaña", "donana", kw_final)) %>%
#   mutate(kw_final = gsub("donãna national park", "donana", kw_final)) %>%
#   mutate(kw_final = gsub("dose response curves", "dose-response curve", kw_final)) %>%
#   mutate(kw_final = gsub("dry alpine meadow", "alpine dry meadow", kw_final)) %>%
#   mutate(kw_final = gsub("dynamic metapopulation", "metapopulation dynamics", kw_final)) %>%
#   mutate(kw_final = gsub("e laeis guineensis", "elaeis guineensis", kw_final)) %>%
#   mutate(kw_final = gsub("e scape", "escape", kw_final)) %>%
#   mutate(kw_final = gsub("east tropical pacific", "tropical east pacific", kw_final)) %>%
#   mutate(kw_final = gsub("eco genomics", "ecogenomics", kw_final)) %>%
#   mutate(kw_final = gsub("eco hydrology", "ecohydrology", kw_final)) %>%
#   mutate(kw_final = gsub("eco morphology", "ecomorphology", kw_final)) %>%
#   mutate(kw_final = gsub("eco phylogenetics", "ecophylogenetics", kw_final)) %>%
#   mutate(kw_final = gsub("eco regions", "ecoregions", kw_final)) %>%
#   mutate(kw_final = gsub("ecological transitions", "ecological transition", kw_final)) %>%
#   mutate(kw_final = gsub("ecoregión", "ecoregion", kw_final)) %>%
#   mutate(kw_final = gsub("ecosystem function and ecosystem services", "ecosystem function and services", kw_final)) %>%
#   mutate(kw_final = gsub("ecosystem function and structure", "ecosystem structure and function", kw_final)) %>%
#   mutate(kw_final = gsub("ecosystem responses", "ecosystem response", kw_final)) %>%
#   mutate(kw_final = gsub("eggload", "egg load", kw_final)) %>%
#   mutate(kw_final = gsub("el nino drought", "enso drought", kw_final)) %>%
#   mutate(kw_final = gsub("el ninõ southern oscillation", "enso", kw_final)) %>%
#   mutate(kw_final = gsub("el niño drought", "enso drought", kw_final)) %>%
#   mutate(kw_final = gsub("el niño-southern oscillation", "enso", kw_final)) %>%
#   mutate(kw_final = gsub("el niño-southern oscillation", "enso", kw_final)) %>%
#   mutate(kw_final = gsub("el nī no southern oscillation enso", "enso", kw_final)) %>%
#   mutate(kw_final = gsub("elevated [co ] 2", "elevated co2", kw_final)) %>%
#   mutate(kw_final = gsub("ende mism", "endemism", kw_final)) %>%
#   mutate(kw_final = gsub("enemyfree space", "enemy-free space", kw_final)) %>%
#   mutate(kw_final = gsub("enemyrelease hypothesis", "enemy release hypothesis", kw_final)) %>%
#   mutate(kw_final = gsub("energetic trade off", "energetic tradeoff", kw_final)) %>%
#   mutate(kw_final = gsub("ensifera ensifera", "ensifera", kw_final)) %>%
#   mutate(kw_final = gsub("environmental education &", "environmental education", kw_final)) %>%
#   mutate(kw_final = gsub("environmental responses", "environmental response", kw_final)) %>%
#   mutate(kw_final = gsub("espinhaço range", "espinhaco range", kw_final)) %>%
#   mutate(kw_final = gsub("estimates", "estimate", kw_final)) %>%
#   mutate(kw_final = gsub("eulemur fulvus fulvus", "eulemur fulvus", kw_final)) %>%
#   mutate(kw_final = gsub("everglades florida", "florida everglades", kw_final)) %>%
#   mutate(kw_final = gsub("evergreen broad leaved forest", "evergreen broadleaved forest", kw_final)) %>%
#   mutate(kw_final = gsub("evergreen lowland rainforest", "lowland evergreen rainforest", kw_final)) %>%
#   mutate(kw_final = gsub("evolution and ecology", "ecology and evolution", kw_final)) %>%
#   mutate(kw_final = gsub("evolutionarily stable strategie", "evolutionarily stable strategies", kw_final)) %>%
#   mutate(kw_final = gsub("evolutionary responses", "evolutionary response", kw_final)) %>%
#   mutate(kw_final = gsub("ex tinction", "extinction", kw_final)) %>%
#   mutate(kw_final = gsub("exap tation", "exaptation", kw_final)) %>%
#   mutate(kw_final = gsub("exotic annual grasses", "annual exotic grasses", kw_final)) %>%
#   mutate(kw_final = gsub("exotic invasive plants", "invasive exotic plants", kw_final)) %>%
#   mutate(kw_final = gsub("exploration exploitation trade-off", "exploration-exploitation trade-off", kw_final)) %>%
#   mutate(kw_final = gsub("exploration-exploitation trade off", "exploration-exploitation trade-off", kw_final)) %>%
#   mutate(kw_final = gsub("extinction-colonization", "extinction-colonization", kw_final)) %>%
#   mutate(kw_final = gsub("extra floral nectar", "extrafloral nectar", kw_final)) %>%
#   mutate(kw_final = gsub("extra floral nectary", "extrafloral nectary", kw_final)) %>%
#   mutate(kw_final = gsub("eye span", "eyespan", kw_final)) %>%
#   mutate(kw_final = gsub("f  statistics", "f statistics", kw_final)) %>%
#   mutate(kw_final = gsub("face free air co  enrichment 2", "face", kw_final)) %>%
#   mutate(kw_final = gsub("factors controlling", "controlling factors", kw_final)) %>%
#   mutate(kw_final = gsub("far red ratio", "red:far red ratio", kw_final)) %>%
#   mutate(kw_final = gsub("farm yard manure", "farmyard manure", kw_final)) %>%
#   mutate(kw_final = gsub("fat body", "body fat", kw_final)) %>%
#   mutate(kw_final = gsub("female female competition", "female-female competition", kw_final)) %>%
#   mutate(kw_final = gsub("female post mating response", "post-mating female response", kw_final)) %>%
#   mutate(kw_final = gsub("fi re ant", "fire ant", kw_final)) %>%
#   mutate(kw_final = gsub("fire herbivore interactions", "fire-herbivore interactions", kw_final)) %>%
#   mutate(kw_final = gsub("fl uvial erosion", "fluvial erosion", kw_final)) %>%
#   mutate(kw_final = gsub("flood plain", "floodplain", kw_final)) %>%
#   mutate(kw_final = gsub("flood plain lakes", "floodplain lakes", kw_final)) %>%
#   mutate(kw_final = gsub("flood plains", "floodplains", kw_final)) %>%
#   mutate(kw_final = gsub("fluctuatingselection", "fluctuating selection", kw_final)) %>%
#   mutate(kw_final = gsub("forest complex", "complex forest", kw_final)) %>%
#   mutate(kw_final = gsub("forest savanna boundary", "savanna-forest boundary", kw_final)) %>%
#   mutate(kw_final = gsub("forest specialists", "forest specialist", kw_final)) %>%
#   mutate(kw_final = gsub("forest uses", "forest use", kw_final)) %>%
#   mutate(kw_final = gsub("forestland", "forest land", kw_final)) %>%
#   mutate(kw_final = gsub("form function relationship", "form-function relationship", kw_final)) %>%
#   mutate(kw_final = gsub("formfunction relationship", "form-function relationship", kw_final)) %>%
#   mutate(kw_final = gsub("fourthcorner problem", "fourth corner problem", kw_final)) %>%
#   mutate(kw_final = gsub("frequency ", "frequency", kw_final)) %>%
#   mutate(kw_final = gsub("fresh water ecology", "freshwater ecology", kw_final)) %>%
#   mutate(kw_final = gsub("fresh water fishes", "freshwater fishes", kw_final)) %>%
#   mutate(kw_final = gsub("fresh water shrimps", "freshwater shrimps", kw_final)) %>%
#   mutate(kw_final = gsub("freshwater lakes", "freshwater lake", kw_final)) %>%
#   mutate(kw_final = gsub("fruitbats", "fruit bats", kw_final)) %>%
#   mutate(kw_final = gsub("fruitfly", "fruit fly", kw_final)) %>%
#   mutate(kw_final = gsub("fruitset", "fruit set", kw_final)) %>%
#   mutate(kw_final = gsub("functional plant group", "plant functional group", kw_final)) %>%
#   mutate(kw_final = gsub("g eospiza", "geospiza", kw_final)) %>%
#   mutate(kw_final = gsub("g max", "gmax", kw_final)) %>%
#   mutate(kw_final = gsub("g x e interaction", "gxe", kw_final)) %>%
#   mutate(kw_final = gsub("gall insect", "insect gall", kw_final)) %>%
#   mutate(kw_final = gsub("gallmaker", "gall maker", kw_final)) %>%
#   mutate(kw_final = gsub("gallwasp", "gall wasp", kw_final)) %>%
#   mutate(kw_final = gsub("gc ms analysis", "gc-ms analysis", kw_final)) %>%
#   mutate(kw_final = gsub("gc-ms analysis", "gc-ms analysis", kw_final)) %>%
#   mutate(kw_final = gsub("gekkonida e", "gekkonidae", kw_final)) %>%
#   mutate(kw_final = gsub("gene by-environment interaction", "gxe", kw_final)) %>%
#   mutate(kw_final = gsub("gene culture co-evolution", "gene culture coevolution", kw_final)) %>%
#   mutate(kw_final = gsub("geneculture coevolution", "gene culture coevolution", kw_final)) %>%
#   mutate(kw_final = gsub("generalist specialist trade-offs", "generalist-specialist trade-offs", kw_final)) %>%
#   mutate(kw_final = gsub("genetic structure and diversity", "genetic diversity and structure", kw_final)) %>%
#   mutate(kw_final = gsub("genome wide association studies", "gwas", kw_final)) %>%
#   mutate(kw_final = gsub("genomewide association studies", "gwas", kw_final)) %>%
#   mutate(kw_final = gsub("genotype by genotype by environment", "gxe", kw_final)) %>%
#   mutate(kw_final = gsub("genotype by genotype interactions", "gxe", kw_final)) %>%
#   mutate(kw_final = gsub("genotype environment associations", "gxe", kw_final)) %>%
#   mutate(kw_final = gsub("genotype environment correlation", "genotype-environment correlation", kw_final)) %>%
#   mutate(kw_final = gsub("genotypephenotype map", "genotype-phenotype map", kw_final)) %>%
#   mutate(kw_final = gsub("genotype x environment interaction", "gxe", kw_final)) %>%
#   mutate(kw_final = gsub("genotype-environment associations", "gxe", kw_final)) %>%
#   mutate(kw_final = gsub("gis geographic information system", "gis", kw_final)) %>%
#   mutate(kw_final = gsub("giving up-density", "giving-up density", kw_final)) %>%
#   mutate(kw_final = gsub("glossophaga commissarisi", "glosophaga commissarisi", kw_final)) %>%
#   mutate(kw_final = gsub("gold fish", "goldfish", kw_final)) %>%
#   mutate(kw_final = gsub("golfo de-california", "golfo de california", kw_final)) %>%
#   mutate(kw_final = gsub("golfo de-nicoya", "golfo de nicoya", kw_final)) %>%
#   mutate(kw_final = gsub("gonado somatic index", "gonado-somatic index", kw_final)) %>%
#   mutate(kw_final = gsub("goodness of-fit", "goodness of fit", kw_final)) %>%
#   mutate(kw_final = gsub("grass tree coexistence", "tree-grass coexistence", kw_final)) %>%
#   mutate(kw_final = gsub("grasses and forbs", "forbs and grasses", kw_final)) %>%
#   mutate(kw_final = gsub("grass-fire cycle", "grass-fire cycle", kw_final)) %>%
#   mutate(kw_final = gsub("green beard", "greenbeard", kw_final)) %>%
#   mutate(kw_final = gsub("green beards", "greenbeard", kw_final)) %>%
#   mutate(kw_final = gsub("ground water", "groundwater", kw_final)) %>%
#   mutate(kw_final = gsub("growth climate relationship", "climate-growth relationship", kw_final)) %>%
#   mutate(kw_final = gsub("growth climate responses", "climate growth responses", kw_final)) %>%
#   mutate(kw_final = gsub("growth defense trade off", "growth-defense tradeoff", kw_final)) %>%
#   mutate(kw_final = gsub("growth differentiation-balance hypothesis", "growth differentiation balance hypothesis", kw_final)) %>%
#   mutate(kw_final = gsub("growth responses", "growth response", kw_final)) %>%
#   mutate(kw_final = gsub("growth-defense trade off", "growth-defense tradeoff", kw_final)) %>%
#   mutate(kw_final = gsub("gulf california", "california gulf", kw_final)) %>%
#   mutate(kw_final = gsub("g x e", "gxe", kw_final)) %>%
#   mutate(kw_final = gsub("g(st)", "gst", kw_final)) %>%
#   mutate(kw_final = gsub("habitat fragmentation and loss", "habitat loss and fragmentation", kw_final)) %>%
#   mutate(kw_final = gsub("habitat matrix", "matrix habitat", kw_final)) %>%
#   mutate(kw_final = gsub("habitat specialists", "habitat specialist", kw_final)) %>%
#   mutate(kw_final = gsub("hair root", "root hair", kw_final)) %>%
#   mutate(kw_final = gsub("haplo diploid", "haplodiploid", kw_final)) %>%
#   mutate(kw_final = gsub("haplo diploid sex determination", "haplodiploid sex determination", kw_final)) %>%
#   mutate(kw_final = gsub("hawai ªi", "hawaii", kw_final)) %>%
#   mutate(kw_final = gsub("hawk moths", "hawkmoths", kw_final)) %>%
#   mutate(kw_final = gsub("height vegetation", "vegetation height", kw_final)) %>%
#   mutate(kw_final = gsub("hemi epiphyte", "hemiepiphyte", kw_final)) %>%
#   mutate(kw_final = gsub("herbchronology", "herb chronology", kw_final)) %>%
#   mutate(kw_final = gsub("herbivore herbivore interactions", "herbivore-herbivore interactions", kw_final)) %>%
#   mutate(kw_final = gsub("herbivore induced plant response", "herbivore-induced plant response", kw_final)) %>%
#   mutate(kw_final = gsub("herbivore induced plant responses", "herbivore-induced plant response", kw_final)) %>%
#   mutate(kw_final = gsub("herbivore interactions", "herbivore-herbivore interactions", kw_final)) %>%
#   mutate(kw_final = gsub("herbivore plant dynamics", "plant-herbivore dynamics", kw_final)) %>%
#   mutate(kw_final = gsub("herbivore plant interaction", "plant-herbivore interaction", kw_final)) %>%
#   mutate(kw_final = gsub("heterospecificpollen", "heterospecific pollen", kw_final)) %>%
#   mutate(kw_final = gsub("heterozygosity-fitness correlation", "heterozygosity-fitness correlation", kw_final)) %>%
#   mutate(kw_final = gsub("hind limb", "hindlimb", kw_final)) %>%
#   mutate(kw_final = gsub("hluhluwe-imfolozi park", "hluhluwe imfolozi park", kw_final)) %>%
#   mutate(kw_final = gsub("hollow tree", "tree hollow", kw_final)) %>%
#   mutate(kw_final = gsub("holo epiphyte", "holoepiphyte", kw_final)) %>%
#   mutate(kw_final = gsub("home gardens", "homegardens", kw_final)) %>%
#   mutate(kw_final = gsub("hop lias malabaricus", "hoplias malabaricus", kw_final)) %>%
#   mutate(kw_final = gsub("host  parasite interaction", "host-parasite interaction", kw_final)) %>%
#   mutate(kw_final = gsub("host parasite relationship", "host-parasite relationship", kw_final)) %>%
#   mutate(kw_final = gsub("host pathogen evolution", "host-pathogen evolution", kw_final)) %>%
#   mutate(kw_final = gsub("host responses", "host response", kw_final)) %>%
#   mutate(kw_final = gsub("host-parasite", "host-parasite", kw_final)) %>%
#   mutate(kw_final = gsub("hostfeeding", "host-feeding", kw_final)) %>%
#   mutate(kw_final = gsub("hostparasite", "host-parasite", kw_final)) %>%
#   mutate(kw_final = gsub("hostparasite system", "host-parasite system", kw_final)) %>%
#   mutate(kw_final = gsub("host-parasite system", "host-parasite system", kw_final)) %>%
#   mutate(kw_final = gsub("hot spot", "hotspots", kw_final)) %>%
#   mutate(kw_final = gsub("hot spots", "hotspots", kw_final)) %>%
#   mutate(kw_final = gsub("huisman olff-fresco models", "huisman-olff-fresco models", kw_final)) %>%
#   mutate(kw_final = gsub("human  elephant conflict", "human-elephant conflict", kw_final)) %>%
#   mutate(kw_final = gsub("human pressures", "human pressure", kw_final)) %>%
#   mutate(kw_final = gsub("human wildlife interactions", "human-wildlife interactions", kw_final)) %>%
#   mutate(kw_final = gsub("hyallela azteca", "hyalella azteca", kw_final)) %>%
#   mutate(kw_final = gsub("hybrid female sterility", "female hybrid sterility", kw_final)) %>%
#   mutate(kw_final = gsub("hybridiza tion", "hybridization", kw_final)) %>%
#   mutate(kw_final = gsub("hyper spectral remote sensing", "hyperspectral remote sensing", kw_final)) %>%
#   mutate(kw_final = gsub("i  mates", "i mates", kw_final)) %>%
#   mutate(kw_final = gsub("immuno competence handicap", "immunocompetence handicap", kw_final)) %>%
#   mutate(kw_final = gsub("immuno suppression", "immunosuppression", kw_final)) %>%
#   mutate(kw_final = gsub("immunoelectronmicroscopy", "immunoelectron microscopy", kw_final)) %>%
#   mutate(kw_final = gsub("income and capital breeding", "capital and income breeding", kw_final)) %>%
#   mutate(kw_final = gsub("increment growth", "growth increment", kw_final)) %>%
#   mutate(kw_final = gsub("index selection", "selection index", kw_final)) %>%
#   mutate(kw_final = gsub("indirect density mediated interactions", "density-mediated indirect interactions", kw_final)) %>%
#   mutate(kw_final = gsub("individual based sim-ulation", "individual-based simulation", kw_final)) %>%
#   mutate(kw_final = gsub("individual by-environment inter-action", "individual by environment interaction", kw_final)) %>%
#   mutate(kw_final = gsub("individualbased model", "individual-based model", kw_final)) %>%
#   mutate(kw_final = gsub("individualbased simulation", "individual-based simulation", kw_final)) %>%
#   mutate(kw_final = gsub("ingestion rates", "ingestion rate", kw_final)) %>%
#   mutate(kw_final = gsub("insect interactions", "insect interaction", kw_final)) %>%
#   mutate(kw_final = gsub("insect pathogens", "insect pathogen", kw_final)) %>%
#   mutate(kw_final = gsub("insect plant association", "insect-plant association", kw_final)) %>%
#   mutate(kw_final = gsub("insect plant associations", "insect-plant association", kw_final)) %>%
#   mutate(kw_final = gsub("insect-plant interaction", "plant-insect interaction", kw_final)) %>%
#   mutate(kw_final = gsub("insectplant interaction", "plant-insect interaction", kw_final)) %>%
#   mutate(kw_final = gsub("insect-plant interaction", "plant-insect interaction", kw_final)) %>%
#   mutate(kw_final = gsub("insect-plant relationships", "insect-plant relationships", kw_final)) %>%
#   mutate(kw_final = gsub("insular populations", "insular population", kw_final)) %>%
#   mutate(kw_final = gsub("inter annual variability", "interannual variability", kw_final)) %>%
#   mutate(kw_final = gsub("inter birth interval", "interbirth interval", kw_final)) %>%
#   mutate(kw_final = gsub("inter breeding", "interbreeding", kw_final)) %>%
#   mutate(kw_final = gsub("inter population hybridization", "interpopulation hybridization", kw_final)) %>%
#   mutate(kw_final = gsub("inter sexual selection", "intersexual selection", kw_final)) %>%
#   mutate(kw_final = gsub("inter species interactions", "interspecies interactions", kw_final)) %>%
#   mutate(kw_final = gsub("inter specific", "interspecific", kw_final)) %>%
#   mutate(kw_final = gsub("inter specific competition", "interspecific competition", kw_final)) %>%
#   mutate(kw_final = gsub("inter specific interactions", "interspecific interactions", kw_final)) %>%
#   mutate(kw_final = gsub("intera ctions", "interactions", kw_final)) %>%
#   mutate(kw_final = gsub("international union for conservation of nature  iucn", "iucn", kw_final)) %>%
#   mutate(kw_final = gsub("international union for conservation of nature iucn", "iucn", kw_final)) %>%
#   mutate(kw_final = gsub("intertidal snails", "intertidal snail", kw_final)) %>%
#   mutate(kw_final = gsub("intra guild interactions", "intraguild interactions", kw_final)) %>%
#   mutate(kw_final = gsub("intra individual variability", "intraindividual variability", kw_final)) %>%
#   mutate(kw_final = gsub("intra locus sexual conflict", "intralocus sexual conflict", kw_final)) %>%
#   mutate(kw_final = gsub("intra sexual competition", "intrasexual competition", kw_final)) %>%
#   mutate(kw_final = gsub("intra sexual dimorphism", "intrasexual dimorphism", kw_final)) %>%
#   mutate(kw_final = gsub("intra sexual selection", "intrasexual selection", kw_final)) %>%
#   mutate(kw_final = gsub("intra specific competition", "intraspecific competition", kw_final)) %>%
#   mutate(kw_final = gsub("intra specific facilitation", "intraspecific facilitation", kw_final)) %>%
#   mutate(kw_final = gsub("intra specific interaction", "intraspecific interaction", kw_final)) %>%
#   mutate(kw_final = gsub("intra specific trait variation", "intraspecific trait variation", kw_final)) %>%
#   mutate(kw_final = gsub("intra specific variation", "intraspecific variation", kw_final)) %>%
#   mutate(kw_final = gsub("intrinsic post zygotic isolation", "intrinsic postzygotic isolation", kw_final)) %>%
#   mutate(kw_final = gsub("invasive nonnative species", "nonnative invasive species", kw_final)) %>%
#   mutate(kw_final = gsub("isolation by-environment", "isolation-by-environment", kw_final)) %>%
#   mutate(kw_final = gsub("isolation bydistance", "isolation-by-distance", kw_final)) %>%
#   mutate(kw_final = gsub("isotope   n  15", "15n isotope", kw_final)) %>%
#   mutate(kw_final = gsub("isthmus of panamá", "isthmus of panama", kw_final)) %>%
#   mutate(kw_final = gsub("its 1", "its1", kw_final)) %>%
#   mutate(kw_final = gsub("itsrdna", "its rdna", kw_final)) %>%
#   mutate(kw_final = gsub("janzen-connell effect", "janzen-connell effect", kw_final)) %>%
#   mutate(kw_final = gsub("key resources", "key resource", kw_final)) %>%
#   mutate(kw_final = gsub("l ymnaea stagnalis", "lymnaea stagnalis", kw_final)) %>%
#   mutate(kw_final = gsub("la paz-bay", "la paz bay", kw_final)) %>%
#   mutate(kw_final = gsub("lady beetles", "ladybeetles", kw_final)) %>%
#   mutate(kw_final = gsub("lake gatun", "gatun lake", kw_final)) %>%
#   mutate(kw_final = gsub("lake land linkages", "lake-land linkages", kw_final)) %>%
#   mutate(kw_final = gsub("lake mývatn", "lake myvatn", kw_final)) %>%
#   mutate(kw_final = gsub("land use cover", "land-use cover", kw_final)) %>%
#   mutate(kw_final = gsub("land use-cover", "land-use cover", kw_final)) %>%
#   mutate(kw_final = gsub("larder hoarding", "larderhoarding", kw_final)) %>%
#   mutate(kw_final = gsub("large scale disturbances", "large-scale disturbance", kw_final)) %>%
#   mutate(kw_final = gsub("late wood", "latewood", kw_final)) %>%
#   mutate(kw_final = gsub("leaf ", "leaf", kw_final)) %>%
#   mutate(kw_final = gsub("leaf cutter ant", "leafcutter ant", kw_final)) %>%
#   mutate(kw_final = gsub("levins metapopulations", "levins metapopulation", kw_final)) %>%
#   mutate(kw_final = gsub("levinsb", "levins b", kw_final)) %>%
#   mutate(kw_final = gsub("liana tree competition", "liana-tree competition", kw_final)) %>%
#   mutate(kw_final = gsub("liana tree interaction", "liana-tree interaction", kw_final)) %>%
#   mutate(kw_final = gsub("life history switch point", "life-history switch point", kw_final)) %>%
#   mutate(kw_final = gsub("life table-response experiments", "life-table response experiments", kw_final)) %>%
#   mutate(kw_final = gsub("lifehistory", "life-history", kw_final)) %>%
#   mutate(kw_final = gsub("lifehistory covariation", "life-history covariation", kw_final)) %>%
#   mutate(kw_final = gsub("lifehistory switch point", "life-history switch point", kw_final)) %>%
#   mutate(kw_final = gsub("lifehistory trade offs", "life-history tradeoffs", kw_final)) %>%
#   mutate(kw_final = gsub("lifehistory traits", "life-history traits", kw_final)) %>%
#   mutate(kw_final = gsub("light : nutrient hypothesis", "light:nutrient hypothesis", kw_final)) %>%
#   mutate(kw_final = gsub("light responses curve", "light curve responses", kw_final)) %>%
#   mutate(kw_final = gsub("likelihoodratio test", "likelihood ratio test", kw_final)) %>%
#   mutate(kw_final = gsub("linearmodels", "linear models", kw_final)) %>%
#   mutate(kw_final = gsub("lock and key", "lock-and-key", kw_final)) %>%
#   mutate(kw_final = gsub("logseries", "log series", kw_final)) %>%
#   mutate(kw_final = gsub("long term ecological research site", "lter", kw_final)) %>%
#   mutate(kw_final = gsub("long term ecological research sites", "lter", kw_final)) %>%
#   mutate(kw_final = gsub("longdistance migration", "long-distance migration", kw_final)) %>%
#   mutate(kw_final = gsub("longterm data", "long-term data", kw_final)) %>%
#   mutate(kw_final = gsub("longterm monitoring", "long-term monitoring", kw_final)) %>%
#   mutate(kw_final = gsub("loss of-function", "loss of function", kw_final)) %>%
#   mutate(kw_final = gsub("lotka volterra competition model", "lotka-volterra competition model", kw_final)) %>%
#   mutate(kw_final = gsub("lotka-volterra", "lotka-volterra", kw_final)) %>%
#   mutate(kw_final = gsub("loxodonta africana africana", "loxodonta africana", kw_final)) %>%
#   mutate(kw_final = gsub("lupïnus arboreus", "lupinus arboreus", kw_final)) %>%
#   mutate(kw_final = gsub("luscinia luscinia", "luscinia", kw_final)) %>%
#   mutate(kw_final = gsub("lyman tria dispar l", "lymantria dispar", kw_final)) %>%
#   mutate(kw_final = gsub("lymantria dispar l", "lymantria dispar", kw_final)) %>%
#   mutate(kw_final = gsub("lynx lynx canadensis", "lynx canadensis", kw_final)) %>%
#   mutate(kw_final = gsub("lévy flights", "levy flights", kw_final)) %>%
#   mutate(kw_final = gsub("ma crocystis pyrifera", "macrocystis pyrifera", kw_final)) %>%
#   mutate(kw_final = gsub("macro ecology", "macroecology", kw_final)) %>%
#   mutate(kw_final = gsub("macro invertebrate", "macroinvertebrate", kw_final)) %>%
#   mutate(kw_final = gsub("macro invertebrates", "macroinvertebrates", kw_final)) %>%
#   mutate(kw_final = gsub("macro molluscs", "macromolluscs", kw_final)) %>%
#   mutate(kw_final = gsub("macro mutation", "macromutation", kw_final)) %>%
#   mutate(kw_final = gsub("mahogany shoot borer", "mahogany shootborer", kw_final)) %>%
#   mutate(kw_final = gsub("male contest competition", "male-male competition", kw_final)) %>%
#   mutate(kw_final = gsub("male fitness gain curves", "male fitness gain curve", kw_final)) %>%
#   mutate(kw_final = gsub("male male contest competition", "male-male competition", kw_final)) %>%
#   mutate(kw_final = gsub("manacus manacus", "manacus", kw_final)) %>%
#   mutate(kw_final = gsub("manage ment", "management", kw_final)) %>%
#   mutate(kw_final = gsub("management conservation", "conservation management", kw_final)) %>%
#   mutate(kw_final = gsub("marine protected área", "marine protected area", kw_final)) %>%
#   mutate(kw_final = gsub("mark  recapture", "mark-recapture", kw_final)) %>%
#   mutate(kw_final = gsub("mark recapture data", "mark-recapture data", kw_final)) %>%
#   mutate(kw_final = gsub("mark-recapture", "mark-recapture", kw_final)) %>%
#   mutate(kw_final = gsub("mark-recapture model", "capture mark-recapture model", kw_final)) %>%
#   mutate(kw_final = gsub("mark-release-recapture", "mark-release-recapture", kw_final)) %>%
#   mutate(kw_final = gsub("mass abundance scaling", "mass-abundance scaling", kw_final)) %>%
#   mutate(kw_final = gsub("match-mismatch", "match-mismatch", kw_final)) %>%
#   mutate(kw_final = gsub("match-mismatch", "match-mismatch", kw_final)) %>%
#   mutate(kw_final = gsub("mating system transitions", "mating system transition", kw_final)) %>%
#   mutate(kw_final = gsub("matrix correlation", "correlation matrix", kw_final)) %>%
#   mutate(kw_final = gsub("matrix projection", "projection matrix", kw_final)) %>%
#   mutate(kw_final = gsub("maximum likelihood estimates", "maximum likelihood estimate", kw_final)) %>%
#   mutate(kw_final = gsub("maërl beds", "maerl beds", kw_final)) %>%
#   mutate(kw_final = gsub("mean variance relationship", "mean-variance relationship", kw_final)) %>%
#   mutate(kw_final = gsub("mean variance scaling", "mean-variance scaling", kw_final)) %>%
#   mutate(kw_final = gsub("mega herbivore", "megaherbivore", kw_final)) %>%
#   mutate(kw_final = gsub("mega herbivores", "megaherbivores", kw_final)) %>%
#   mutate(kw_final = gsub("melanargia galathea l", "melanargia galathea", kw_final)) %>%
#   mutate(kw_final = gsub("melanoplus femurrubrum femurrubrum", "melanoplus femurrubrum", kw_final)) %>%
#   mutate(kw_final = gsub("meso herbivore", "mesoherbivore", kw_final)) %>%
#   mutate(kw_final = gsub("meso herbivores", "mesoherbivore", kw_final)) %>%
#   mutate(kw_final = gsub("meta frontier", "metafrontier", kw_final)) %>%
#   mutate(kw_final = gsub("meta plasticity", "metaplasticity", kw_final)) %>%
#   mutate(kw_final = gsub("meta population ecology", "metapopulation ecology", kw_final)) %>%
#   mutate(kw_final = gsub("metapopulation dynamic", "metapopulation dynamics", kw_final)) %>%
#   mutate(kw_final = gsub("metopeurum fuscoviride", "metopeurum fusco viride", kw_final)) %>%
#   mutate(kw_final = gsub("michoacán", "michoacan", kw_final)) %>%
#   mutate(kw_final = gsub("micro climate", "microclimate", kw_final)) %>%
#   mutate(kw_final = gsub("micro ecosystem", "microecosystem", kw_final)) %>%
#   mutate(kw_final = gsub("micro environment", "microenvironment", kw_final)) %>%
#   mutate(kw_final = gsub("micro environmental conditions", "microenvironmental conditions", kw_final)) %>%
#   mutate(kw_final = gsub("micro environmental variation", "microenvironmental variation", kw_final)) %>%
#   mutate(kw_final = gsub("micro habitat", "microhabitat", kw_final)) %>%
#   mutate(kw_final = gsub("micro satellites", "microsatellites", kw_final)) %>%
#   mutate(kw_final = gsub("micro topography", "microtopography", kw_final)) %>%
#   mutate(kw_final = gsub("microbe host interactions", "host-microbe interactions", kw_final)) %>%
#   mutate(kw_final = gsub("microbial n biomass", "microbial biomass n", kw_final)) %>%
#   mutate(kw_final = gsub("microscopy electronic", "electronic microscopy", kw_final)) %>%
#   mutate(kw_final = gsub("microsporid ia", "microsporidia", kw_final)) %>%
#   mutate(kw_final = gsub("mineralization: nitrogen", "nitrogen mineralization", kw_final)) %>%
#   mutate(kw_final = gsub("mini barcode", "minibarcode", kw_final)) %>%
#   mutate(kw_final = gsub("mini rhizotrons", "minirhizotrons", kw_final)) %>%
#   mutate(kw_final = gsub("mito nuclear discordance", "mitonuclear discordance", kw_final)) %>%
#   mutate(kw_final = gsub("mixed linear model", "linear mixed model", kw_final)) %>%
#   mutate(kw_final = gsub("mixed model effects", "mixed effects model", kw_final)) %>%
#   mutate(kw_final = gsub("mixed species stands", "mixed species stand", kw_final)) %>%
#   mutate(kw_final = gsub("mixedgrass prairie", "mixed-grass prairie", kw_final)) %>%
#   mutate(kw_final = gsub("mixedwoods", "mixed woods", kw_final)) %>%
#   mutate(kw_final = gsub("mixing litter", "litter mixing", kw_final)) %>%
#   mutate(kw_final = gsub("mo nogamy", "monogamy", kw_final)) %>%
#   mutate(kw_final = gsub("model aggregation", "aggregation model", kw_final)) %>%
#   mutate(kw_final = gsub("model data comparison", "model-data comparison", kw_final)) %>%
#   mutate(kw_final = gsub("model population", "population model", kw_final)) %>%
#   mutate(kw_final = gsub("modeling distribution", "distribution modeling", kw_final)) %>%
#   mutate(kw_final = gsub("modeling population dynamics", "population dynamics modeling", kw_final)) %>%
#   mutate(kw_final = gsub("mono culture plantation", "monoculture plantation", kw_final)) %>%
#   mutate(kw_final = gsub("monocot ", "monocot", kw_final)) %>%
#   mutate(kw_final = gsub("monsoons", "monsoon", kw_final)) %>%
#   mutate(kw_final = gsub("mosaic landscape", "landscape mosaic", kw_final)) %>%
#   mutate(kw_final = gsub("mosquitos", "mosquito", kw_final)) %>%
#   mutate(kw_final = gsub("most productive institutions and authors", "most productive authors and institutions", kw_final)) %>%
#   mutate(kw_final = gsub("multi annual cycles", "multiannual cycles", kw_final)) %>%
#   mutate(kw_final = gsub("multi dimensional scaling", "multidimensional scaling", kw_final)) %>%
#   mutate(kw_final = gsub("multi element analysis", "multielement analysis", kw_final)) %>%
#   mutate(kw_final = gsub("multi functionality", "multifunctionality", kw_final)) %>%
#   mutate(kw_final = gsub("multi host system", "multihost system", kw_final)) %>%
#   mutate(kw_final = gsub("multi locus", "multilocus", kw_final)) %>%
#   mutate(kw_final = gsub("multi locus heterozygosity", "multilocus heterozygosity", kw_final)) %>%
#   mutate(kw_final = gsub("multi modal signaling", "multimodal signaling", kw_final)) %>%
#   mutate(kw_final = gsub("multi modal signals", "multimodal signal", kw_final)) %>%
#   mutate(kw_final = gsub("multi predator environments", "multipredator environments", kw_final)) %>%
#   mutate(kw_final = gsub("multi scale", "multiscale", kw_final)) %>%
#   mutate(kw_final = gsub("multi scale analysis", "multiscale analysis", kw_final)) %>%
#   mutate(kw_final = gsub("multi scaled random walk", "multiscaled random walk", kw_final)) %>%
#   mutate(kw_final = gsub("multi species communities", "multispecies communities", kw_final)) %>%
#   mutate(kw_final = gsub("multi species interaction", "multispecies interaction", kw_final)) %>%
#   mutate(kw_final = gsub("multi state capture-recapture models", "multistate capture recapture models", kw_final)) %>%
#   mutate(kw_final = gsub("multi state model", "multistate model", kw_final)) %>%
#   mutate(kw_final = gsub("multi stemmed trees", "multistemmed trees", kw_final)) %>%
#   mutate(kw_final = gsub("multi strata models", "multistrata models", kw_final)) %>%
#   mutate(kw_final = gsub("multi trophic communities", "multitrophic communities", kw_final)) %>%
#   mutate(kw_final = gsub("multi trophic interaction", "multitrophic interaction", kw_final)) %>%
#   mutate(kw_final = gsub("multi trophic models", "multitrophic models", kw_final)) %>%
#   mutate(kw_final = gsub("multi trophic networks", "multitrophic networks", kw_final)) %>%
#   mutate(kw_final = gsub("multiple stable state", "multiple stable states", kw_final)) %>%
#   mutate(kw_final = gsub("murray darling basin", "murray-darling basin", kw_final)) %>%
#   mutate(kw_final = gsub("mustela nivalis nivalis", "mustela nivalis", kw_final)) %>%
#   mutate(kw_final = gsub("mut ualism", "mutualism", kw_final)) %>%
#   mutate(kw_final = gsub("mutational melt down", "mutational meltdown", kw_final)) %>%
#   mutate(kw_final = gsub("mutual isms", "mutualisms", kw_final)) %>%
#   mutate(kw_final = gsub("myotis myotis", "myotis", kw_final)) %>%
#   mutate(kw_final = gsub("myrmechocory", "myrmecochory", kw_final)) %>%
#   mutate(kw_final = gsub("n : p ratio", "n:p ratio", kw_final)) %>%
#   mutate(kw_final = gsub("n and p co limitation", "n:p colimitation", kw_final)) %>%
#   mutate(kw_final = gsub("n and p colimitation", "n:p colimitation", kw_final)) %>%
#   mutate(kw_final = gsub("n p ratios", "n:p ratios", kw_final)) %>%
#   mutate(kw_final = gsub("n: p ratios", "n:p ratios", kw_final)) %>%
#   mutate(kw_final = gsub("nasua nasua", "nasua", kw_final)) %>%
#   mutate(kw_final = gsub("native vs introduced species", "introduced vs native species", kw_final)) %>%
#   mutate(kw_final = gsub("natural abundance δ n 15", "natural abundance δ n15", kw_final)) %>%
#   mutate(kw_final = gsub("nature reserves", "nature reserve", kw_final)) %>%
#   mutate(kw_final = gsub("nearest neighbor distances", "nearest neighbor distance", kw_final)) %>%
#   mutate(kw_final = gsub("nectar spurs", "nectar spur", kw_final)) %>%
#   mutate(kw_final = gsub("neo endemism", "neoendemism", kw_final)) %>%
#   mutate(kw_final = gsub("neo tropics", "neotropics", kw_final)) %>%
#   mutate(kw_final = gsub("nest predation rate", "nest-predation rate", kw_final)) %>%
#   mutate(kw_final = gsub("nest predation rates", "nest-predation rate", kw_final)) %>%
#   mutate(kw_final = gsub("nest sites", "nest site", kw_final)) %>%
#   mutate(kw_final = gsub("nesting sites", "nesting site", kw_final)) %>%
#   mutate(kw_final = gsub("nh   immobilization 4 +", "nh4+ immobilization", kw_final)) %>%
#   mutate(kw_final = gsub("nh  immobilization 4+", "nh4+ immobilization", kw_final)) %>%
#   mutate(kw_final = gsub("niche neutrality continuum", "niche-neutrality continuum", kw_final)) %>%
#   mutate(kw_final = gsub("niche-neutrality continuum", "niche-neutrality continuum", kw_final)) %>%
#   mutate(kw_final = gsub("night time transpiration", "night-time transpiration", kw_final)) %>%
#   mutate(kw_final = gsub("nighttime transpiration", "night-time transpiration", kw_final)) %>%
#   mutate(kw_final = gsub("nitrogen and light availability", "light and nitrogen availability", kw_final)) %>%
#   mutate(kw_final = gsub("nitrogen to phosphorus ratio", "n:p ratio", kw_final)) %>%
#   mutate(kw_final = gsub("nitrogen to-phosphorus ratio", "n:p ratio", kw_final)) %>%
#   mutate(kw_final = gsub("no  immobilization 3 ", "no3 immobilization", kw_final)) %>%
#   mutate(kw_final = gsub("no  immobilization 3  ", "no3 immobilization", kw_final)) %>%
#   mutate(kw_final = gsub("no 2", "no2", kw_final)) %>%
#   mutate(kw_final = gsub("non additive selection", "nonadditive selection", kw_final)) %>%
#   mutate(kw_final = gsub("non additivity", "nonadditivity", kw_final)) %>%
#   mutate(kw_final = gsub("non breeders", "nonbreeders", kw_final)) %>%
#   mutate(kw_final = gsub("non breeding", "nonbreeding", kw_final)) %>%
#   mutate(kw_final = gsub("non consumptive interactions", "nonconsumptive interactions", kw_final)) %>%
#   mutate(kw_final = gsub("non destructive method", "nondestructive method", kw_final)) %>%
#   mutate(kw_final = gsub("non equilibrium dynamics", "nonequilibrium dynamics", kw_final)) %>%
#   mutate(kw_final = gsub("non human primates", "nonhuman primates", kw_final)) %>%
#   mutate(kw_final = gsub("non independence", "nonindependence", kw_final)) %>%
#   mutate(kw_final = gsub("non invasive monitoring", "noninvasive monitoring", kw_final)) %>%
#   mutate(kw_final = gsub("non linear models", "nonlinear models", kw_final)) %>%
#   mutate(kw_final = gsub("non linearity", "nonlinearity", kw_final)) %>%
#   mutate(kw_final = gsub("non metric multi-dimensional scaling", "nmds", kw_final)) %>%
#   mutate(kw_final = gsub("non native plant", "nonnative plant", kw_final)) %>%
#   mutate(kw_final = gsub("non parametric covariance function", "nonparametric covariance function", kw_final)) %>%
#   mutate(kw_final = gsub("non parametric estimators", "nonparametric estimators", kw_final)) %>%
#   mutate(kw_final = gsub("non pollinating fig wasps", "nonpollinating fig wasps", kw_final)) %>%
#   mutate(kw_final = gsub("north eastern north america", "eastern north america", kw_final)) %>%
#   mutate(kw_final = gsub("north west argentina", "northwest argentina", kw_final)) %>%
#   mutate(kw_final = gsub("north west himalaya", "northwest himalaya", kw_final)) %>%
#   mutate(kw_final = gsub("north west territories", "northwest territories", kw_final)) %>%
#   mutate(kw_final = gsub("north western europe", "northwestern europe", kw_final)) %>%
#   mutate(kw_final = gsub("north western mediterranean", "northwestern mediterranean", kw_final)) %>%
#   mutate(kw_final = gsub("northern québec", "northern quebec", kw_final)) %>%
#   mutate(kw_final = gsub("nuclear cytoplasmic interaction", "cytoplasmic nuclear interaction", kw_final)) %>%
#   mutate(kw_final = gsub("nullmodel", "null model", kw_final)) %>%
#   mutate(kw_final = gsub("numerical and functional responses", "functional and numerical responses", kw_final)) %>%
#   mutate(kw_final = gsub("nutr ient dynamics", "nutrient dynamics", kw_final)) %>%
#   mutate(kw_final = gsub("nutrient  allelochemical interactions", "nutrient allelochemical interactions", kw_final)) %>%
#   mutate(kw_final = gsub("nutrient hotspots", "nutrient hotspot", kw_final)) %>%
#   mutate(kw_final = gsub("nutrient phytoplankton-zooplankton npz model", "nutrient-phytoplankton-zooplankton model", kw_final)) %>%
#   mutate(kw_final = gsub("nutrientphytoplankton zooplankton npz model", "nutrient-phytoplankton-zooplankton model", kw_final)) %>%
#   mutate(kw_final = gsub("n : p ratio", "n:p ratio", kw_final)) %>%
#   mutate(kw_final = gsub("o rthoptera", "orthoptera", kw_final)) %>%
#   mutate(kw_final = gsub("o u process", "ou process", kw_final)) %>%
#   mutate(kw_final = gsub("oak quercus", "quercus oak", kw_final)) %>%
#   mutate(kw_final = gsub("occupancy abundance relationship", "occupancy-abundance relationship", kw_final)) %>%
#   mutate(kw_final = gsub("ol faction", "olfaction", kw_final)) %>%
#   mutate(kw_final = gsub("oldgrowth forest", "old-growth forest", kw_final)) %>%
#   mutate(kw_final = gsub("ontogenetic diet shifts", "ontogenetic diet shift", kw_final)) %>%
#   mutate(kw_final = gsub("ornament evolution", "evolution ornament", kw_final)) %>%
#   mutate(kw_final = gsub("osa península", "osa peninsula", kw_final)) %>%
#   mutate(kw_final = gsub("outbreak pest", "pest outbreak", kw_final)) %>%
#   mutate(kw_final = gsub("over dominance", "overdominance", kw_final)) %>%
#   mutate(kw_final = gsub("over fishing", "overfishing", kw_final)) %>%
#   mutate(kw_final = gsub("over fitting", "overfitting", kw_final)) %>%
#   mutate(kw_final = gsub("over produced esterases", "overproduced esterase", kw_final)) %>%
#   mutate(kw_final = gsub("over winter survival", "overwinter survival", kw_final)) %>%
#   mutate(kw_final = gsub("over wintering", "overwintering", kw_final)) %>%
#   mutate(kw_final = gsub("overproduced esterases", "overproduced esterase", kw_final)) %>%
#   mutate(kw_final = gsub("oyster catcher", "oystercatcher", kw_final)) 
# 
# 
# 
# 
# keywords <- keywords %>%
#   mutate(kw_final = gsub("p  st", "pst", kw_final)) %>%
#   mutate(kw_final = gsub("pacific panamá", "pacific panama", kw_final)) %>%
#   mutate(kw_final = gsub("paircorrelation function", "pair-correlation function", kw_final)) %>%
#   mutate(kw_final = gsub("paleo ecology", "paleoecology", kw_final)) %>%
#   mutate(kw_final = gsub("pamana", "panama", kw_final)) %>%
#   mutate(kw_final = gsub("parasite host ecology", "host-parasite ecology", kw_final)) %>%
#   mutate(kw_final = gsub("parasite host interaction", "host-parasite interaction", kw_final)) %>%
#   mutate(kw_final = gsub("parasite host relationship", "host-parasite relationship", kw_final)) %>%
#   mutate(kw_final = gsub("parasite parasite interactions", "parasite-parasite interactions", kw_final)) %>%
#   mutate(kw_final = gsub("parasitoid host dynamics", "host-parasitoid dynamics", kw_final)) %>%
#   mutate(kw_final = gsub("parasitoid host interaction", "host-parasitoid interaction", kw_final)) %>%
#   mutate(kw_final = gsub("paren tal care", "parental care", kw_final)) %>%
#   mutate(kw_final = gsub("partitioning habitat", "habitat partitioning", kw_final)) %>%
#   mutate(kw_final = gsub("pathogen host interactions", "host-pathogen interactions", kw_final)) %>%
#   mutate(kw_final = gsub("peatbog", "peat bog", kw_final)) %>%
#   mutate(kw_final = gsub("pennaeus vannamei", "penaeus vannamei", kw_final)) %>%
#   mutate(kw_final = gsub("perennial bunch grass", "perennial bunchgrass", kw_final)) %>%
#   mutate(kw_final = gsub("perennial native grasses", "native perennial grasses", kw_final)) %>%
#   mutate(kw_final = gsub("performance trait", "trait performance", kw_final)) %>%
#   mutate(kw_final = gsub("phaseolus lunatus l", "phaseolus lunatus", kw_final)) %>%
#   mutate(kw_final = gsub("phenotypicselection analysis", "phenotypic selection analysis", kw_final)) %>%
#   mutate(kw_final = gsub("phosp horus", "phosphorus", kw_final)) %>%
#   mutate(kw_final = gsub("photosyn thesis", "photosynthesis", kw_final)) %>%
#   mutate(kw_final = gsub("phyloge nomics", "phylogenomics", kw_final)) %>%
#   mutate(kw_final = gsub("phylogenetic bayesian multilevel models", "bayesian phylogenetic multilevel models", kw_final)) %>%
#   mutate(kw_final = gsub("physico chemical", "physicochemical", kw_final)) %>%
#   mutate(kw_final = gsub("physico chemical factors", "physicochemical factors", kw_final)) %>%
#   mutate(kw_final = gsub("physico chemical parameters", "physicochemical parameters", kw_final)) %>%
#   mutate(kw_final = gsub("phyto plankton", "phytoplankton", kw_final)) %>%
#   mutate(kw_final = gsub("phytop lankton", "phytoplankton", kw_final)) %>%
#   mutate(kw_final = gsub("pinus ponderosa ponderosa", "pinus ponderosa", kw_final)) %>%
#   mutate(kw_final = gsub("pinus sp", "pinus", kw_final)) %>%
#   mutate(kw_final = gsub("pinussp", "pinus", kw_final)) %>%
#   mutate(kw_final = gsub("pit fall traps", "pitfall traps", kw_final)) %>%
#   mutate(kw_final = gsub("piñon", "pinon", kw_final)) %>%
#   mutate(kw_final = gsub("plant  animal interactions", "plant-animal interactions", kw_final)) %>%
#   mutate(kw_final = gsub("plant  insect interaction", "insect-plant interaction", kw_final)) %>%
#   mutate(kw_final = gsub("plant anim al interactions", "plant-animal interactions", kw_final)) %>%
#   mutate(kw_final = gsub("plant environment interactions", "plant-environment interactions", kw_final)) %>%
#   mutate(kw_final = gsub("plant fungi interactions", "plant-fungi interactions", kw_final)) %>%
#   mutate(kw_final = gsub("plant fungus interactions", "plant-fungus interactions", kw_final)) %>%
#   mutate(kw_final = gsub("plant herbivore dynamics", "plant-herbivore dynamics", kw_final)) %>%
#   mutate(kw_final = gsub("plant indirect defense", "indirect plant defense", kw_final)) %>%
#   mutate(kw_final = gsub("plant induced defense", "induced plant defense", kw_final)) %>%
#   mutate(kw_final = gsub("plant induced defenses", "induced plant defense", kw_final)) %>%
#   mutate(kw_final = gsub("plant insect association", "insect-plant association", kw_final)) %>%
#   mutate(kw_final = gsub("plant insect-pathogen interactions", "plant-insect-pathogen interactions", kw_final)) %>%
#   mutate(kw_final = gsub("plant interference", "plant-plant interference", kw_final)) %>%
#   mutate(kw_final = gsub("plant invasive", "invasive plant", kw_final)) %>%
#   mutate(kw_final = gsub("plant mineral nutrition", "mineral plant nutrition", kw_final)) %>%
#   mutate(kw_final = gsub("plant plant interference", "plant-plant interference", kw_final)) %>%
#   mutate(kw_final = gsub("plant resources", "plant resource", kw_final)) %>%
#   mutate(kw_final = gsub("plant soil biota interactions", "plant-soil-biota interactions", kw_final)) %>%
#   mutate(kw_final = gsub("plant soil microbe interactions", "plant soil-microbe interactions", kw_final)) %>%
#   mutate(kw_final = gsub("plant soil system", "plant-soil system", kw_final)) %>%
#   mutate(kw_final = gsub("plant soil-biota interactions", "plant-soil-biota interactions", kw_final)) %>%
#   mutate(kw_final = gsub("plant-herbivore interactions", "plant-herbivore interactions", kw_final)) %>%
#   mutate(kw_final = gsub("plantanimal interactions", "plant-animal interactions", kw_final)) %>%
#   mutate(kw_final = gsub("plantation forest", "forest plantation", kw_final)) %>%
#   mutate(kw_final = gsub("plantinsect interactions", "plant-insect interaction", kw_final)) %>%
#   mutate(kw_final = gsub("plantsoil interactions", "plant-soil interactions", kw_final)) %>%
#   mutate(kw_final = gsub("plant-insect-pathogen interactions", "plant-insect-pathogen interactions", kw_final)) %>%
#   mutate(kw_final = gsub("plant-pathogen-insect interactions", "plant-insect-pathogen interactions", kw_final)) %>%
#   mutate(kw_final = gsub("plant-pollinator network", "plant-pollinator network", kw_final)) %>%
#   mutate(kw_final = gsub("pleist ocene", "pleistocene", kw_final)) %>%
#   mutate(kw_final = gsub("poisson log normal", "poisson lognormal", kw_final)) %>%
#   mutate(kw_final = gsub("policy making", "policymaking", kw_final)) %>%
#   mutate(kw_final = gsub("pollen : ovule ratio", "pollen:ovule ratio", kw_final)) %>%
#   mutate(kw_final = gsub("pollen carry over", "pollen carryover", kw_final)) %>%
#   mutate(kw_final = gsub("pollen-ovule ratio", "pollen:ovule ratio", kw_final)) %>%
#   mutate(kw_final = gsub("pollinating fig wasp", "fig pollinating wasp", kw_final)) %>%
#   mutate(kw_final = gsub("pollinator plant interactions", "plant-pollinator interactions", kw_final)) %>%
#   mutate(kw_final = gsub("poly morphism", "polymorphism", kw_final)) %>%
#   mutate(kw_final = gsub("pond breeding amphibian", "pondbreeding amphibian", kw_final)) %>%
#   mutate(kw_final = gsub("population estimates", "population estimate", kw_final)) %>%
#   mutate(kw_final = gsub("population matrix model", "matrix population model", kw_final)) %>%
#   mutate(kw_final = gsub("population matrix models", "matrix population models", kw_final)) %>%
#   mutate(kw_final = gsub("population responses", "population response", kw_final)) %>%
#   mutate(kw_final = gsub("population s", "populations", kw_final)) %>%
#   mutate(kw_final = gsub("population size structures", "population size structure", kw_final)) %>%
#   mutate(kw_final = gsub("porcellionides p rui nos us", "porcellionides pruinosus", kw_final)) %>%
#   mutate(kw_final = gsub("porites porites", "porites", kw_final)) %>%
#   mutate(kw_final = gsub("positive plant plant interactions", "positive plant interactions", kw_final)) %>%
#   mutate(kw_final = gsub("post dispersal", "postdispersal", kw_final)) %>%
#   mutate(kw_final = gsub("post dispersal mortality", "postdispersal mortality", kw_final)) %>%
#   mutate(kw_final = gsub("post fire recovery", "postfire recovery", kw_final)) %>%
#   mutate(kw_final = gsub("post fire regeneration", "postfire regeneration", kw_final)) %>%
#   mutate(kw_final = gsub("post fire resprouting", "postfire resprouting", kw_final)) %>%
#   mutate(kw_final = gsub("post glacial dispersal", "postglacial dispersal", kw_final)) %>%
#   mutate(kw_final = gsub("post glacial expansion", "postglacial expansion", kw_final)) %>%
#   mutate(kw_final = gsub("post glacial range expansion", "postglacial range expansion", kw_final)) %>%
#   mutate(kw_final = gsub("post glacial recolonization", "postglacial recolonization", kw_final)) %>%
#   mutate(kw_final = gsub("post ingestive feedback", "postingestive feedback", kw_final)) %>%
#   mutate(kw_final = gsub("post mating", "post-mating", kw_final)) %>%
#   mutate(kw_final = gsub("post mating female response", "post-mating female response", kw_final)) %>%
#   mutate(kw_final = gsub("post mating prezygotic barriers", "postmating prezygotic barriers", kw_final)) %>%
#   mutate(kw_final = gsub("postcopulatory", "post-copulatory", kw_final)) %>%
#   mutate(kw_final = gsub("potential evapo transpiration", "potential evapotranspiration", kw_final)) %>%
#   mutate(kw_final = gsub("power lines", "powerlines", kw_final)) %>%
#   mutate(kw_final = gsub("powerlaw scaling", "power law scaling", kw_final)) %>%
#   mutate(kw_final = gsub("pre dispersal seed predators", "predispersal seed predator", kw_final)) %>%
#   mutate(kw_final = gsub("pre dispersal seed-predator", "predispersal seed predator", kw_final)) %>%
#   mutate(kw_final = gsub("pre emptive competition", "preemptive competition", kw_final)) %>%
#   mutate(kw_final = gsub("pre montane", "premontane", kw_final)) %>%
#   mutate(kw_final = gsub("pre zygotic", "prezygotic", kw_final)) %>%
#   mutate(kw_final = gsub("precipitation extreme", "extreme precipitation", kw_final)) %>%
#   mutate(kw_final = gsub("preda tor", "predator", kw_final)) %>%
#   mutate(kw_final = gsub("predati on", "predation", kw_final)) %>%
#   mutate(kw_final = gsub("predator  prey dynamics", "predator-prey dynamics", kw_final)) %>%
#   mutate(kw_final = gsub("predator : prey ratio", "predator:prey ratio", kw_final)) %>%
#   mutate(kw_final = gsub("predator non consumptive effects", "predator nonconsumptive effects", kw_final)) %>%
#   mutate(kw_final = gsub("predator prey ecology", "predator-prey ecology", kw_final)) %>%
#   mutate(kw_final = gsub("predatorprey", "predator-prey", kw_final)) %>%
#   mutate(kw_final = gsub("predispersal seed predators", "predispersal seed predator", kw_final)) %>%
#   mutate(kw_final = gsub("preferred speeds", "preferred speed", kw_final)) %>%
#   mutate(kw_final = gsub("premontane moist forest", "moist premontane forest", kw_final)) %>%
#   mutate(kw_final = gsub("presence absence map", "presence-absence map", kw_final)) %>%
#   mutate(kw_final = gsub("presence only model", "presence-only model", kw_final)) %>%
#   mutate(kw_final = gsub("presence-absence map", "presence-absence map", kw_final)) %>%
#   mutate(kw_final = gsub("presenceonly model", "presence-only model", kw_final)) %>%
#   mutate(kw_final = gsub("prey predator dynamics", "predator-prey dynamics", kw_final)) %>%
#   mutate(kw_final = gsub("prey predator interaction", "predator-prey interaction", kw_final)) %>%
#   mutate(kw_final = gsub("prey predator ratio", "predator:prey ratio", kw_final)) %>%
#   mutate(kw_final = gsub("prey predator system", "prey-predator system", kw_final)) %>%
#   mutate(kw_final = gsub("prey spider", "spider prey", kw_final)) %>%
#   mutate(kw_final = gsub("pro ovigeny", "proovigeny", kw_final)) %>%
#   mutate(kw_final = gsub("procrustes distances", "procrustes distance", kw_final)) %>%
#   mutate(kw_final = gsub("produ ctivity", "productivity", kw_final)) %>%
#   mutate(kw_final = gsub("productivity diversity", "productivity-diversity", kw_final)) %>%
#   mutate(kw_final = gsub("pseudo autosomal region", "pseudoautosomal region", kw_final)) %>%
#   mutate(kw_final = gsub("pseudo replication", "pseudoreplication", kw_final)) %>%
#   mutate(kw_final = gsub("q  -f st st", "q-f st", kw_final)) %>%
#   mutate(kw_final = gsub("quality signal", "signal quality", kw_final)) %>%
#   mutate(kw_final = gsub("quasi cycles", "quasicycles", kw_final)) %>%
#   mutate(kw_final = gsub("quasi species", "quasispecies", kw_final)) %>%
#   mutate(kw_final = gsub("r  0", "r0", kw_final)) %>%
#   mutate(kw_final = gsub("r -k-selection", "r-k selection", kw_final)) %>%
#   mutate(kw_final = gsub("r h whittaker", "rh whittaker", kw_final)) %>%
#   mutate(kw_final = gsub("r* theory*", "r* theory", kw_final)) %>%
#   mutate(kw_final = gsub("r-k selection", "r-k selection", kw_final)) %>%
#   mutate(kw_final = gsub("r: fr", "red:far red ratio", kw_final)) %>%
#   mutate(kw_final = gsub("rain fall", "rainfall", kw_final)) %>%
#   mutate(kw_final = gsub("random ampliÔ¨Åed polymorphic dna", "rapd", kw_final)) %>%
#   mutate(kw_final = gsub("range restricted species", "restricted range species", kw_final)) %>%
#   mutate(kw_final = gsub("rare vs common species", "common vs rare species", kw_final)) %>%
#   mutate(kw_final = gsub("razor fish", "razorfish", kw_final)) %>%
#   mutate(kw_final = gsub("re burn", "reburn", kw_final)) %>%
#   mutate(kw_final = gsub("re fuge", "refuge", kw_final)) %>%
#   mutate(kw_final = gsub("re introduction", "reintroduction", kw_final)) %>%
#   mutate(kw_final = gsub("re mating", "remating", kw_final)) %>%
#   mutate(kw_final = gsub("re productive suppression", "reproductive suppression", kw_final)) %>%
#   mutate(kw_final = gsub("reaction  diffusion", "reaction-diffusion", kw_final)) %>%
#   mutate(kw_final = gsub("recessive alleles", "recessive allele", kw_final)) %>%
#   mutate(kw_final = gsub("red : far red ratio", "red:far red ratio", kw_final)) %>%
#   mutate(kw_final = gsub("red far red ratio", "red:far red ratio", kw_final)) %>%
#   mutate(kw_final = gsub("red fox  vulpes vulpes ", "red fox, vulpes vulpes ", kw_final)) %>%
#   mutate(kw_final = gsub("red fox vulpes vulpes", "red fox, vulpes vulpes ", kw_final)) %>%
#   mutate(kw_final = gsub("red jungle fowl", "red junglefowl", kw_final)) %>%
#   mutate(kw_final = gsub("red-far red ratio", "red:far red ratio", kw_final)) %>%
#   mutate(kw_final = gsub("red: far red ratio", "red:far red ratio", kw_final)) %>%
#   mutate(kw_final = gsub("reduction oxidation potential", "oxidation reduction potential", kw_final)) %>%
#   mutate(kw_final = gsub("reef coral", "coral reef", kw_final)) %>%
#   mutate(kw_final = gsub("regional vs local dynamics", "local vs regional dynamics", kw_final)) %>%
#   mutate(kw_final = gsub("regression trees", "regression tree", kw_final)) %>%
#   mutate(kw_final = gsub("relative species abundances", "relative species abundance", kw_final)) %>%
#   mutate(kw_final = gsub("remotesensing", "remote sensing", kw_final)) %>%
#   mutate(kw_final = gsub("reproduc tion", "reproduction", kw_final)) %>%
#   mutate(kw_final = gsub("reproductive life span", "reproductive lifespan", kw_final)) %>%
#   mutate(kw_final = gsub("reproductivebiology", "reproductive biology", kw_final)) %>%
#   mutate(kw_final = gsub("residence patch", "patch residence", kw_final)) %>%
#   mutate(kw_final = gsub("residues", "residue", kw_final)) %>%
#   mutate(kw_final = gsub("resistance surfaces", "resistance surface", kw_final)) %>%
#   mutate(kw_final = gsub("resource waves", "resource wave", kw_final)) %>%
#   mutate(kw_final = gsub("response surfaces", "response surface", kw_final)) %>%
#   mutate(kw_final = gsub("responses", "response", kw_final)) %>%
#   mutate(kw_final = gsub("amplified fragment length polymorphism aflp", "aflp", kw_final)) %>%
#   mutate(kw_final = gsub("aflp markers", "aflp", kw_final)) %>%
#   mutate(kw_final = gsub("aflp analysis", "aflp", kw_final)) %>%
#   mutate(kw_final = gsub("amplified fragment length polymorphisms", "aflp", kw_final)) %>%
#   mutate(kw_final = gsub("amplified fragment length polymorphism", "aflp", kw_final)) %>%
#   mutate(kw_final = gsub("methylation sensitive amplified fragment length polymorphism ms-aflp", "ms-aflp", kw_final)) %>%
#   mutate(kw_final = gsub("fluorescent fragment length polymorphism fflp", "fflp", kw_final)) %>%
#   mutate(kw_final = gsub("terminal restriction fragment length polymorphism", "trflp", kw_final)) %>%
#   mutate(kw_final = gsub("t rflp", "trflp", kw_final)) %>%
#   mutate(kw_final = gsub("rflp analysis", "rflp", kw_final)) %>%
#   mutate(kw_final = gsub("restriction fragment length polymorphism rflp", "rflp", kw_final)) %>%
#   mutate(kw_final = gsub("restriction fragment length polymorphisms rflp", "rflp", kw_final)) %>%
#   mutate(kw_final = gsub("rflp restriction fragment length polymorphism", "rflp", kw_final)) %>%
#   mutate(kw_final = gsub("richness specific", "species richness", kw_final)) %>%
#   mutate(kw_final = gsub("risk  forage trade-off", "risk-forage trade-off", kw_final)) %>%
#   mutate(kw_final = gsub("risk forage trade-off", "risk-forage trade-off", kw_final)) %>%
#   mutate(kw_final = gsub("river  watershed exchange", "river watershed exchange", kw_final)) %>%
#   mutate(kw_final = gsub("river floodplain", "floodplain river", kw_final)) %>%
#   mutate(kw_final = gsub("river paraná", "parana river", kw_final)) %>%
#   mutate(kw_final = gsub("rna dna ratio", "rna:dna ratio", kw_final)) %>%
#   mutate(kw_final = gsub("rna interference rnai", "rna interference", kw_final)) %>%
#   mutate(kw_final = gsub("rna-dna", "rna:dna", kw_final)) %>%
#   mutate(kw_final = gsub("rna: dna", "rna:dna", kw_final)) %>%
#   mutate(kw_final = gsub("rna : dna", "rna:dna", kw_final)) %>%
#   mutate(kw_final = gsub("rna-dna", "rna:dna", kw_final)) %>%
#   mutate(kw_final = gsub("road kills", "roadkill", kw_final)) %>%
#   mutate(kw_final = gsub("rock paper-scissors competition", "rock-paper-scissors competition", kw_final)) %>%
#   mutate(kw_final = gsub("rocky intertidal shores", "intertidal rocky shores", kw_final)) %>%
#   mutate(kw_final = gsub("root shoot ratios", "root:shoot ratios", kw_final)) %>%
#   mutate(kw_final = gsub("root to-shoot ratios", "root:shoot ratios", kw_final)) %>%
#   mutate(kw_final = gsub("root vertical distribution", "vertical root distribution", kw_final)) %>%
#   mutate(kw_final = gsub("root-ratio", "root:shoot ratio", kw_final)) %>%
#   mutate(kw_final = gsub("rupicapra pyrenaica pyrenaica", "rupicapra pyrenaica", kw_final)) %>%
#   mutate(kw_final = gsub("río negro", "rio negro", kw_final)) %>%
#   mutate(kw_final = gsub("s patial scale", "spatial scale", kw_final)) %>%
#   mutate(kw_final = gsub("s peciation", "speciation", kw_final)) %>%
#   mutate(kw_final = gsub("sagitta ria lancifolia", "sagittaria lancifolia", kw_final)) %>%
#   mutate(kw_final = gsub("salamandra salamandra", "salamandra", kw_final)) %>%
#   mutate(kw_final = gsub("san andrés island", "san andres island", kw_final)) %>%
#   mutate(kw_final = gsub("savanna-forest boundary", "savanna-forest boundary", kw_final)) %>%
#   mutate(kw_final = gsub("scale transitions", "scale transition", kw_final)) %>%
#   mutate(kw_final = gsub("scatter hoarding rodents", "scatterhoarding rodents", kw_final)) %>%
#   mutate(kw_final = gsub("scinc idae", "scincidae", kw_final)) %>%
#   mutate(kw_final = gsub("sea bird", "seabird", kw_final)) %>%
#   mutate(kw_final = gsub("sea birds", "seabirds", kw_final)) %>%
#   mutate(kw_final = gsub("sea water temperature", "seawater temperature", kw_final)) %>%
#   mutate(kw_final = gsub("seasonal burning", "aseasonal burning", kw_final)) %>%
#   mutate(kw_final = gsub("secondary plant compounds", "plant secondary compounds", kw_final)) %>%
#   mutate(kw_final = gsub("secondgrowth forest", "second growth forest", kw_final)) %>%
#   mutate(kw_final = gsub("second-ary forest", "secondary forest", kw_final)) %>%
#   mutate(kw_final = gsub("seed : ovule ratio", "seed:ovule ratio", kw_final)) %>%
#   mutate(kw_final = gsub("seed predation and dispersal", "seed dispersal and predation", kw_final)) %>%
#   mutate(kw_final = gsub("seed predation", "seed predation", kw_final)) %>%
#   mutate(kw_final = gsub("seed: ovule ratio", "seed:ovule ratio", kw_final)) %>%
#   mutate(kw_final = gsub("seedbank", "seed bank", kw_final)) %>%
#   mutate(kw_final = gsub("seedling survival and growth", "seedling growth and survival", kw_final)) %>%
#   mutate(kw_final = gsub("seedset", "seed set", kw_final)) %>%
#   mutate(kw_final = gsub("selection  artificial", "artificial selection", kw_final)) %>%
#   mutate(kw_final = gsub("selection  experimental", "experimental selection", kw_final)) %>%
#   mutate(kw_final = gsub("selection  group-kin", "group-kin selection  ", kw_final)) %>%
#   mutate(kw_final = gsub("selection-natural", "natural selection", kw_final)) %>%
#   mutate(kw_final = gsub("selection-sexual", "sexual selection", kw_final)) %>%
#   mutate(kw_final = gsub("self  fertilization", "self-fertilization", kw_final)) %>%
#   mutate(kw_final = gsub("self nonself recognition", "self-nonself recognition", kw_final)) %>%
#   mutate(kw_final = gsub("self-non self recognition", "self-nonself recognition", kw_final)) %>%
#   mutate(kw_final = gsub("selforganization", "self-organization", kw_final)) %>%
#   mutate(kw_final = gsub("semi arid grassland", "semiarid grassland", kw_final)) %>%
#   mutate(kw_final = gsub("semi arid woodland", "semiarid woodland", kw_final)) %>%
#   mutate(kw_final = gsub("semi balanus balanoides", "semibalanus balanoides", kw_final)) %>%
#   mutate(kw_final = gsub("semi deciduous", "semideciduous", kw_final)) %>%
#   mutate(kw_final = gsub("semi deciduous tropical forest", "tropical semideciduous forest", kw_final)) %>%
#   mutate(kw_final = gsub("semideciduous tropical forest", "tropical semideciduous forest", kw_final)) %>%
#   mutate(kw_final = gsub("seroepidemiolology", "seroepidemiology", kw_final)) %>%
#   mutate(kw_final = gsub("serpentin e", "serpentine", kw_final)) %>%
#   mutate(kw_final = gsub("sesquiterpene lactones", "sesquiterpene lactone", kw_final)) %>%
#   mutate(kw_final = gsub("sessile invertebrates", "sessile invertebrate", kw_final)) %>%
#   mutate(kw_final = gsub("sexlimited polymorphism", "sex limited polymorphism", kw_final)) %>%
#   mutate(kw_final = gsub("sexspecific population dynamics", "sex specific population dynamics", kw_final)) %>%
#   mutate(kw_final = gsub("sexual ", "sexual", kw_final)) %>%
#   mutate(kw_final = gsub("sexually antagonistic co evolution", "sexually antagonistic coevolution", kw_final)) %>%
#   mutate(kw_final = gsub("sexually transmitted infections", "sexually transmitted infection", kw_final)) %>%
#   mutate(kw_final = gsub("shell fish", "shellfish", kw_final)) %>%
#   mutate(kw_final = gsub("short grass steppe", "shortgrass steppe", kw_final)) %>%
#   mutate(kw_final = gsub("shrimp by catch", "shrimp bycatch", kw_final)) %>%
#   mutate(kw_final = gsub("shrub desert", "desert shrub", kw_final)) %>%
#   mutate(kw_final = gsub("shrub grass competition", "grass-shrub competition", kw_final)) %>%
#   mutate(kw_final = gsub("shrub-steppe", "shrub-steppe", kw_final)) %>%
#   mutate(kw_final = gsub("sib ship reconstruction", "sibship reconstruction", kw_final)) %>%
#   mutate(kw_final = gsub("sierra nevada california", "california sierra nevada", kw_final)) %>%
#   mutate(kw_final = gsub("signal color", "color signal", kw_final)) %>%
#   mutate(kw_final = gsub("signaling-courtship", "signaling-courtship", kw_final)) %>%
#   mutate(kw_final = gsub("sink source", "source-sink", kw_final)) %>%
#   mutate(kw_final = gsub("sink source relationships", "source-sink relationships", kw_final)) %>%
#   mutate(kw_final = gsub("siskiyou klamath", "klamath siskiyou", kw_final)) %>%
#   mutate(kw_final = gsub("size ", "size", kw_final)) %>%
#   mutate(kw_final = gsub("size and age at maturity", "age and size at maturity", kw_final)) %>%
#   mutate(kw_final = gsub("size at-age", "size at age", kw_final)) %>%
#   mutate(kw_final = gsub("size-number trade off", "size number trade-off", kw_final)) %>%
#   mutate(kw_final = gsub("sizebias", "size bias", kw_final)) %>%
#   mutate(kw_final = gsub("size-number trade off", "size number trade-off", kw_final)) %>%
#   mutate(kw_final = gsub("slow growth-high-mortality hypothesis", "slow growth-high-mortality hypothesis", kw_final)) %>%
#   mutate(kw_final = gsub("small mam mals", "small mammals", kw_final)) %>%
#   mutate(kw_final = gsub("small mouth bass", "smallmouth bass", kw_final)) %>%
#   mutate(kw_final = gsub("snake bite", "snakebite", kw_final)) %>%
#   mutate(kw_final = gsub("socio ecological system", "socioecological system", kw_final)) %>%
#   mutate(kw_final = gsub("soft and hard selection", "hard and soft selection", kw_final)) %>%
#   mutate(kw_final = gsub("soft scale insect", "soft-scale insect", kw_final)) %>%
#   mutate(kw_final = gsub("soft scale insects", "soft-scale insect", kw_final)) %>%
#   mutate(kw_final = gsub("soil borne diseases", "soilborne diseases", kw_final)) %>%
#   mutate(kw_final = gsub("soil seedbank", "soil seed bank", kw_final)) %>%
#   mutate(kw_final = gsub("source sink population dynamics", "source-sink population dynamics", kw_final)) %>%
#   mutate(kw_final = gsub("source sink relationships", "source-sink relationships", kw_final)) %>%
#   mutate(kw_final = gsub("source-sink dynamics", "source sink dynamics", kw_final)) %>%
#   mutate(kw_final = gsub("south east brazil", "southeast brazil", kw_final)) %>%
#   mutate(kw_final = gsub("south eastern pacific", "eastern south pacific", kw_final)) %>%
#   mutate(kw_final = gsub("south western nigeria", "southwestern nigeria", kw_final)) %>%
#   mutate(kw_final = gsub("sp atial pattern", "spatial pattern", kw_final)) %>%
#   mutate(kw_final = gsub("space for time substitution", "space-for-time substitution", kw_final)) %>%
#   mutate(kw_final = gsub("spatial auto regression", "spatial autoregression", kw_final)) %>%
#   mutate(kw_final = gsub("spatial capturerecapture", "spatial capture-recapture", kw_final)) %>%
#   mutate(kw_final = gsub("spatial genetic autocorrelation", "genetic spatial autocorrelation", kw_final)) %>%
#   mutate(kw_final = gsub("spatial pattern distribution", "spatial distribution pattern", kw_final)) %>%
#   mutate(kw_final = gsub("spatial temporal variability", "spatial-temporal variability", kw_final)) %>%
#   mutate(kw_final = gsub("spatio temporal heterogeneity", "spatiotemporal heterogeneity", kw_final)) %>%
#   mutate(kw_final = gsub("spatio temporal model", "spatiotemporal model", kw_final)) %>%
#   mutate(kw_final = gsub("spe cies richness", "species richness", kw_final)) %>%
#   mutate(kw_final = gsub("specialist generalist trade-offs", "generalist-specialist trade-offs", kw_final)) %>%
#   mutate(kw_final = gsub("specialist generalist tradeoffs", "generalist-specialist trade-offs", kw_final)) %>%
#   mutate(kw_final = gsub("specialist vs generalist", "generalist vs specialist", kw_final)) %>%
#   mutate(kw_final = gsub("specialists vsgeneralists", "specialists vs generalists", kw_final)) %>%
#   mutate(kw_final = gsub("species archetype", "species archetypes", kw_final)) %>%
#   mutate(kw_final = gsub("species area hypothesis", "species-area hypothesis", kw_final)) %>%
#   mutate(kw_final = gsub("species coexistences", "species coexistence", kw_final)) %>%
#   mutate(kw_final = gsub("species cooccurrences", "species cooccurrence", kw_final)) %>%
#   mutate(kw_final = gsub("species core", "core species", kw_final)) %>%
#   mutate(kw_final = gsub("species indicator", "indicator species", kw_final)) %>%
#   mutate(kw_final = gsub("species individual curve", "species-individual curve", kw_final)) %>%
#   mutate(kw_final = gsub("species individual-curves", "species-individual curve", kw_final)) %>%
#   mutate(kw_final = gsub("species phylogenetics", "phylogenetic species", kw_final)) %>%
#   mutate(kw_final = gsub("species range edges", "species range edge", kw_final)) %>%
#   mutate(kw_final = gsub("species tree distribution", "tree species distribution", kw_final)) %>%
#   mutate(kw_final = gsub("species -area relationship", "species-area relationship", kw_final)) %>%
#   mutate(kw_final = gsub("speciesabundance distributions", "species-abundance distributions", kw_final)) %>%
#   mutate(kw_final = gsub("species-energy", "species energy", kw_final)) %>%
#   mutate(kw_final = gsub("specific richness", "species richness", kw_final)) %>%
#   mutate(kw_final = gsub("spermcompetition", "sperm competition", kw_final)) %>%
#   mutate(kw_final = gsub("spider silks", "silk spiders", kw_final)) %>%
#   mutate(kw_final = gsub("spill over", "spillover", kw_final)) %>%
#   mutate(kw_final = gsub("stable carbon and nitrogen isotopes", "carbon and nitrogen stable isotopes", kw_final)) %>%
#   mutate(kw_final = gsub("stable carbon isotope", "carbon stable isotope", kw_final)) %>%
#   mutate(kw_final = gsub("stable carbon isotope ratios", "stable carbon-isotope ratios", kw_final)) %>%
#   mutate(kw_final = gsub("stable hydrogen isotopes", "hydrogen stable isotopes", kw_final)) %>%
#   mutate(kw_final = gsub("stable isotope  n 15", "15n stable isotope", kw_final)) %>%
#   mutate(kw_final = gsub("stable nitrogen isotopes", "nitrogen stable isotopes", kw_final)) %>%
#   mutate(kw_final = gsub("stage age structure", "age-stage structure", kw_final)) %>%
#   mutate(kw_final = gsub("stage specific vital rate", "stage-specific vital rate", kw_final)) %>%
#   mutate(kw_final = gsub("stage specific vital rates", "stage-specific vital rate", kw_final)) %>%
#   mutate(kw_final = gsub("stage structured", "stage-structured", kw_final)) %>%
#   mutate(kw_final = gsub("stage structured populations", "stage-structured populations", kw_final)) %>%
#   mutate(kw_final = gsub("stagestructured", "stage-structured", kw_final)) %>%
#   mutate(kw_final = gsub("stagestructured models", "stage-structured models", kw_final)) %>%
#   mutate(kw_final = gsub("stagestructured populations", "stage-structured populations", kw_final)) %>%
#   mutate(kw_final = gsub("state and-transition model", "state and transition model", kw_final)) %>%
#   mutate(kw_final = gsub("stink bugs", "stinkbugs", kw_final)) %>%
#   mutate(kw_final = gsub("stochastic blockmodel", "stochastic block model", kw_final)) %>%
#   mutate(kw_final = gsub("stress hormones", "stress hormone", kw_final)) %>%
#   mutate(kw_final = gsub("stress responses", "stress response", kw_final)) %>%
#   mutate(kw_final = gsub("strix occidentalis occidentalis", "strix occidentalis", kw_final)) %>%
#   mutate(kw_final = gsub("structural equation modelingsem", "structural equation modeling", kw_final)) %>%
#   mutate(kw_final = gsub("structure community", "community structure", kw_final)) %>%
#   mutate(kw_final = gsub("structure function", "structure-function", kw_final)) %>%
#   mutate(kw_final = gsub("structure population", "population structure", kw_final)) %>%
#   mutate(kw_final = gsub("structure size", "size structure", kw_final)) %>%
#   mutate(kw_final = gsub("structure vegetation", "vegetation structure", kw_final)) %>%
#   mutate(kw_final = gsub("sub antarctic", "subantarctic", kw_final)) %>%
#   mutate(kw_final = gsub("sub humid tropics", "subhumid tropics", kw_final)) %>%
#   mutate(kw_final = gsub("sub lethal effects", "sublethal effects", kw_final)) %>%
#   mutate(kw_final = gsub("subarctic salt marsh", "subarctic saltmarsh", kw_final)) %>%
#   mutate(kw_final = gsub("subtropical evergreen broad leaved forest", "subtropical evergreen broadleaved forest", kw_final)) %>%
#   mutate(kw_final = gsub("sub-saharan africa", "sub-saharan africa", kw_final)) %>%
#   mutate(kw_final = gsub("succession rates", "succession rate", kw_final)) %>%
#   mutate(kw_final = gsub("super population", "superpopulation", kw_final)) %>%
#   mutate(kw_final = gsub("superspreaders", "superspreader", kw_final)) %>%
#   mutate(kw_final = gsub("surface area to-volume ratio", "surface area to volume ratio", kw_final)) %>%
#   mutate(kw_final = gsub("t ropics", "tropics", kw_final)) %>%
#   mutate(kw_final = gsub("tall grass prairie", "tallgrass prairie", kw_final)) %>%
#   mutate(kw_final = gsub("taylors powerlaw", "taylors power law", kw_final)) %>%
#   mutate(kw_final = gsub("tehuacán valley", "tehuacan valley", kw_final)) %>%
#   mutate(kw_final = gsub("temperate tropical comparison", "temperate vs tropical", kw_final)) %>%
#   mutate(kw_final = gsub("temperature  dependent sex determination", "temperature-dependent sex determination", kw_final)) %>%
#   mutate(kw_final = gsub("temperature size-rule", "temperature size rule", kw_final)) %>%
#   mutate(kw_final = gsub("temporal and spatial distribution", "spatial and temporal distribution", kw_final)) %>%
#   mutate(kw_final = gsub("temporal and spatial scale", "spatial and temporal scale", kw_final)) %>%
#   mutate(kw_final = gsub("temporal and spatial variation", "spatial and temporal variation", kw_final)) %>%
#   mutate(kw_final = gsub("terraba sierpe", "sierpe térraba", kw_final)) %>%
#   mutate(kw_final = gsub("terrestrial gasteropods", "terrestrial gastropods", kw_final)) %>%
#   mutate(kw_final = gsub("territorial signals", "territorial signal", kw_final)) %>%
#   mutate(kw_final = gsub("territory establishment and quality", "territory quality and establishment", kw_final)) %>%
#   mutate(kw_final = gsub("testoster one", "testosterone", kw_final)) %>%
#   mutate(kw_final = gsub("tetrahymena tetrahymena", "tetrahymena", kw_final)) %>%
#   mutate(kw_final = gsub("th ermoregulation", "thermoregulation", kw_final)) %>%
#   mutate(kw_final = gsub("thermal co adaptation", "thermal coadaptation", kw_final)) %>%
#   mutate(kw_final = gsub("thermo tolerance", "thermotolerance", kw_final)) %>%
#   mutate(kw_final = gsub("time scale", "timescale", kw_final)) %>%
#   mutate(kw_final = gsub("time scales", "timescale", kw_final)) %>%
#   mutate(kw_final = gsub("time-activity budgets", "time activity budgets", kw_final)) %>%
#   mutate(kw_final = gsub("tolerance fecundity trade-off", "tolerance-fecundity trade off", kw_final)) %>%
#   mutate(kw_final = gsub("top down vs bottom up effects", "top=down vs bottom-up effects", kw_final)) %>%
#   mutate(kw_final = gsub("top down vs bottom-up effects", "top=down vs bottom-up effects", kw_final)) %>%
#   mutate(kw_final = gsub("top kill", "topkill", kw_final)) %>%
#   mutate(kw_final = gsub("top soil", "topsoil", kw_final)) %>%
#   mutate(kw_final = gsub("topdown forces", "top-down forces", kw_final)) %>%
#   mutate(kw_final = gsub("topdown limitation", "top-down limitation", kw_final)) %>%
#   mutate(kw_final = gsub("toppredator", "top predator", kw_final)) %>%
#   mutate(kw_final = gsub("torus translations", "torus translation", kw_final)) %>%
#   mutate(kw_final = gsub("tracking climate", "climate tracking", kw_final)) %>%
#   mutate(kw_final = gsub("trade offs of food and safety", "tradeoffs of food and safety", kw_final)) %>%
#   mutate(kw_final = gsub("trait environment interaction", "trait-environment interaction", kw_final)) %>%
#   mutate(kw_final = gsub("trait shifts", "trait shift", kw_final)) %>%
#   mutate(kw_final = gsub("traitmediated indirect interaction", "trait-mediated indirect interaction", kw_final)) %>%
#   mutate(kw_final = gsub("trans generation", "transgeneration", kw_final)) %>%
#   mutate(kw_final = gsub("transfer functions", "transfer function", kw_final)) %>%
#   mutate(kw_final = gsub("transformations", "transformation", kw_final)) %>%
#   mutate(kw_final = gsub("transgressive over yielding", "transgressive overyielding", kw_final)) %>%
#   mutate(kw_final = gsub("translocations", "translocation", kw_final)) %>%
#   mutate(kw_final = gsub("transmis sion", "transmission", kw_final)) %>%
#   mutate(kw_final = gsub("transmission virulence tradeoff", "transmission virulence trade-off", kw_final)) %>%
#   mutate(kw_final = gsub("tree grass balance", "tree-grass coexistance", kw_final)) %>%
#   mutate(kw_final = gsub("tree liana competition", "liana-tree competition", kw_final)) %>%
#   mutate(kw_final = gsub("tree liana interaction", "liana-tree interaction", kw_final)) %>%
#   mutate(kw_final = gsub("tree regression analysis", "regression tree analysis", kw_final)) %>%
#   mutate(kw_final = gsub("tree survival and growth", "tree growth and survival", kw_final)) %>%
#   mutate(kw_final = gsub("tree-grass balance", "tree-grass coexistance", kw_final)) %>%
#   mutate(kw_final = gsub("tree-grass co existence", "tree-grass coexistence", kw_final)) %>%
#   mutate(kw_final = gsub("tree-grass interactions", "tree-grass interactions", kw_final)) %>%
#   mutate(kw_final = gsub("tree-grass interactions", "tree-grass interactions", kw_final)) %>%
#   mutate(kw_final = gsub("tri trophic", "tritrophic", kw_final)) %>%
#   mutate(kw_final = gsub("tri trophic system", "tritrophic system", kw_final)) %>%
#   mutate(kw_final = gsub("tropical agro ecosystem", "tropical agroecosystem", kw_final)) %>%
#   mutate(kw_final = gsub("tropical high mountains", "high tropical mountains", kw_final)) %>%
#   mutate(kw_final = gsub("tropical primary forest", "primary tropical forest", kw_final)) %>%
#   mutate(kw_final = gsub("tropical sub montane forest", "tropical submontane forest", kw_final)) %>%
#   mutate(kw_final = gsub("truss box", "box truss", kw_final)) %>%
#   mutate(kw_final = gsub("tube worm", "tubeworm", kw_final)) %>%
#   mutate(kw_final = gsub("térraba", "terraba", kw_final)) %>%
#   mutate(kw_final = gsub("túngara frog", "tungara frog", kw_final)) %>%
#   mutate(kw_final = gsub("ultra structure", "ultrastructure", kw_final)) %>%
#   mutate(kw_final = gsub("under dispersion", "underdispersion", kw_final)) %>%
#   mutate(kw_final = gsub("under sampling", "undersampling", kw_final)) %>%
#   mutate(kw_final = gsub("up scaling", "scaling up", kw_final)) %>%
#   mutate(kw_final = gsub("urban rural gradient", "urban-rural gradient", kw_final)) %>%
#   mutate(kw_final = gsub("variance mean relationship", "mean-variance relationship", kw_final)) %>%
#   mutate(kw_final = gsub("viability ", "viability", kw_final)) %>%
#   mutate(kw_final = gsub("walkingsticks", "walking sticks", kw_final)) %>%
#   mutate(kw_final = gsub("water borne cues", "waterborne cues", kw_final)) %>%
#   mutate(kw_final = gsub("water frogs", "waterfrogs", kw_final)) %>%
#   mutate(kw_final = gsub("water holes", "waterholes", kw_final)) %>%
#   mutate(kw_final = gsub("waterstress", "water stress", kw_final)) %>%
#   mutate(kw_final = gsub("weaver spiders", "weaver spider", kw_final)) %>%
#   mutate(kw_final = gsub("weddell seals", "weddell seal", kw_final)) %>%
#   mutate(kw_final = gsub("weight length relationship", "length-weight relationship", kw_final)) %>%
#   mutate(kw_final = gsub("west central méxico", "west central mexico", kw_final)) %>%
#   mutate(kw_final = gsub("wet evergreen tropical forest", "tropical wet evergreen forest", kw_final)) %>%
#   mutate(kw_final = gsub("white spruce  picea glauca ", "white spruce picea glauca ", kw_final)) %>%
#   mutate(kw_final = gsub("white spruce picea glauca", "white spruce picea glauca ", kw_final)) %>%
#   mutate(kw_final = gsub("wild fires", "wildfires", kw_final)) %>%
#   mutate(kw_final = gsub("wind speeds", "wind speed", kw_final)) %>%
#   mutate(kw_final = gsub("wind storm", "windstorm", kw_final)) %>%
#   mutate(kw_final = gsub("wind throw", "windthrow", kw_final)) %>%
#   mutate(kw_final = gsub("wing thorax ratio", "wing:thorax ratio", kw_final)) %>%
#   mutate(kw_final = gsub("winner loser effect", "winner-loser effect", kw_final)) %>%
#   mutate(kw_final = gsub("within host in-teractions", "within host interactions", kw_final)) %>%
#   mutate(kw_final = gsub("withinhost dynamics", "within-host dynamics", kw_final)) %>%
#   mutate(kw_final = gsub("wood ", "wood", kw_final)) %>%
#   mutate(kw_final = gsub("x y recombination", "xy recombination", kw_final)) %>%
#   mutate(kw_final = gsub("xel há", "xel ha", kw_final)) %>%
#   mutate(kw_final = gsub("y ield", "yield", kw_final)) %>%
#   mutate(kw_final = gsub("yplant", "y plant", kw_final)) %>%
#   mutate(kw_final = gsub("yucatán península", "yucatan peninsula", kw_final)) %>%
#   mutate(kw_final = gsub("yucca yucca moth", "yucca moth", kw_final)) %>%
#   mutate(kw_final = gsub("z imbabwe", "zimbabwe", kw_final)) %>%
#   mutate(kw_final = gsub("zvariegatus", "z variegatus", kw_final)) %>%
#   mutate(kw_final = gsub("δ n natural abundance 15", "natural abundance δ n15", kw_final)) %>%
#   mutate(kw_final = gsub("ecosystem functioning", "ecosystem function", kw_final)) %>%
#   # mutate(kw_final = gsub("amazon", "amazonia", kw_final)) %>%
#   mutate(kw_final = gsub("modeling", "model", kw_final)) %>%
#   mutate(kw_final = gsub("biodiversity and ecosystem function", "biodiversity-ecosystem function", kw_final)) %>%
#   mutate(kw_final = gsub("biodiversity and ecosystem service", "biodiversity-ecosystem function", kw_final)) %>%
#   mutate(kw_final = gsub("biodiversity and ecosystem function relationship", "biodiversity-ecosystem function", kw_final)) %>%
#   mutate(kw_final = gsub("biodiversity and ecosystem functioning theory", "biodiversity-ecosystem function", kw_final)) %>%
#   mutate(kw_final = gsub("biodiversity ecosystem function bdef", "biodiversity-ecosystem function", kw_final)) %>%
#   mutate(kw_final = gsub("biodiversity-ecosystem function bef", "biodiversity-ecosystem function", kw_final)) %>%
#   mutate(kw_final = gsub("yasuni ecological research station", "yasuni", kw_final)) %>%
#   mutate(kw_final = gsub("la selva biological station", "la selva", kw_final)) %>%
#   mutate(kw_final = gsub("santa rosa", "santa rosa national park", kw_final))


# remove plurals ----------------------------------------------------------


  
keywords <- keywords %>%
  mutate(kw_final = gsub("interactions", "interaction", kw_final)) %>%
  mutate(kw_final = gsub("areas", "area", kw_final)) %>%
  mutate(kw_final = gsub("approximations", "approximation", kw_final)) %>%
  mutate(kw_final = gsub("hypotheses", "hypothesis", kw_final)) %>%
  mutate(kw_final = gsub("communities", "community", kw_final)) %>%
  mutate(kw_final = gsub("dynamics", "dynamic", kw_final)) %>%
  mutate(kw_final = gsub("tradeoffs", "tradeoff", kw_final)) %>%
  mutate(kw_final = gsub("mammals", "mammal", kw_final)) %>%
  mutate(kw_final = gsub("traits", "trait", kw_final)) %>%
  mutate(kw_final = gsub("hotspots", "hotspot", kw_final)) %>%
  mutate(kw_final = gsub("experiments", "experiment", kw_final)) %>%
  mutate(kw_final = gsub("products", "product", kw_final)) %>%
  mutate(kw_final = gsub("feedbacks", "feedback", kw_final)) %>%
  mutate(kw_final = gsub("models", "model", kw_final)) %>%
  mutate(kw_final = gsub("effects", "effect", kw_final)) %>%
  mutate(kw_final = gsub("predators", "predator", kw_final)) %>%
  mutate(kw_final = gsub("populations", "population", kw_final)) %>%
  mutate(kw_final = gsub("systems", "system", kw_final)) %>%
  mutate(kw_final = gsub("distributions", "distribution", kw_final)) %>%
  mutate(kw_final = gsub("linkages", "linkage", kw_final)) %>%
  mutate(kw_final = gsub("pools", "pool", kw_final)) %>%
  mutate(kw_final = gsub("simulations", "simulation", kw_final)) %>%
  mutate(kw_final = gsub("microorganisms", "microorganism", kw_final)) %>%
  mutate(kw_final = gsub("pathogens", "pathogen", kw_final)) %>%
  mutate(kw_final = gsub("tannins", "tannin", kw_final)) %>%
  mutate(kw_final = gsub("islands", "island", kw_final)) %>%
  mutate(kw_final = gsub("ants", "ant", kw_final)) %>%
  mutate(kw_final = gsub("treefalls", "treefall", kw_final)) %>%
  mutate(kw_final = gsub("factors", "factor", kw_final)) %>%
  mutate(kw_final = gsub("landscapes", "landscape", kw_final)) %>%
  mutate(kw_final = gsub("networks", "network", kw_final)) %>%
  mutate(kw_final = gsub("frogs", "frog", kw_final)) %>%
  mutate(kw_final = gsub("polymorphisms", "polymorphism", kw_final)) %>%
  mutate(kw_final = gsub("defenses", "defense", kw_final)) %>%
  mutate(kw_final = gsub("resources", "resource", kw_final)) %>%
  mutate(kw_final = gsub("litterbags", "litterbag", kw_final)) %>%
  mutate(kw_final = gsub("responses", "response", kw_final)) %>%
  mutate(kw_final = gsub("relationships", "relationship", kw_final)) %>%
  mutate(kw_final = gsub("grasslands", "grassland", kw_final)) %>%
  mutate(kw_final = gsub("forests", "forest", kw_final)) %>%
  mutate(kw_final = gsub("ratios", "ratio", kw_final)) %>%
  mutate(kw_final = gsub("outbreaks", "outbreak", kw_final)) %>%
  mutate(kw_final = gsub("services", "service", kw_final)) %>%
  mutate(kw_final = gsub("nectaries", "nectary", kw_final)) %>%
  mutate(kw_final = gsub("analyses", "analysis", kw_final)) %>%
  mutate(kw_final = gsub("gradients", "gradient", kw_final)) %>%
  mutate(kw_final = gsub("invasions", "invasion", kw_final)) %>%
  mutate(kw_final = gsub("correlations", "correlation", kw_final)) %>%
  mutate(kw_final = gsub("methods", "method", kw_final)) %>%
  mutate(kw_final = gsub("functions", "function", kw_final)) %>%
  mutate(kw_final = gsub("parasitoids", "parasitoid", kw_final)) %>%
  mutate(kw_final = gsub("amphibians", "amphibian", kw_final)) %>%
  mutate(kw_final = gsub("herbivores", "herbivore", kw_final)) %>%
  mutate(kw_final = gsub("lizards", "lizard", kw_final)) %>%
  mutate(kw_final = gsub("parasites", "parasite", kw_final)) %>%
  mutate(kw_final = gsub("aflps", "aflp", kw_final)) %>%
  mutate(kw_final = gsub("food webs", "food web", kw_final)) %>%
  mutate(kw_final = gsub("trees", "tree", kw_final)) %>%
  mutate(kw_final = gsub("contrasts", "contrast", kw_final)) %>%
  mutate(kw_final = gsub("spacers", "spacer", kw_final)) %>%
  mutate(kw_final = gsub("phenotypes", "phenotype", kw_final)) %>%
  mutate(kw_final = gsub("volatiles", "volatile", kw_final)) %>%
  mutate(kw_final = gsub("units", "unit", kw_final)) %>%
  mutate(kw_final = gsub("ipms", "ipm", kw_final)) %>%
  mutate(kw_final = gsub("organisms", "organism", kw_final)) %>%
  mutate(kw_final = gsub("breeders", "breeder", kw_final)) %>%
  mutate(kw_final = gsub("norms", "norm", kw_final)) %>%
  mutate(kw_final = gsub("rates", "rate", kw_final)) %>%
  mutate(kw_final = gsub("ranges", "range", kw_final)) %>%
  mutate(kw_final = gsub("conflicts", "conflict", kw_final)) %>%
  mutate(kw_final = gsub("associations", "association", kw_final)) %>%
  mutate(kw_final = gsub("values", "value", kw_final)) %>%
  mutate(kw_final = gsub("benefits", "benefit", kw_final)) %>%
  mutate(kw_final = gsub("birds", "bird", kw_final)) %>%
  mutate(kw_final = gsub("cheaters", "cheater", kw_final)) %>%
  mutate(kw_final = gsub("disturbances", "disturbance", kw_final)) %>% 
  mutate(kw_final = gsub("palaeogenetics","paleogenetics", kw_final)) %>%
  mutate(kw_final = gsub("palaeogenomics","paleogenomics", kw_final)) %>%
  mutate(kw_final = gsub("archipelagoes","archipelagos", kw_final)) %>%
  mutate(kw_final = gsub("coelocmocytes","coelomocytes", kw_final)) %>%
  mutate(kw_final = gsub("morphometics","morphometrics", kw_final)) %>%
  mutate(kw_final = gsub("palaeotropics","paleotropics", kw_final)) %>%
  mutate(kw_final = gsub("caerulescens","caeruleseens", kw_final)) %>%
  mutate(kw_final = gsub("oligochaetes","oligochaets", kw_final)) %>%
  mutate(kw_final = gsub("enrichmens","enrichments", kw_final)) %>%
  mutate(kw_final = gsub("flavanoids","flavonoids", kw_final)) %>%
  mutate(kw_final = gsub("mangrooves","mangroves", kw_final)) %>%
  mutate(kw_final = gsub("mesocosmos","mesocosms", kw_final)) %>%
  mutate(kw_final = gsub("milipedes","millipedes", kw_final)) %>%
  mutate(kw_final = gsub("psitacids","psittacids", kw_final)) %>%
  mutate(kw_final = gsub("scorpiones","scorpions", kw_final)) %>%
  mutate(kw_final = gsub("allozymes","alozymes", kw_final)) %>%
  mutate(kw_final = gsub("volcanoes","volcanos", kw_final)) %>%
  mutate(kw_final = gsub("mollusks","molluscs", kw_final)) %>%
  mutate(kw_final = gsub("moluscs","molluscs", kw_final)) %>%
  mutate(kw_final = gsub("opossums","possums", kw_final)) %>%
  mutate(kw_final = gsub("genetics","genetic", kw_final)) %>%
  mutate(kw_final = gsub("microsatellites","microsatellite", kw_final)) %>%
  mutate(kw_final = gsub("neotropics","neotropic", kw_final)) %>%
  mutate(kw_final = gsub("isotopes","isotope", kw_final)) %>%
  mutate(kw_final = gsub("nutrients","nutrient", kw_final)) %>%
  mutate(kw_final = gsub("morphometrics","morphometric", kw_final)) %>%
  mutate(kw_final = gsub("insects","insect", kw_final)) %>%
  mutate(kw_final = gsub("tropics","tropic", kw_final)) %>%
  mutate(kw_final = gsub("allozymes","allozyme", kw_final)) %>%
  mutate(kw_final = gsub("rodents","rodent", kw_final)) %>%
  mutate(kw_final = gsub("phylogenetics","phylogenetic", kw_final)) %>%
  mutate(kw_final = gsub("anurans","anuran", kw_final)) %>%
  mutate(kw_final = gsub("groups","group", kw_final)) %>%
  mutate(kw_final = gsub("lianas","liana", kw_final)) %>%
  mutate(kw_final = gsub("primates","primate", kw_final)) %>%
  mutate(kw_final = gsub("seedlings","seedling", kw_final)) %>%
  mutate(kw_final = gsub("wetlands","wetland", kw_final)) %>%
  mutate(kw_final = gsub("epiphytes","epiphyte", kw_final)) %>%
  mutate(kw_final = gsub("streams","stream", kw_final)) %>%
  mutate(kw_final = gsub("energetics","energetic", kw_final)) %>%
  mutate(kw_final = gsub("ungulates","ungulate", kw_final)) %>%
  mutate(kw_final = gsub("hydrocarbons","hydrocarbon", kw_final)) %>%
  mutate(kw_final = gsub("cascades","cascade", kw_final)) %>%
  mutate(kw_final = gsub("cycles","cycle", kw_final)) %>%
  mutate(kw_final = gsub("chromosomes","chromosome", kw_final)) %>%
  mutate(kw_final = gsub("mangroves","mangrove", kw_final)) %>%
  mutate(kw_final = gsub("constraints","constraint", kw_final)) %>%
  mutate(kw_final = gsub("arthropods","arthropod", kw_final)) %>%
  mutate(kw_final = gsub("pollinators","pollinator", kw_final)) %>%
  mutate(kw_final = gsub("reptiles","reptile", kw_final)) %>%
  mutate(kw_final = gsub("patterns","pattern", kw_final)) %>%
  mutate(kw_final = gsub("diatoms","diatom", kw_final)) %>%
  mutate(kw_final = gsub("spiders","spider", kw_final)) %>%
  mutate(kw_final = gsub("biomechanics","biomechanic", kw_final)) %>%
  mutate(kw_final = gsub("fishes","fishe", kw_final)) %>%
  mutate(kw_final = gsub("aphids","aphid", kw_final)) %>%
  mutate(kw_final = gsub("bryophytes","bryophyte", kw_final)) %>%
  mutate(kw_final = gsub("carotenoids","carotenoid", kw_final)) %>%
  mutate(kw_final = gsub("mutations","mutation", kw_final)) %>%
  mutate(kw_final = gsub("grasses","grass", kw_final)) %>%
  mutate(kw_final = gsub("beetles","beetle", kw_final)) %>%
  mutate(kw_final = gsub("genomics","genomic", kw_final)) %>%
  mutate(kw_final = gsub("hurricanes","hurricane", kw_final)) %>%
  mutate(kw_final = gsub("clines","cline", kw_final)) %>%
  mutate(kw_final = gsub("records","record", kw_final)) %>%
  mutate(kw_final = gsub("tadpoles","tadpole", kw_final)) %>%
  mutate(kw_final = gsub("differences","difference", kw_final)) %>%
  mutate(kw_final = gsub("metabolites","metabolite", kw_final)) %>%
  mutate(kw_final = gsub("termites","termite", kw_final)) %>%
  mutate(kw_final = gsub("crickets","cricket", kw_final)) %>%
  mutate(kw_final = gsub("statistics","statistic", kw_final)) %>%
  mutate(kw_final = gsub("components","component", kw_final)) %>%
  mutate(kw_final = gsub("legumes","legume", kw_final)) %>%
  mutate(kw_final = gsub("limits","limit", kw_final)) %>%
  mutate(kw_final = gsub("corals","coral", kw_final)) %>%
  mutate(kw_final = gsub("marsupials","marsupial", kw_final)) %>%
  mutate(kw_final = gsub("microbes","microbe", kw_final)) %>%
  mutate(kw_final = gsub("systematics","systematic", kw_final)) %>%
  mutate(kw_final = gsub("fossils","fossil", kw_final)) %>%
  mutate(kw_final = gsub("microcosms","microcosm", kw_final)) %>%
  mutate(kw_final = gsub("mollusks","mollusk", kw_final)) %>%
  mutate(kw_final = gsub("angiosperms","angiosperm", kw_final)) %>%
  mutate(kw_final = gsub("peatlands","peatland", kw_final)) %>%
  mutate(kw_final = gsub("protists","protist", kw_final)) %>%
  mutate(kw_final = gsub("snakes","snake", kw_final)) %>%
  mutate(kw_final = gsub("habits","habit", kw_final)) %>%
  mutate(kw_final = gsub("helminths","helminth", kw_final)) %>%
  mutate(kw_final = gsub("bivalves","bivalve", kw_final)) %>%
  mutate(kw_final = gsub("conditions","condition", kw_final)) %>%
  mutate(kw_final = gsub("frugivores","frugivore", kw_final)) %>%
  mutate(kw_final = gsub("nematodes","nematode", kw_final)) %>%
  mutate(kw_final = gsub("rotifers","rotifer", kw_final)) %>%
  mutate(kw_final = gsub("turtles","turtle", kw_final)) %>%
  mutate(kw_final = gsub("urchins","urchin", kw_final)) %>%
  mutate(kw_final = gsub("bioenergetics","bioenergetic", kw_final)) %>%
  mutate(kw_final = gsub("processes","process", kw_final)) %>%
  mutate(kw_final = gsub("snails","snail", kw_final)) %>%
  mutate(kw_final = gsub("carnivores","carnivore", kw_final)) %>%
  mutate(kw_final = gsub("earthworms","earthworm", kw_final)) %>%
  mutate(kw_final = gsub("engineers","engineer", kw_final)) %>%
  mutate(kw_final = gsub("macrophytes","macrophyte", kw_final)) %>%
  mutate(kw_final = gsub("mussels","mussel", kw_final)) %>%
  mutate(kw_final = gsub("orchids","orchid", kw_final)) %>%
  mutate(kw_final = gsub("phenolics","phenolic", kw_final)) %>%
  mutate(kw_final = gsub("relations","relation", kw_final)) %>%
  mutate(kw_final = gsub("simulans","simulan", kw_final)) %>%
  mutate(kw_final = gsub("drylands","dryland", kw_final)) %>%
  mutate(kw_final = gsub("lipids","lipid", kw_final)) %>%
  mutate(kw_final = gsub("mutualisms","mutualism", kw_final)) %>%
  mutate(kw_final = gsub("polychaetes","polychaete", kw_final)) %>%
  mutate(kw_final = gsub("shifts","shift", kw_final)) %>%
  mutate(kw_final = gsub("shrubs","shrub", kw_final)) %>%
  mutate(kw_final = gsub("states","state", kw_final)) %>%
  mutate(kw_final = gsub("trichomes","trichome", kw_final)) %>%
  mutate(kw_final = gsub("copepods","copepod", kw_final)) %>%
  mutate(kw_final = gsub("gastropods","gastropod", kw_final)) %>%
  mutate(kw_final = gsub("saplings","sapling", kw_final)) %>%
  mutate(kw_final = gsub("variables","variable", kw_final)) %>%
  mutate(kw_final = gsub("geostatistics","geostatistic", kw_final)) %>%
  mutate(kw_final = gsub("grasshoppers","grasshopper", kw_final)) %>%
  mutate(kw_final = gsub("markers","marker", kw_final)) %>%
  mutate(kw_final = gsub("passerines","passerine", kw_final)) %>%
  mutate(kw_final = gsub("stocks","stock", kw_final)) %>%
  mutate(kw_final = gsub("stressors","stressor", kw_final)) %>%
  mutate(kw_final = gsub("bioacoustics","bioacoustic", kw_final)) %>%
  mutate(kw_final = gsub("epigenetics","epigenetic", kw_final)) %>%
  mutate(kw_final = gsub("metals","metal", kw_final)) %>%
  mutate(kw_final = gsub("seaweeds","seaweed", kw_final)) %>%
  mutate(kw_final = gsub("sediments","sediment", kw_final)) %>%
  mutate(kw_final = gsub("amphipods","amphipod", kw_final)) %>%
  mutate(kw_final = gsub("bumblebees","bumblebee", kw_final)) %>%
  mutate(kw_final = gsub("grazers","grazer", kw_final)) %>%
  mutate(kw_final = gsub("infections","infection", kw_final)) %>%
  mutate(kw_final = gsub("leaves","leaf", kw_final)) %>%
  mutate(kw_final = gsub("mycorrhizas","mycorrhizae", kw_final)) %>%
  mutate(kw_final = gsub("ornaments","ornament", kw_final)) %>%
  mutate(kw_final = gsub("syndromes","syndrome", kw_final)) %>%
  mutate(kw_final = gsub("bioindicators","bioindicator", kw_final)) %>%
  mutate(kw_final = gsub("bromeliads","bromeliad", kw_final)) %>%
  mutate(kw_final = gsub("corridors","corridor", kw_final)) %>%
  mutate(kw_final = gsub("fields","field", kw_final)) %>%
  mutate(kw_final = gsub("mosses","moss", kw_final)) %>%
  mutate(kw_final = gsub("specialists","specialist", kw_final)) %>%
  mutate(kw_final = gsub("anthocyanins","anthocyanin", kw_final)) %>%
  mutate(kw_final = gsub("detritivores","detritivore", kw_final)) %>%
  mutate(kw_final = gsub("economics","economic", kw_final)) %>%
  mutate(kw_final = gsub("ecotypes","ecotype", kw_final)) %>%
  mutate(kw_final = gsub("glucosinolates","glucosinolate", kw_final)) %>%
  mutate(kw_final = gsub("guilds","guild", kw_final)) %>%
  mutate(kw_final = gsub("mountains","mountain", kw_final)) %>%
  mutate(kw_final = gsub("ectotherms","ectotherm", kw_final)) %>%
  mutate(kw_final = gsub("events","event", kw_final)) %>%
  mutate(kw_final = gsub("floristics","floristic", kw_final)) %>%
  mutate(kw_final = gsub("fragments","fragment", kw_final)) %>%
  mutate(kw_final = gsub("generalists","generalist", kw_final)) %>%
  mutate(kw_final = gsub("impacts","impact", kw_final)) %>%
  mutate(kw_final = gsub("levels","level", kw_final)) %>%
  mutate(kw_final = gsub("lichens","lichen", kw_final)) %>%
  mutate(kw_final = gsub("myrmecophytes","myrmecophyte", kw_final)) %>%
  mutate(kw_final = gsub("pheromones","pheromone", kw_final)) %>%
  mutate(kw_final = gsub("plantations","plantation", kw_final)) %>%
  mutate(kw_final = gsub("wolves","wolf", kw_final)) %>%
  mutate(kw_final = gsub("barnacles","barnacle", kw_final)) %>%
  mutate(kw_final = gsub("bottlenecks","bottleneck", kw_final)) %>%
  mutate(kw_final = gsub("caterpillars","caterpillar", kw_final)) %>%
  mutate(kw_final = gsub("humans","human", kw_final)) %>%
  mutate(kw_final = gsub("hybrids","hybrid", kw_final)) %>%
  mutate(kw_final = gsub("isozymes","isozyme", kw_final)) %>%
  mutate(kw_final = gsub("microarthropods","microarthropod", kw_final)) %>%
  mutate(kw_final = gsub("pteridophytes","pteridophyte", kw_final)) %>%
  mutate(kw_final = gsub("raptors","raptor", kw_final)) %>%
  mutate(kw_final = gsub("crustaceans","crustacean", kw_final)) %>%
  mutate(kw_final = gsub("deserts","desert", kw_final)) %>%
  mutate(kw_final = gsub("echinoderms","echinoderm", kw_final)) %>%
  mutate(kw_final = gsub("finches","finch", kw_final)) %>%
  mutate(kw_final = gsub("flowers","flower", kw_final)) %>%
  mutate(kw_final = gsub("phylogenomics","phylogenomic", kw_final)) %>%
  mutate(kw_final = gsub("salamanders","salamander", kw_final)) %>%
  mutate(kw_final = gsub("sharks","shark", kw_final)) %>%
  mutate(kw_final = gsub("siderophores","siderophore", kw_final)) %>%
  mutate(kw_final = gsub("trends","trend", kw_final)) %>%
  mutate(kw_final = gsub("assemblages","assemblage", kw_final)) %>%
  mutate(kw_final = gsub("compounds","compound", kw_final)) %>%
  mutate(kw_final = gsub("conifers","conifer", kw_final)) %>%
  mutate(kw_final = gsub("miners","miner", kw_final)) %>%
  mutate(kw_final = gsub("mosquitoes","mosquitoe", kw_final)) %>%
  mutate(kw_final = gsub("pastures","pasture", kw_final)) %>%
  mutate(kw_final = gsub("psittacids","psittacid", kw_final)) %>%
  mutate(kw_final = gsub("rivers","river", kw_final)) %>%
  mutate(kw_final = gsub("clones","clone", kw_final)) %>%
  mutate(kw_final = gsub("fluctuations","fluctuation", kw_final)) %>%
  mutate(kw_final = gsub("generations","generation", kw_final)) %>%
  mutate(kw_final = gsub("glucocorticoids","glucocorticoid", kw_final)) %>%
  mutate(kw_final = gsub("habitats","habitat", kw_final)) %>%
  mutate(kw_final = gsub("honeybees","honeybee", kw_final)) %>%
  mutate(kw_final = gsub("insectivores","insectivore", kw_final)) %>%
  mutate(kw_final = gsub("inversions","inversion", kw_final)) %>%
  mutate(kw_final = gsub("juveniles","juvenile", kw_final)) %>%
  mutate(kw_final = gsub("pioneers","pioneer", kw_final)) %>%
  mutate(kw_final = gsub("preferences","preference", kw_final)) %>%
  mutate(kw_final = gsub("refuges","refuge", kw_final)) %>%
    mutate(kw_final = gsub("histories","history", kw_final)) %>%
  mutate(kw_final = gsub("consumers","consumer", kw_final)) %>%
  mutate(kw_final = gsub("declines","decline", kw_final)) %>%
  mutate(kw_final = gsub("dinoflagellates","dinoflagellate", kw_final)) %>%
  mutate(kw_final = gsub("drivers","driver", kw_final)) %>%
  mutate(kw_final = gsub("environments","environment", kw_final)) %>%
  mutate(kw_final = gsub("fruits","fruit", kw_final)) %>%
  mutate(kw_final = gsub("glycosides","glycoside", kw_final)) %>%
  mutate(kw_final = gsub("halophytes","halophyte", kw_final)) %>%
  mutate(kw_final = gsub("indicators","indicator", kw_final)) %>%
  mutate(kw_final = gsub("landslides","landslide", kw_final)) %>%
  mutate(kw_final = gsub("molluscs","mollusc", kw_final)) %>%
  mutate(kw_final = gsub("perceptions","perception", kw_final)) %>%
  mutate(kw_final = gsub("pinnipeds","pinniped", kw_final)) %>%
  mutate(kw_final = gsub("shrews","shrew", kw_final)) %>%
  mutate(kw_final = gsub("transcriptomics","transcriptomic", kw_final)) %>%
  mutate(kw_final = gsub("alkaloids","alkaloid", kw_final)) %>%
  mutate(kw_final = gsub("annuals","annual", kw_final)) %>%
  mutate(kw_final = gsub("cardenolides","cardenolide", kw_final)) %>%
  mutate(kw_final = gsub("chains","chain", kw_final)) %>%
  mutate(kw_final = gsub("changes","change", kw_final)) %>%
  mutate(kw_final = gsub("cichlids","cichlid", kw_final)) %>%
  mutate(kw_final = gsub("dipterocarps","dipterocarp", kw_final)) %>%
  mutate(kw_final = gsub("diseases","disease", kw_final)) %>%
  mutate(kw_final = gsub("enzymes","enzyme", kw_final)) %>%
  mutate(kw_final = gsub("geckos","gecko", kw_final)) %>%
  mutate(kw_final = gsub("himalayas","himalaya", kw_final)) %>%
  mutate(kw_final = gsub("microsites","microsite", kw_final)) %>%
  mutate(kw_final = gsub("movements","movement", kw_final)) %>%
  mutate(kw_final = gsub("reserves","reserve", kw_final)) %>%
  mutate(kw_final = gsub("salmonids","salmonid", kw_final)) %>%
  mutate(kw_final = gsub("shredders","shredder", kw_final)) %>%
  mutate(kw_final = gsub("thrips","thrip", kw_final)) %>%
  mutate(kw_final = gsub("zoonoses","zoonosis", kw_final)) %>%
  mutate(kw_final = gsub("approaches","approach", kw_final)) %>%
  mutate(kw_final = gsub("barriers","barrier", kw_final)) %>%
  mutate(kw_final = gsub("cladocerans","cladoceran", kw_final)) %>%
  mutate(kw_final = gsub("cytogenetics","cytogenetic", kw_final)) %>%
  mutate(kw_final = gsub("ectomycorrhizas","ectomycorrhiza", kw_final)) %>%
  mutate(kw_final = gsub("elements","element", kw_final)) %>%
  mutate(kw_final = gsub("endophytes","endophyte", kw_final)) %>%
  mutate(kw_final = gsub("equations","equation", kw_final)) %>%
  mutate(kw_final = gsub("flavonoids","flavonoid", kw_final)) %>%
  mutate(kw_final = gsub("hormones","hormone", kw_final)) %>%
  mutate(kw_final = gsub("hornbills","hornbill", kw_final)) %>%
  mutate(kw_final = gsub("mesocosms","mesocosm", kw_final)) %>%
  mutate(kw_final = gsub("metabolomics","metabolomic", kw_final)) %>%
  mutate(kw_final = gsub("microhabitats","microhabitat", kw_final)) %>%
  mutate(kw_final = gsub("numbers","number", kw_final)) %>%
  mutate(kw_final = gsub("regimes","regime", kw_final)) %>%
  mutate(kw_final = gsub("signals","signal", kw_final)) %>%
  mutate(kw_final = gsub("spores","spore", kw_final)) %>%
  mutate(kw_final = gsub("sticklebacks","stickleback", kw_final)) %>%
  mutate(kw_final = gsub("threats","threat", kw_final)) %>%
  mutate(kw_final = gsub("trematodes","trematode", kw_final)) %>%
  mutate(kw_final = gsub("weapons","weapon", kw_final)) %>%
  mutate(kw_final = gsub("borders","border", kw_final)) %>%
  mutate(kw_final = gsub("climbers","climber", kw_final)) %>%
  mutate(kw_final = gsub("contents","content", kw_final)) %>%
  mutate(kw_final = gsub("dinosaurs","dinosaur", kw_final)) %>%
  mutate(kw_final = gsub("exclosures","exclosure", kw_final)) %>%
  mutate(kw_final = gsub("exudates","exudate", kw_final)) %>%
  mutate(kw_final = gsub("fences","fence", kw_final)) %>%
  mutate(kw_final = gsub("gerbils","gerbil", kw_final)) %>%
  mutate(kw_final = gsub("helpers","helper", kw_final)) %>%
  mutate(kw_final = gsub("liverworts","liverwort", kw_final)) %>%
  mutate(kw_final = gsub("marshes","marsh", kw_final)) %>%
  mutate(kw_final = gsub("minerals","mineral", kw_final)) %>%
  mutate(kw_final = gsub("rearrangements","rearrangement", kw_final)) %>%
  mutate(kw_final = gsub("specimens","specimen", kw_final)) %>%
  mutate(kw_final = gsub("spines","spine", kw_final)) %>%
  mutate(kw_final = gsub("thresholds","threshold", kw_final)) %>%
  mutate(kw_final = gsub("viruses","viruse", kw_final)) %>%
  mutate(kw_final = gsub("waders","wader", kw_final)) %>%
  mutate(kw_final = gsub("attitudes","attitude", kw_final)) %>%
  mutate(kw_final = gsub("biofilms","biofilm", kw_final)) %>%
  mutate(kw_final = gsub("characters","character", kw_final)) %>%
  mutate(kw_final = gsub("characteristics","characteristic", kw_final)) %>%
  mutate(kw_final = gsub("cicadas","cicada", kw_final)) %>%
  mutate(kw_final = gsub("collections","collection", kw_final)) %>%
  mutate(kw_final = gsub("concepts","concept", kw_final)) %>%
  mutate(kw_final = gsub("consequences","consequence", kw_final)) %>%
  mutate(kw_final = gsub("decomposers","decomposer", kw_final)) %>%
  mutate(kw_final = gsub("exotics","exotic", kw_final)) %>%
  mutate(kw_final = gsub("extinctions","extinction", kw_final)) %>%
  mutate(kw_final = gsub("extremes","extreme", kw_final)) %>%
  mutate(kw_final = gsub("floods","flood", kw_final)) %>%
  mutate(kw_final = gsub("indices","index", kw_final)) %>%
  mutate(kw_final = gsub("individuals","individual", kw_final)) %>%
  mutate(kw_final = gsub("macrofossils","macrofossil", kw_final)) %>%
  mutate(kw_final = gsub("macronutrients","macronutrient", kw_final)) %>%
  mutate(kw_final = gsub("mesograzers","mesograzer", kw_final)) %>%
  mutate(kw_final = gsub("monkeys","monkey", kw_final)) %>%
  mutate(kw_final = gsub("odonates","odonate", kw_final)) %>%
  mutate(kw_final = gsub("otoliths","otolith", kw_final)) %>%
  mutate(kw_final = gsub("parameters","parameter", kw_final)) %>%
  mutate(kw_final = gsub("parrots","parrot", kw_final)) %>%
  mutate(kw_final = gsub("sanctions","sanction", kw_final)) %>%
  mutate(kw_final = gsub("seagrasses","seagrass", kw_final)) %>%
  mutate(kw_final = gsub("shadows","shadow", kw_final)) %>%
  mutate(kw_final = gsub("tactics","tactic", kw_final)) %>%
  mutate(kw_final = gsub("beaches","beach", kw_final)) %>%
  mutate(kw_final = gsub("bibliometrics","bibliometric", kw_final)) %>%
  mutate(kw_final = gsub("biomes","biome", kw_final)) %>%
  mutate(kw_final = gsub("cetaceans","cetacean", kw_final)) %>%
  mutate(kw_final = gsub("chewers","chewer", kw_final)) %>%
  mutate(kw_final = gsub("ciliates","ciliate", kw_final)) %>%
  mutate(kw_final = gsub("cladistics","cladistic", kw_final)) %>%
  mutate(kw_final = gsub("competitors","competitor", kw_final)) %>%
  mutate(kw_final = gsub("correlates","correlate", kw_final)) %>%
  mutate(kw_final = gsub("dolphins","dolphin", kw_final)) %>%
  mutate(kw_final = gsub("endosymbionts","endosymbiont", kw_final)) %>%
  mutate(kw_final = gsub("extracts","extract", kw_final)) %>%
  mutate(kw_final = gsub("filters","filter", kw_final)) %>%
  mutate(kw_final = gsub("gymnosperms","gymnosperm", kw_final)) %>%
  mutate(kw_final = gsub("hemiepiphytes","hemiepiphyte", kw_final)) %>%
  mutate(kw_final = gsub("hermaphrodites","hermaphrodite", kw_final)) %>%
  mutate(kw_final = gsub("lagoons","lagoon", kw_final)) %>%
  mutate(kw_final = gsub("lemurs","lemur", kw_final)) %>%
  mutate(kw_final = gsub("mechanisms","mechanism", kw_final)) %>%
  mutate(kw_final = gsub("metrics","metric", kw_final)) %>%
  mutate(kw_final = gsub("oysters","oyster", kw_final)) %>%
  mutate(kw_final = gsub("penguins","penguin", kw_final)) %>%
  mutate(kw_final = gsub("phenols","phenol", kw_final)) %>%
  mutate(kw_final = gsub("photographs","photograph", kw_final)) %>%
  mutate(kw_final = gsub("pigments","pigment", kw_final)) %>%
  mutate(kw_final = gsub("pipiens","pipien", kw_final)) %>%
  mutate(kw_final = gsub("rhizomes","rhizome", kw_final)) %>%
  mutate(kw_final = gsub("scorpions","scorpion", kw_final)) %>%
  mutate(kw_final = gsub("seastars","seastar", kw_final)) %>%
  mutate(kw_final = gsub("shrublands","shrubland", kw_final)) %>%
  mutate(kw_final = gsub("splendens","splenden", kw_final)) %>%
  mutate(kw_final = gsub("sponges","sponge", kw_final)) %>%
  mutate(kw_final = gsub("squamates","squamate", kw_final)) %>%
  mutate(kw_final = gsub("squirrels","squirrel", kw_final)) %>%
  mutate(kw_final = gsub("tables","table", kw_final)) %>%
  mutate(kw_final = gsub("toxins","toxin", kw_final)) %>%
  mutate(kw_final = gsub("visitors","visitor", kw_final)) %>%
  mutate(kw_final = gsub("adults","adult", kw_final)) %>%
  mutate(kw_final = gsub("allelochemicals","allelochemical", kw_final)) %>%
  mutate(kw_final = gsub("animals","animal", kw_final)) %>%
  mutate(kw_final = gsub("antibiotics","antibiotic", kw_final)) %>%
  mutate(kw_final = gsub("budgets","budget", kw_final)) %>%
  mutate(kw_final = gsub("cottonwoods","cottonwood", kw_final)) %>%
  mutate(kw_final = gsub("counts","count", kw_final)) %>%
  mutate(kw_final = gsub("cuttings","cutting", kw_final)) %>%
  mutate(kw_final = gsub("distances","distance", kw_final)) %>%
  mutate(kw_final = gsub("driptips","driptip", kw_final)) %>%
  mutate(kw_final = gsub("endemics","endemic", kw_final)) %>%
  mutate(kw_final = gsub("floodplains","floodplain", kw_final)) %>%
  mutate(kw_final = gsub("graminoids","graminoid", kw_final)) %>%
  mutate(kw_final = gsub("handicaps","handicap", kw_final)) %>%
  mutate(kw_final = gsub("hatchlings","hatchling", kw_final)) %>%
  mutate(kw_final = gsub("hazards","hazard", kw_final)) %>%
  mutate(kw_final = gsub("hydroids","hydroid", kw_final)) %>%
  mutate(kw_final = gsub("inputs","input", kw_final)) %>%
  mutate(kw_final = gsub("lemmings","lemming", kw_final)) %>%
  mutate(kw_final = gsub("modules","module", kw_final)) %>%
  mutate(kw_final = gsub("monoterpenes","monoterpene", kw_final)) %>%
  mutate(kw_final = gsub("morphs","morph", kw_final)) %>%
  mutate(kw_final = gsub("plains","plain", kw_final)) %>%
  mutate(kw_final = gsub("polyphenols","polyphenol", kw_final)) %>%
  mutate(kw_final = gsub("scientometrics","scientometric", kw_final)) %>%
  mutate(kw_final = gsub("seamounts","seamount", kw_final)) %>%
  mutate(kw_final = gsub("shrimps","shrimp", kw_final)) %>%
  mutate(kw_final = gsub("skinks","skink", kw_final)) %>%
  mutate(kw_final = gsub("terpenes","terpene", kw_final)) %>%
  mutate(kw_final = gsub("vectors","vector", kw_final)) %>%
  mutate(kw_final = gsub("vultures","vulture", kw_final)) %>%
  mutate(kw_final = gsub("aggregations","aggregation", kw_final)) %>%
  mutate(kw_final = gsub("aridlands","aridland", kw_final)) %>%
  mutate(kw_final = gsub("attractors","attractor", kw_final)) %>%
  mutate(kw_final = gsub("cardamines","cardamine", kw_final)) %>%
  mutate(kw_final = gsub("cations","cation", kw_final)) %>%
  mutate(kw_final = gsub("chironomids","chironomid", kw_final)) %>%
  mutate(kw_final = gsub("coefficients","coefficient", kw_final)) %>%
  mutate(kw_final = gsub("comparisons","comparison", kw_final)) %>%
  mutate(kw_final = gsub("differentials","differential", kw_final)) %>%
  mutate(kw_final = gsub("ecdysteroids","ecdysteroid", kw_final)) %>%
  mutate(kw_final = gsub("ecoinformatics","ecoinformatic", kw_final)) %>%
  mutate(kw_final = gsub("ecometabolomics","ecometabolomic", kw_final)) %>%
  mutate(kw_final = gsub("ecophylogenetics","ecophylogenetic", kw_final)) %>%
  mutate(kw_final = gsub("ecotones","ecotone", kw_final)) %>%
  mutate(kw_final = gsub("elasmobranchs","elasmobranch", kw_final)) %>%
  mutate(kw_final = gsub("epiphylls","epiphyll", kw_final)) %>%
  mutate(kw_final = gsub("equivalents","equivalent", kw_final)) %>%
  mutate(kw_final = gsub("eukaryotes","eukaryote", kw_final)) %>%
  mutate(kw_final = gsub("feathers","feather", kw_final)) %>%
  mutate(kw_final = gsub("felids","felid", kw_final)) %>%
  mutate(kw_final = gsub("floaters","floater", kw_final)) %>%
  mutate(kw_final = gsub("forces","force", kw_final)) %>%
  mutate(kw_final = gsub("gametophytes","gametophyte", kw_final)) %>%
  mutate(kw_final = gsub("gonads","gonad", kw_final)) %>%
  mutate(kw_final = gsub("gophers","gopher", kw_final)) %>%
  mutate(kw_final = gsub("groves","grove", kw_final)) %>%
  mutate(kw_final = gsub("hawkmoths","hawkmoth", kw_final)) %>%
  mutate(kw_final = gsub("immunoglobulins","immunoglobulin", kw_final)) %>%
  mutate(kw_final = gsub("inquilines","inquiline", kw_final)) %>%
  mutate(kw_final = gsub("intervals","interval", kw_final)) %>%
  mutate(kw_final = gsub("introductions","introduction", kw_final)) %>%
  mutate(kw_final = gsub("limpets","limpet", kw_final)) %>%
  mutate(kw_final = gsub("livelihoods","livelihood", kw_final)) %>%
  mutate(kw_final = gsub("manakins","manakin", kw_final)) %>%
  mutate(kw_final = gsub("markets","market", kw_final)) %>%
  mutate(kw_final = gsub("matrices","matrice", kw_final)) %>%
  mutate(kw_final = gsub("meadows","meadow", kw_final)) %>%
  mutate(kw_final = gsub("measles","measle", kw_final)) %>%
  mutate(kw_final = gsub("meerkats","meerkat", kw_final)) %>%
  mutate(kw_final = gsub("mistletoes","mistletoe", kw_final)) %>%
  mutate(kw_final = gsub("modifications","modification", kw_final)) %>%
  mutate(kw_final = gsub("niches","niche", kw_final)) %>%
  mutate(kw_final = gsub("otters","otter", kw_final)) %>%
  mutate(kw_final = gsub("patens","paten", kw_final)) %>%
  mutate(kw_final = gsub("perturbations","perturbation", kw_final)) %>%
  mutate(kw_final = gsub("phytochemicals","phytochemical", kw_final)) %>%
  mutate(kw_final = gsub("planthoppers","planthopper", kw_final)) %>%
  mutate(kw_final = gsub("predictors","predictor", kw_final)) %>%
  mutate(kw_final = gsub("producers","producer", kw_final)) %>%
  mutate(kw_final = gsub("propagules","propagule", kw_final)) %>%
  mutate(kw_final = gsub("proteomics","proteomic", kw_final)) %>%
  mutate(kw_final = gsub("seasons","season", kw_final)) %>%
  mutate(kw_final = gsub("sprouts","sprout", kw_final)) %>%
  mutate(kw_final = gsub("sugars","sugar", kw_final)) %>%
  mutate(kw_final = gsub("sunflecks","sunfleck", kw_final)) %>%
  mutate(kw_final = gsub("swamps","swamp", kw_final)) %>%
  mutate(kw_final = gsub("symbionts","symbiont", kw_final)) %>%
  mutate(kw_final = gsub("symbioses","symbiosis", kw_final)) %>%
  mutate(kw_final = gsub("trials","trial", kw_final)) %>%
  mutate(kw_final = gsub("weevils","weevil", kw_final)) %>%
  mutate(kw_final = gsub("woodlands","woodland", kw_final)) %>%
  mutate(kw_final = gsub("acorns","acorn", kw_final)) %>%
  mutate(kw_final = gsub("acoustics","acoustic", kw_final)) %>%
  mutate(kw_final = gsub("aggregates","aggregate", kw_final)) %>%
  mutate(kw_final = gsub("alleles","allele", kw_final)) %>%
  mutate(kw_final = gsub("autosomes","autosome", kw_final)) %>%
  mutate(kw_final = gsub("baboons","baboon", kw_final)) %>%
  mutate(kw_final = gsub("biomarkers","biomarker", kw_final)) %>%
  mutate(kw_final = gsub("blooms","bloom", kw_final)) %>%
  mutate(kw_final = gsub("bristles","bristle", kw_final)) %>%
  mutate(kw_final = gsub("browsers","browser", kw_final)) %>%
  mutate(kw_final = gsub("bryozoans","bryozoan", kw_final)) %>%
  mutate(kw_final = gsub("bulbuls","bulbul", kw_final)) %>%
  mutate(kw_final = gsub("burrows","burrow", kw_final)) %>%
  mutate(kw_final = gsub("clocks","clock", kw_final)) %>%
  mutate(kw_final = gsub("cockroaches","cockroach", kw_final)) %>%
  mutate(kw_final = gsub("cohorts","cohort", kw_final)) %>%
  mutate(kw_final = gsub("coordinates","coordinate", kw_final)) %>%
  mutate(kw_final = gsub("corrientes","corriente", kw_final)) %>%
  mutate(kw_final = gsub("cotyledons","cotyledon", kw_final)) %>%
  mutate(kw_final = gsub("covariates","covariate", kw_final)) %>%
  mutate(kw_final = gsub("coyotes","coyote", kw_final)) %>%
  mutate(kw_final = gsub("cryptogams","cryptogam", kw_final)) %>%
  mutate(kw_final = gsub("crystals","crystal", kw_final)) %>%
  mutate(kw_final = gsub("currents","current", kw_final)) %>%
  mutate(kw_final = gsub("cyclones","cyclone", kw_final)) %>%
  mutate(kw_final = gsub("dispersers","disperser", kw_final)) %>%
  mutate(kw_final = gsub("ecoregions","ecoregion", kw_final)) %>%
  mutate(kw_final = gsub("embryos","embryo", kw_final)) %>%
  mutate(kw_final = gsub("emissions","emission", kw_final)) %>%
  mutate(kw_final = gsub("epidemics","epidemic", kw_final)) %>%
  mutate(kw_final = gsub("ethics","ethic", kw_final)) %>%
  mutate(kw_final = gsub("faeces","feces", kw_final)) %>%
  mutate(kw_final = gsub("fusions","fusion", kw_final)) %>%
  mutate(kw_final = gsub("gametes","gamete", kw_final)) %>%
  mutate(kw_final = gsub("gardens","garden", kw_final)) %>%
  mutate(kw_final = gsub("glaciations","glaciation", kw_final)) %>%
  mutate(kw_final = gsub("glands","gland", kw_final)) %>%
  mutate(kw_final = gsub("heatwaves","heatwave", kw_final)) %>%
  mutate(kw_final = gsub("innovations","innovation", kw_final)) %>%
  mutate(kw_final = gsub("inselbergs","inselberg", kw_final)) %>%
  mutate(kw_final = gsub("interactors","interactor", kw_final)) %>%
  mutate(kw_final = gsub("kinematics","kinematic", kw_final)) %>%
  mutate(kw_final = gsub("ladybeetles","ladybeetle", kw_final)) %>%
  mutate(kw_final = gsub("landmarks","landmark", kw_final)) %>%
  mutate(kw_final = gsub("leafhoppers","leafhopper", kw_final)) %>%
  mutate(kw_final = gsub("malformations","malformation", kw_final)) %>%
  mutate(kw_final = gsub("matings","mating", kw_final)) %>%
  mutate(kw_final = gsub("microenvironments","microenvironment", kw_final)) %>%
  mutate(kw_final = gsub("midges","midge", kw_final)) %>%
  mutate(kw_final = gsub("migrans","migran", kw_final)) %>%
  mutate(kw_final = gsub("traps","trap", kw_final)) %>%
  mutate(kw_final = gsub("minisatellites","minisatellite", kw_final)) %>%
  mutate(kw_final = gsub("monocultures","monoculture", kw_final)) 




# temporary save/reload point ---------------------------------------------

write_csv(keywords, file = "./bibliometrics/data_intermediate/kw_int.csv")
# keywords<-read_csv("./bibliometrics/data_intermediate/kw_int.csv")


unique_kw_summary <- keywords %>%
  group_by(kw_final) %>%
  tally() %>%
  arrange(desc(n))
unique_kw_summary


# final check for plurals ------------------------------------------------------------
# look for last s

s_words<-unique_kw_summary %>%
  # separate(kw_final,c("first", "second","third"), " ", extra = "merge") %>% 
  separate(kw_final,c("first", "last_word"), " ", fill = "left") %>% 
  mutate(last_no_s=str_sub(last_word, 1,str_length(last_word)-1)) %>%  
  relocate(last_no_s,.after = "last_word") %>% 
  # mutate(last_word=case_when(
  #  (is.na(third)==TRUE & is.na(second)==TRUE)~first,
  #  (is.na(third)==TRUE & is.na(second)!=TRUE)~second,
  #  (is.na(second)!=TRUE & is.na(third)!=TRUE)~third,
  #  TRUE~as.factor("ugh")
  #  )) %>%
  filter(nchar(last_word)>5) %>% 
  # arrange(desc(nchar(last_word))) %>% 
  # slice(1:1) %>% 
  mutate(final_letter=str_sub(last_word, - 1, - 1)) %>% 
  mutate(final_2letter=str_sub(last_word, - 2, - 1)) %>%  
  mutate(final_3letter=str_sub(last_word, - 3, - 1)) %>%  
  filter(str_detect(last_word,"oides")!=TRUE)  %>% 
  filter(str_detect(last_word,"spides")!=TRUE)  %>% 
  filter(str_detect(last_word,"ormes")!=TRUE)  %>% 
  filter(str_detect(last_word,"yops")!=TRUE)  %>% 
  filter(str_detect(last_word,"ales")!=TRUE)  %>%
  filter(str_detect(last_word,"cites")!=TRUE)  %>%
  filter(str_detect(last_word,"aes")!=TRUE)  %>% 
  # filter(str_detect(last_word,"dae")!=TRUE)  %>% 
  filter(str_detect(last_word,"termes")!=TRUE)  %>% 
  filter(str_detect(last_word,"ichos")!=TRUE)  %>% 
  filter(str_detect(last_word,"otheres")!=TRUE)  %>% 
  filter(str_detect(last_word,"iops")!=TRUE)  %>% 
  filter(str_detect(last_word,"icans")!=TRUE)  %>% 
  filter(str_detect(last_word,"poda")!=TRUE)  %>% 
  filter(str_detect(last_word,"arians")!=TRUE)  %>% 
  filter(str_detect(last_word,"ulidas")!=TRUE)  %>% 
  filter(str_detect(last_word,"podes")!=TRUE)  %>%
  filter(str_detect(last_word,"cetes")!=TRUE)  %>% 
  filter(str_detect(last_word,"monas")!=TRUE)  %>% 
  filter(str_detect(last_word,"chos")!=TRUE)  %>% 
  filter(str_detect(last_word,"frons")!=TRUE)  %>% 
  filter(str_detect(last_word,"cytes")!=TRUE)  %>% 
  filter(str_detect(last_word,"ipes")!=TRUE)  %>% 
  filter(str_detect(last_word,"gens")!=TRUE)  %>% 
  filter(str_detect(last_word,"tans")!=TRUE)  %>% 
  filter(str_detect(last_word,"ideae")!=TRUE)  %>% 
  filter(final_letter=="s") %>% 
  filter(final_2letter!="ss") %>% 
  filter(final_2letter!="is") %>% 
  filter(final_2letter!="us") %>% 
  filter(final_2letter!="ys") %>% 
  filter(final_3letter!="ies") %>%
  distinct(last_word, .keep_all=TRUE)

s_words

write_csv(s_words, file = "./bibliometrics/data_intermediate/s_words.csv")

write_csv(keywords, file = "./bibliometrics/data_intermediate/kw_int_no_plurals.csv")
# keywords<-read_csv("./bibliometrics/data_intermediate/kw_int_no_plurals.csv")



# remove hyphens ----------------------------------------------------------


keywords<-keywords %>% mutate(kw_final = gsub("-", " ", kw_final))

unique_kw_summary <- keywords %>%
  group_by(kw_final) %>%
  tally() %>%
  arrange(desc(n))
unique_kw_summary



# keywords<-left_join(keywords, refined3, relationship = "many-to-many")
# summary(keywords$kw_final==keywords$kw_final_refined)


# kw similarity -----------------------------------------------------------

# 
# 
# unique_kw_summary <- keywords %>%
#   group_by(kw_final) %>%
#   tally() %>%
#   arrange(desc(n))
# unique_kw_summary
# 
# 
# 


# check similarity of s_words, can use resulting output to depluralize
library(RecordLinkage)
Name.check <- function(DataToClean) {
  CHECKFILE <- DataToClean
  NamesList <- sapply(CHECKFILE$kw_final, agrep, CHECKFILE$kw_final, value = TRUE)
  NamesDF <- data.frame(
    Name1 = rep(names(NamesList), lapply(NamesList, length)),
    Name2 = unlist(NamesList)
  )

  # summary(NamesDF)
  # str(NamesDF)

  # Create a column to which you will add a logical condition telling you if the names are an EXACT match
  NamesDF$match <- NA
  NamesDF$match <- NamesDF$Name1 == NamesDF$Name2
  # match2<-ifelse(NamesDF$match=="TRUE",1,0) #convert TRUE/FALSEto 0/1
  # NamesDF<-cbind(NamesDF,match2)
  # head(NamesDF,40)
  # str(NamesDF)
  NamesDF <- arrange(NamesDF, Name1, Name2) # organize in alphabetica order
  NamesDF <- filter(NamesDF, match == FALSE) # THIS DELETES ALL NAMES THAT ARE 100% MATCH
  head(NamesDF)
  # Convert to chr
  NamesDF$Name1 <- as.character(NamesDF$Name1)
  NamesDF$Name2 <- as.character(NamesDF$Name2)
  str(NamesDF)

  # Calclulate the proportional similarity and # changes required to go from one name to another. Package RecordLinkage
  NamesDF$Name_sim <- levenshteinSim(NamesDF$Name1, NamesDF$Name2)
  NamesDF$Name_dist <- levenshteinDist(NamesDF$Name1, NamesDF$Name2)

  # Because this does all pairwise comparisons, it results in redundancy: "e bruna vs emilio bruna" and "emilio bruna vs e bruna"
  # are in different rows, even though they are the same "comparison". This deletes one of the two
  NamesDF <- NamesDF[!duplicated(t(apply(NamesDF, 1, sort))), ]
  # this arranges them in order from most similar (1 change required) to least similar.
  # look carefully at those with a few changes, as they are likely to be a tiny spelling mistake or difference in intials


  NamesDF$index <- seq.int(nrow(NamesDF)) # adds a column with an index to make it easier to id which row you need'
  NamesDF <- NamesDF %>% select(index, Name1, Name2, Name_sim, Name_dist) # It's kinda ugly, but this rearranges columns (and dumps the "FALSE")
  NamesDF <- arrange(NamesDF, desc(Name_sim))
  NamesDF <- NamesDF %>% filter(Name_sim>0.70)
  # head(NamesDF)
  write_csv(NamesDF, file = "./bibliometrics/data_intermediate/kw_similarity.csv") # export it as a csv file


  return(NamesDF)
}

# unique_kw_summary <- unique_kw_summary %>% mutate_all(trimws)
# foo <- unique_kw_summary %>% slice(500:1500)

DataToClean<-s_words %>% rename(kw_final=last_word)
DataToClean<-foo
kw_similarity <- Name.check(DataToClean)



# end kw similarity -------------------------------------------------------

# unique_kw <- unique(keywords$kw_final_refined) %>% as_tibble()

library(countrycode)
keywords$code <- countrycode(keywords$kw_final_refined, origin = "country.name", destination = "iso3c")
keywords <- keywords %>%
  mutate(code = case_when(
    (code == "CIV" & kw_final != "ivory coast") ~ as.character(NA),
    (code == "AIA" & kw_final != "anguilla") ~ as.character(NA),
    (code == "ARG" & kw_final != "argentina") ~ as.character(NA),
    (code == "COD" & (kw_final != "dr congo" | kw_final != "katanga dem. rep. congo")) ~ as.character(NA),
    (code == "AIA" & kw_final != "anguilla") ~ as.character(NA),
    (code == "BRA" & str_detect(kw_final, "brazilian") == TRUE) ~ as.character(NA),
    (code == "AUS" & str_detect(kw_final, "autstralian") == TRUE) ~ as.character(NA),
    kw_final == "asclepias syriaca" ~ as.character(NA),
    kw_final == "antiguastrea antingua" ~ as.character(NA),
    TRUE ~ as.character(code)
  ))



unique_kw_summary <- keywords %>%
  group_by(kw_final) %>%
  tally() %>%
  arrange(desc(n))

#
# anguilla bengalensis
# anguilla-anguilla
# anguilla-rostrata
# argentina anserina
# antiguastrea
# st. lawrence river québec canada
# williams lake british columbia canada
# cocos nucifera
# swiss long-term forest research programme lwf
# swiss stone pine
# austrocedrus chilensis
# aristotelia chilensis
# chilesius camposi
# (usa)-> , usa
# la selva, costa rica
# carpobrotus chilensis
# bathygobius cocosensis
# austrian
# dracocephalum austriacum
# dahomey gap
# bahamas mosquitofish
# bolivian
# aedes aegypti
# picea mariana
# monteverde
# bef china
# carpodacus mexicanus
# repalace united states with usa
# echinacea angustifolia
# seychelles warbler
# anopheles gambiae
# lutjanus peru
# heterandria formosa
# pteropus tonganus
# sierra madre oriental
# brazil nut
# canada goose
# gulf of mexico
# ecuadorian
# brazil nut
# brazil-nuts
# leishmania braziliensis
# yukon canada->yukon,canada
# boreal forest (yukon, canada)
# canada jays
# canada lynx
# canada warbler
# grassland.national park (canada)
# killarney provincial park ontario canada


keywords_clean_summary <- keywords %>%
  as_tibble() %>%
  filter(code != FALSE) %>%
  group_by(code, kw_final) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  mutate(nchar = nchar(kw_final)) %>%
  arrange(desc(nchar))
keywords_clean_summary





keywords40 <- keywords_clean_summary %>% filter(nchar > 40)
keywords5 <- keywords_clean_summary %>% filter(nchar < 5)
# keywords$kw_final2<-str_replace(keywords$kw_final2,"[:punct:]", " ")
#
#
#
# keywords_clean_summary<-keywords_clean_summary %>%
#   separate(kw_final,c("kw_final2","kw_final3"), sep = (" [(]"),extra="merge",remove=FALSE) %>%
#
#
# keywords_clean_summary<-keywords_clean_summary %>%
#   separate(kw_final,c("kw_final2","kw_final3"), sep = (" [(]"),extra="merge",remove=FALSE) %>%
#   separate(kw_final2,c("kw_final3","kw_final4"), sep = ("[)]"),extra="merge",remove=FALSE)
#


# language classification -------------------------------------------------


library(textcat)
library(cld2)
# library(cld3)
# https://www.r-bloggers.com/2021/05/language-identification-using-the-fasttext-package-a-benchmark/
# keywords100 <- keywords %>% slice(1:100)
# keywords100 <- keywords100 %>%
#   mutate(
#     textcat = textcat(x = kw_final),
#     # cld3 = cld3::detect_language(text = text),
#     cld2 = cld2::detect_language(text = kw_final, plain_text = TRUE)
#   )

# cld2_mixed=cld2::detect_language_mixed(text = kw_final, plain_text = FALSE))


# extract a spelling dictionary
tokens <- unnest_tokens(tbl = (keywords %>% slice(1:1000)), output = token, input = kw_final)
wordlist <- unique(tokens$token)

# Spell check the words
spelling.errors <- hunspell(wordlist)
spelling.errors <- unique(unlist(spelling.errors))
spelling.sugg <- hunspell_suggest(spelling.errors, dict = dictionary("en_US"))

# Pick the first suggestion
spelling.sugg <- unlist(lapply(spelling.sugg, function(x) x[1]))
spelling.dict <- as.data.frame(cbind(spelling.errors, spelling.sugg))
spelling.dict$spelling.pattern <- paste0("\\b", spelling.dict$spelling.errors, "\\b")

# Write out spelling dictionary
write.csv(
  x = spelling.dict, file = "./bibliometrics/code_analysis/intermediate_data/spelling.dict.csv",
  fileEncoding = "utf8", row.names = F
)

# Parse features
tokens <- unnest_tokens(
  tbl = X, output = token,
  input = feature_response, token = stringr::str_split,
  pattern = "  |\\, |\\.|\\,|\\;"
)
tokens$token <- trimws(tokens$token,
  which = c("both", "left", "right"),
  whitespace = "[ \t\r\n]"
)

# Remove empty features
tokens <- tokens[!tokens$token == "", ]

tokens$corrected <- stri_replace_all_regex(
  str = tokens$token,
  pattern = spelling.dict$spelling.pattern,
  replacement = spelling.dict$spelling.sugg,
  vectorize_all = FALSE
)

# Rename columns
tokens <- tokens %>%
  rename(cue = cue, feature = corrected) %>%
  select(cue, feature)

# Write processed file
write.csv(
  x = tokens, file = "./bibliometrics/code_analysis/intermediate_data/spellchecked.features.csv",
  fileEncoding = "utf8", row.names = F
)


# Lemmatization and Multi-Word Sequences ----------------------------------

# Open the spell checked data
X <- read.csv("./bibliometrics/code_analysis/intermediate_data/spellchecked.features.csv", stringsAsFactors = F)

# Extract the list of updated tokens
tokens <- unnest_tokens(tbl = X, output = word, input = corrected)
cuelist <- unique(tokens$refID)

# Create a dataframe for lemmas
tokens.tagged <- data.frame(
  doc_id = character(),
  token = character(),
  wclass = character(),
  lemma = character(),
  stringsAsFactors = FALSE
)

# Loop over cues and create lemmas + POS tags
for (i in 1:length(cuelist)) {
  temp.tag <- suppressWarnings(
    suppressMessages(
      treetag(c(X$feature[X$cue == cuelist[i]], "NULL"),
        treetagger = "manual", format = "obj",
        TT.tknz = FALSE, lang = "en", doc_id = cuelist[i],
        # These parameters are based on your computer
        TT.options = list(path = "~/downloads/TreeTagger", preset = "en")
      )
    )
  )

  temp.tag <- temp.tag@TT.res %>%
    mutate_if(is.factor, as.character)

  tokens.tagged <- tokens.tagged %>%
    bind_rows(temp.tag %>%
      select(doc_id, token, wclass, lemma))
}

tokens.tagged <- tokens.tagged %>%
  rename(cue = doc_id, feature = token, pos = wclass)

# Clean up unknown lookups
tokens.tagged$lemma[tokens.tagged$lemma == "<unknown>"] <- tokens.tagged$feature[tokens.tagged$lemma == "<unknown>"]
tokens.tagged$lemma[tokens.tagged$lemma == "@card@"] <- tokens.tagged$feature[tokens.tagged$lemma == "@card@"]
tokens.tagged$lemma <- tolower(tokens.tagged$lemma)

# Write processed file
write.csv(
  x = tokens.tagged, file = "../output_data/lemmatized.features.csv",
  fileEncoding = "utf8", row.names = F
)











# spell




# keywords_clean<-keywords_clean %>%
#   mutate(kw_final = case_when(
#   (str_detect(kw_final,", (usa)") == TRUE) ~ ", usa",
#   TRUE ~ as.character(kw_final)))
#   mutate(kw_final=gsub("//(uk)",", uk",kw_final))
#
# keywords_clean<-keywords_clean %>%
#   separate(kw_final,c("kw_final2","kw_final3"),sep=",",remove=FALSE,extra="warn") %>%
#   separate(kw_final3,c("kw_final4","kw_final5"),sep="\\(",remove=FALSE,extra="warn")
keywords_clean_summary <- keywords %>%
  as_tibble() %>%
  group_by(kw_final) %>%
  summarize(n = n())
keywords_clean_summary

# -------------------- check for spelling mistakes ---------------

library(hunspell)

## find the misspelled words
# foo<-kw_final_counts_split %>%  select(kw_original) %>% slice(1:3000)
bad.words <- hunspell(keywords_clean_summary$kw_final)
bad.words <- unique(unlist(bad.words))
sugg.words <- hunspell_suggest(bad.words)
sugg.words <- unlist(lapply(sugg.words, function(x) x[1]))
word.list <- as.data.frame(cbind(bad.words, sugg.words))
#
# freq.word <- count(foo, kw_original)
# names(freq.word)
# names(word.list)
# freq.word <- inner_join(freq.word, word.list, by = c(kw_original = "bad.words")) %>% mutate(sugg.words=tolower(sugg.words))
# freq.word <- freq.word %>% distinct(sugg.words,kw_original) %>% mutate(unique=(sugg.words==kw_original)) %>% filter(unique==FALSE)
# freq.word<- freq.word %>%
#   mutate(sugg.words=gsub(" ","",sugg.words))  %>%
#   mutate(unique=(sugg.words==kw_original)) %>%
#   filter(unique==FALSE) %>%
#   mutate(sugg.words=gsub(" ","",sugg.words))  %>%
#   mutate(unique=(sugg.words==kw_original)) %>%
#   filter(unique==FALSE)


# keywords_clean$kw_final<-str_replace(keywords_clean$kw_final,"species\\) diversity/biodiversity", "species diversity")
# keywords_clean$kw_final<-str_replace(keywords_clean$kw_final,"[:punct:]", "")
# keywords_clean$kw_final<-str_replace(keywords_clean$kw_final,"\\)", "")
# keywords_clean$kw_final<-str_replace(keywords_clean$kw_final,"\\(", "")
# keywords_clean$kw_final<-str_replace(keywords_clean$kw_final,"\\,", "")


keywords_clean_summary <- keywords_clean %>%
  as_tibble() %>%
  group_by(kw_final) %>%
  summarize(n = n()) %>%
  arrange(desc(n))
keywords_clean_summary

keywords <- keywords %>%
  separate(kw_final, c("kw_final2", "kw_final3"), sep = (" [(]"), extra = "merge", remove = FALSE)
keywords <- keywords %>%
  separate(kw_final3, c("kw_final3", "kw_final4"), sep = ("[)]"), extra = "merge", remove = FALSE)

# sort to get rid of abbreviations
keywords$kw_final2 <- str_replace(keywords$kw_final2, "[:punct:]", " ")
keywords$kw_final2 <- str_replace(keywords$kw_final2, "[-]", " ")




keywords <- keywords %>%
  mutate(code = case_when(
    (code == "CIV" & kw_final != "ivory coast") ~ as.character(NA),
    (code == "AIA" & kw_final != "anguilla") ~ as.character(NA),
    (code == "ARG" & kw_final != "argentina") ~ as.character(NA),
    (code == "COD" & (kw_final != "dr congo" | kw_final != "katanga dem. rep. congo")) ~ as.character(NA),
    (code == "AIA" & kw_final != "anguilla") ~ as.character(NA),
    (code == "BRA" & str_detect(kw_final, "brazilian") == TRUE) ~ as.character(NA),
    (code == "AUS" & str_detect(kw_final, "autstralian") == TRUE) ~ as.character(NA),
    kw_final == "asclepias syriaca" ~ as.character(NA),
    kw_final == "antiguastrea antingua" ~ as.character(NA),
    TRUE ~ as.character(code)
  ))

library(countrycode)
keywords_clean_summary
# to avoid countries being split/be consistent re names
# you can convert all to iso code

# kw100<-keywords_clean_summary
kw100 <- keywords_clean_summary %>% slice(1:5000)
library(countrycode)
kw100$code <- countrycode(kw100$kw_final, origin = "country.name", destination = "iso3c")
kw_country <- kw100 %>%
  filter(code != FALSE) %>%
  group_by(kw_final, code) %>%
  mutate(code = case_when(
    (code == "CIV" & kw_final != "ivory coast") ~ as.character(NA),
    (code == "AIA" & kw_final != "anguilla") ~ as.character(NA),
    (code == "ARG" & kw_final != "argentina") ~ as.character(NA),
    (code == "COD" & (kw_final != "dr congo" | kw_final != "katanga dem. rep. congo")) ~ as.character(NA),
    (code == "AIA" & kw_final != "anguilla") ~ as.character(NA),
    (code == "BRA" & str_detect(kw_final, "brazilian") == TRUE) ~ as.character(NA),
    (code == "AUS" & str_detect(kw_final, "autstralian") == TRUE) ~ as.character(NA),
    kw_final == "asclepias syriaca" ~ as.character(NA),
    kw_final == "antiguastrea antingua" ~ as.character(NA),
    TRUE ~ as.character(code)
  ))
# delete "western "
# delete "eastern "
# delete "northern "
# delete "southern "
kw100$code <- countrycode(kw100$kw_final, origin = "country.name", destination = "iso3c")

keywords_clean$code <- countrycode(keywords_clean$kw_final, origin = "country.name", destination = "iso3c")
keywords_clean <- keywords_clean %>%
  mutate(code = case_when(
    (code == "CIV" & kw_final != "ivory coast") ~ as.character(NA),
    (code == "AIA" & kw_final != "anguilla") ~ as.character(NA),
    (code == "ARG" & kw_final != "argentina") ~ as.character(NA),
    (code == "COD" & (kw_final != "dr congo" | kw_final != "katanga dem. rep. congo")) ~ as.character(NA),
    (code == "AIA" & kw_final != "anguilla") ~ as.character(NA),
    (code == "BRA" & str_detect(kw_final, "brazilian") == TRUE) ~ as.character(NA),
    (code == "AUS" & str_detect(kw_final, "australian") == TRUE) ~ as.character(NA),
    kw_final == "asclepias syriaca" ~ as.character(NA),
    kw_final == "antiguastrea antingua" ~ as.character(NA),
    TRUE ~ as.character(code)
  ))


# anguilla bengalensis
# anguilla-anguilla
# anguilla-rostrata
# argentina anserina
# antiguastrea
# st. lawrence river québec canada
# williams lake british columbia canada
# cocos nucifera
# swiss long-term forest research programme lwf
# swiss stone pine
# austrocedrus chilensis
# aristotelia chilensis
# chilesius camposi
# (usa)-> , usa
# la selva, costa rica
# carpobrotus chilensis
# bathygobius cocosensis
# austrian
# dracocephalum austriacum
# dahomey gap
# bahamas mosquitofish
# bolivian
# ecuadorian
# brazil nut
# brazil-nuts
# leishmania braziliensis
# yukon canada->yukon,canada
# boreal forest (yukon, canada)
# canada jays
# canada lynx
# canada warbler
# grassland.national park (canada)
# killarney provincial park ontario canada


keywords_clean_summary <- keywords %>%
  as_tibble() %>%
  group_by(kw_final2) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  filter(is.na(code) == FALSE)
keywords_clean_summary





# lemmatization -----------------------------------------------------------
# install.packages('udpipe')
# https://bnosac.github.io/udpipe/en/
# library(udpipe)
# x <- c(doc_a = "In our last meeting, someone said that we are meeting again tomorrow",
#        doc_b = "It's better to be good at being the best")
# anno <- udpipe(x, "english")
# anno[, c("doc_id", "sentence_id", "token", "lemma", "upos")]


# prior to lemmatizing, there are some that might be worth excluding
# taxonomic:
# ii
# orum
# ae
# arum
# aceae
# oideae
# oides
# rales
# ormes


keywords_clean_summary <- keywords %>%
  as_tibble() %>%
  group_by(kw_final2) %>%
  summarize(n = n()) %>%
  arrange(desc(n))
keywords_clean_summary


library(udpipe)
x <- keywords_clean_summary %>%
  select(kw_final2) %>%
  mutate(id = row_number()) %>%
  slice(1:3000) %>%
  pull(kw_final2)


# x<-keywords_clean_summary %>% select(kw_final)
# x<-unlist(x)
anno <- udpipe(x, "english")
anno$unique <- (anno$token == anno$lemma)
summary(anno$unique)
lemmas <- anno[, c("doc_id", "sentence_id", "token", "lemma", "upos", "unique")]
lemmas <- lemmas %>% arrange(unique)
lemmas

false_checks <- lemmas %>%
  filter(unique == FALSE) %>%
  group_by(token, lemma) %>%
  tally() %>%
  mutate(diff = (nchar(token) - nchar(lemma))) %>%
  arrange(desc(diff))

lemmas_summary <- lemmas %>%
  group_by(lemma, token) %>%
  tally() %>%
  arrange(desc(n)) %>%
  filter(n > 100)


lemmas_summary <- lemmas %>%
  group_by(lemma, token) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(lemma) %>%
  tally() %>%
  arrange(desc(n)) %>%
  filter(n > 1)

# kw comb freq stats: https://bnosac.github.io/udpipe/docs/doc5.html
library(udpipe)
x <- keywords_clean_summary %>%
  select(kw_final) %>%
  mutate(id = row_number())
# %>%
#   slice(1:10000)
# data(brussels_reviews)
# comments <- subset(brussels_reviews, language %in% "es")
# ud_model <- udpipe_download_model(language = "spanish")
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)
x <- udpipe_annotate(ud_model, x = x$kw_final, doc_id = x$id)
x <- as.data.frame(x)
stats <- keywords_rake(
  x = x, term = "lemma", group = "doc_id",
  relevant = x$upos %in% c("NOUN", "ADJ")
)
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
library(lattice)
barchart(key ~ rake,
  data = head(subset(stats, freq > 3), 20), col = "cadetblue",
  main = "Keywords identified by RAKE",
  xlab = "Rake"
)


# most ocurring noun
stats <- subset(x, upos %in% c("NOUN"))
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq,
  data = head(stats, 20), col = "cadetblue",
  main = "Most occurring nouns", xlab = "Freq"
)

## most ocurring ADJECTIVES
stats <- subset(x, upos %in% c("ADJ"))
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq,
  data = head(stats, 20), col = "cadetblue",
  main = "Most occurring adjectives", xlab = "Freq"
)

# Nouns / adjectives used in same sentence
cooc <- cooccurrence(
  x = subset(x, upos %in% c("NOUN", "ADJ")),
  term = "lemma",
  # group = c("doc_id", "paragraph_id", "sentence_id"))
  group = c("doc_id")
)
head(cooc, 20)


# Nouns / adjectives which follow one another
cooc <- cooccurrence(x$lemma, relevant = x$upos %in% c("NOUN", "ADJ"), skipgram = 1)
head(cooc)



# TODO: correct
# seed->see
# seedling->seed
# seedseedling->seedseedl
# tree seedling->treeseedling
# wren->w
#



# Tutorial for knowledge classification from raw text
# Huang Tian-Yuan (Hope)
# This tutorial gives an example of how to use akc package to carry out automatic knowledge classification based on raw text.
# https://cran.rstudio.com/web/packages/akc/vignettes/tutorial_raw_text.html

# outdated lemmatization practice -----------------------------------------


# # sample<-c("I like apple, pear, pears, and bannana.")
# #
# #   (keywords_clean$kw_final)
# ##############
# # https://stackoverflow.com/questions/28214148/how-to-perform-lemmatization-in-r
# library(koRpus)
# library(koRpus.lang.en)
# # ~/Dropbox (UFL)/Talks/atbc2022_plenary_talk/bibliometrics/code_analysis/tree_tagger
# # sh /bibliometrics/code_analysis/tree_tagger/install-tagger.sh
# # cd /atbc2022_plenary_talk/bibliometrics/code_analysis/tree_tagger/install-tagger.sh
# # cd /bibliometrics/code_analysis/tree_tagger/install-tagger.sh
# # ./code_analysis/tree_tagger/install-tagger.sh
# tagged.results <- treetag(c("run", "ran", "running"), treetagger="manual", format="obj",
#                           TT.tknz=FALSE , lang="en",
#                           TT.options=list(path="./TreeTagger", preset="en"))
# tagged.results@TT.res
#
# system("./atbc2022_plenary_talk/bibliometrics/code_analysis/tree_tagger/install-tagger.sh")
# tagged.text <- tokenize("./bibliometrics/code_analysis/sample_text.txt", lang="en")
# str(describe(tagged.text))
# describe(tagged.text)[["lttr.distrib"]]
# freq.analysis.res <- freq.analysis(tagged.text, corp.freq=LCC.en)
# taggedText(freq.analysis.res)
#
#
# OR
#
# # https://cran.r-project.org/web/packages/textstem/readme/README.html
#
# library(tm)
# library(lexicon)
# library(wordcloud)
# library(textstem)
#
# sample<-c("I like apple, pear, pears, and bannana.")
#
# (keywords_clean$kw_final)
# kw_final <- keywords_clean[['kw_final']]
# sample<-keywords_clean_summary %>% select(kw_final) %>% slice(1:100)
# sample<-sample[['kw_final']]
# sample<-(sample)
# a<-sapply(c("operates", "operating", "operation", "operational", "operator", "operators", "operative", "operatives"), lemmatize_words)
# sapply(sample, lemmatize_words)
#
# crCorpus_lem <- tm_map(sample, lemmatize_strings)
# inspect(crCorpus_lem[[1]])
# https://rpubs.com/chelseyhill/669117
##############
























# (generalized) linear model
keywords_clean <- keywords_clean %>% as_tibble()
keywords_clean$kw_final <- str_trim(keywords_clean$kw_final)
kw_final_counts <- keywords_clean %>%
  mutate_all(str_trim) %>%
  select(kw_final) %>%
  mutate(kw_final = trimws(kw_final)) %>%
  group_by(kw_final) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  mutate(kw_final = gsub("-", " ", kw_final)) %>%
  mutate(to = strsplit(kw_final, " ")) %>%
  unnest(to) %>%
  group_by(kw_final) %>%
  mutate(row = row_number()) %>%
  spread(row, to) %>%
  ungroup() %>%
  select(-n) %>%
  pivot_longer(!kw_final, names_to = "letter", values_to = "kw_original") %>%
  select(-letter) %>%
  drop_na(kw_original) %>%
  mutate(kw_original = gsub("\n", " ", kw_original)) %>%
  # mutate(kw_original=str_replace('\\n"', '')) %>%
  mutate(kw_original = tolower(kw_original)) %>%
  mutate(kw_original = trimws(kw_original))

kw_final_counts$kw_original <- str_replace(kw_final_counts$kw_original, "\\,", "")
# library(textclean)
# kw_final_counts<-kw_final_counts %>%
#   replace_non_ascii(kw_original, replacement = "", remove.nonconverted = TRUE)

kw_final_counts_40 <- kw_final_counts %>%
  group_by(kw_original) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  filter(n > 40)

# kw_final_counts$last_letter<-str_sub(kw_final_counts$kw_original,-1,-1)
# kw_final_counts_pl <- kw_final_counts %>% filter(last_letter=="s")
# kw_final_counts<-kw_final_counts %>% arrange(kw_original,desc(n))


library(SnowballC)
prof.tm <- mutate(kw_final_counts, word.stem = wordStem(kw_original, language = "en"))
prof.tm <- prof.tm %>%
  mutate(stem_chk = kw_original == word.stem) %>%
  filter(stem_chk == FALSE)



library(RecordLinkage)
prof.tm$word_sim <- levenshteinSim(prof.tm$kw_original, prof.tm$word.stem)
prof.tm <- prof.tm %>%
  arrange(desc(word_sim)) %>%
  select(-stem_chk)
prof.tm
# https://books.psychstat.org/textmining/data.html
library(hunspell)

## find the misspelled words
# foo<-kw_final_counts_split %>%  select(kw_original) %>% slice(1:3000)
bad.words <- hunspell(kw_final_counts$kw_original)
bad.words <- unique(unlist(bad.words))
sugg.words <- hunspell_suggest(bad.words)
sugg.words <- unlist(lapply(sugg.words, function(x) x[1]))
word.list <- as.data.frame(cbind(bad.words, sugg.words))

freq.word <- count(foo, kw_original)
names(freq.word)
names(word.list)
freq.word <- inner_join(freq.word, word.list, by = c(kw_original = "bad.words")) %>% mutate(sugg.words = tolower(sugg.words))
freq.word <- freq.word %>%
  distinct(sugg.words, kw_original) %>%
  mutate(unique = (sugg.words == kw_original)) %>%
  filter(unique == FALSE)
freq.word <- freq.word %>%
  mutate(sugg.words = gsub(" ", "", sugg.words)) %>%
  mutate(unique = (sugg.words == kw_original)) %>%
  filter(unique == FALSE) %>%
  mutate(sugg.words = gsub(" ", "", sugg.words)) %>%
  mutate(unique = (sugg.words == kw_original)) %>%
  filter(unique == FALSE)

library(RecordLinkage)
freq.word$word_sim <- levenshteinSim(freq.word$kw_original, freq.word$sugg.words)
freq.word <- freq.word %>% arrange(desc(word_sim))
write_csv(freq.word, "./bibliometrics/data_intermediate/freq.word.csv")

# library("SemNetCleaner")
foo <- kw_final_counts_split %>%
  select(kw_original) %>%
  slice(1:3000)
clean <- textcleaner(
  data = foo, miss = NA,
  partBY = "row", dictionary = "general"
)


kw_final_counts_split <- kw_final_counts %>%
  group_by(kw_original) %>%
  summarize(n = n()) %>%
  arrange(desc(n))


clean <- textcleaner(
  data = open.animals[, -c(1:2)], miss = 99,
  partBY = "row", dictionary = "animals"
)

kw_final_counts %>%
  slice(1:300) %>%
  mutate(kw_final_split = stringr::str_extract_all(
    kw_final_counts$kw_final,
    "(trait)|(forest)|(mangrove)"
  )) %>%
  tidyr::unnest(cols = c(focal_words))

fruits <- c("trait", "forest", "mangrove")

kw_final_counts$kw_final2 <- sapply(stringr::str_extract_all(
  kw_final_counts$kw_final,
  sprintf("(%s)", paste0(fruits, collapse = "|"))
), toString)

rows1 <- floor(nrow(kw_final_counts) / 3)
rows2 <- rows1 * 2
rows3 <- rows1 * 3
cola <- kw_final_counts %>% slice(1:rows1)
colb <- kw_final_counts %>% slice((rows1 + 1):(rows2))
colc <- kw_final_counts %>% slice((rows2 + 1):(rows3))
kw_final_counts <- bind_cols(cola, colb, colc)
write_csv(kw_final_counts, "./bibliometrics/data_intermediate/kw_final_counts.csv")
# rm(keywords_unique)



keywords_unique <- keywords %>%
  select(kw_final) %>%
  unique() %>%
  arrange(kw_final)


write_csv(keywords_unique, "./bibliometrics/data_intermediate/keywords_unique.csv")
# rm(keywords_unique)



kw_similarity <- Name.check(keywords_unique)
Name.check <- function(DataToClean) {
  CHECKFILE <- DataToClean
  NamesList <- sapply(CHECKFILE$kw_final, agrep, CHECKFILE$kw_final, value = TRUE)
  NamesDF <- data.frame(
    Name1 = rep(names(NamesList), lapply(NamesList, length)),
    Name2 = unlist(NamesList)
  )

  # summary(NamesDF)
  # str(NamesDF)

  # Create a column to which you will add a logical condition telling you if the names are an EXACT match
  NamesDF$match <- NA
  NamesDF$match <- NamesDF$Name1 == NamesDF$Name2
  # match2<-ifelse(NamesDF$match=="TRUE",1,0) #convert TRUE/FALSEto 0/1
  # NamesDF<-cbind(NamesDF,match2)
  # head(NamesDF,40)
  # str(NamesDF)
  NamesDF <- arrange(NamesDF, Name1, Name2) # organize in alphabetica order
  NamesDF <- filter(NamesDF, match == FALSE) # THIS DELETES ALL NAMES THAT ARE 100% MATCH
  head(NamesDF)
  # Convert to chr
  NamesDF$Name1 <- as.character(NamesDF$Name1)
  NamesDF$Name2 <- as.character(NamesDF$Name2)
  str(NamesDF)

  # Calclulate the proportional similarity and # changes required to go from one name to another. Package RecordLinkage
  NamesDF$Name_sim <- levenshteinSim(NamesDF$Name1, NamesDF$Name2)
  NamesDF$Name_dist <- levenshteinDist(NamesDF$Name1, NamesDF$Name2)

  # Because this does all pairwise comparisons, it results in redundancy: "e bruna vs emilio bruna" and "emilio bruna vs e bruna"
  # are in different rows, even though they are the same "comparison". This deletes one of the two
  NamesDF <- NamesDF[!duplicated(t(apply(NamesDF, 1, sort))), ]
  # this arranges them in order from most similar (1 change required) to least similar.
  # look carefully at those with a few changes, as they are likely to be a tiny spelling mistake or difference in intials


  NamesDF$index <- seq.int(nrow(NamesDF)) # adds a column with an index to make it easier to id which row you need'
  NamesDF <- NamesDF %>% select(index, Name1, Name2, Name_sim, Name_dist) # It's kinda ugly, but this rearranges columns (and dumps the "FALSE")
  NamesDF <- arrange(NamesDF, desc(Name_sim))
  # head(NamesDF)
  write_csv(NamesDF, file = "./bibliometrics/data_intermediate/kw_similarity.csv") # export it as a csv file


  return(NamesDF)
}


keywords <- keywords %>%
  # mutate(kw_final=gsub("agroforest","agroforest",kw_final)) %>%
  mutate(article_cat = case_when(
    str_detect(kw_final, "tropics") == TRUE ~ "TRUE",
    str_detect(kw_final, "tropical") == TRUE ~ "TRUE",
    str_detect(kw_final, "bci") == TRUE ~ "TRUE",
    str_detect(kw_final, "pasoh") == TRUE ~ "TRUE",
    str_detect(kw_final, "la selva") == TRUE ~ "TRUE",
    str_detect(kw_final, "ots-oet") == TRUE ~ "TRUE",
    str_detect(kw_final, "bdffp") == TRUE ~ "TRUE",
    str_detect(kw_final, "manu national park") == TRUE ~ "TRUE",
    str_detect(kw_final, "cocha cashu") == TRUE ~ "TRUE",
    str_detect(kw_final, "amazon") == TRUE ~ "TRUE",
    str_detect(kw_final, "bci") == TRUE ~ "TRUE",
    # str_detect(kw_final,"tropic")==TRUE~"TRUE",
    str_detect(kw_final, "afrotrop") == TRUE ~ "TRUE",
    str_detect(kw_final, "rain forest") == TRUE ~ "TRUE",
    str_detect(kw_final, "dry forest") == TRUE ~ "TRUE",
    str_detect(kw_final, "la selva") == TRUE ~ "TRUE",
    str_detect(kw_final, "yasuni") == TRUE ~ "TRUE",
    TRUE ~ as.character("FALSE")
  )) %>%
  arrange(desc(article_cat))

names(keywords)
forest_kw <- keywords_unique %>%
  filter(str_detect(kw_final, "forest") == TRUE)


# pooling keywords  -------------------------------------------------------


names(keywords)
forest_kw <- keywords_unique %>%
  filter(str_detect(kw_final, "forest") == TRUE)

rain_forest_kw <- forest_kw %>%
  filter(
    (
      str_detect(kw_final, "wet") |
        str_detect(kw_final, "rain") |
        str_detect(kw_final, "african forest") |
        str_detect(kw_final, "rain")
    )
  ) %>%
  filter(str_detect(kw_final, "forest drainage") == FALSE) %>%
  filter(str_detect(kw_final, "forested wetlands") == FALSE) %>%
  filter(str_detect(kw_final, "temperate rainforest") == FALSE) %>%
  filter(str_detect(kw_final, "eucalyptus forest") == FALSE) %>%
  filter(str_detect(kw_final, "subtropical") == FALSE) %>%
  filter(str_detect(kw_final, "temperate") == FALSE) %>%
  filter(str_detect(kw_final, "hemiepiphytes lianas pioneers tree growth tropical wet forest") == FALSE) %>%
  filter(str_detect(kw_final, "litter litter nutrient content primary forest tropical rainforest") == FALSE)



temp_rainforest_kw <- forest_kw %>%
  filter(
    (
      str_detect(kw_final, "temperate rainforest") |
        str_detect(kw_final, "temperate rainforest")
      # str_detect(kw_final,"temperate")
    )
  )


# includes temp rainforest
temp_forest_kw <- forest_kw %>%
  filter(
    (
      str_detect(kw_final, "temperate")
    )
  )


subtrop_forest_kw <- forest_kw %>%
  filter(
    (
      str_detect(kw_final, "subtropical")
    )
  )

dry_forest_kw <- forest_kw %>%
  filter(str_detect(kw_final, "dry") == TRUE)

boreal_forest_kw <- forest_kw %>%
  filter(str_detect(kw_final, "boreal") == TRUE)


savanna_kw <- keywords_unique %>%
  filter(str_detect(kw_final, "savanna") == TRUE) %>%
  filter(str_detect(kw_final, "autccology bolivia cerrado gramineae kranz anatomy neotropics savanna") == FALSE) %>%
  filter(str_detect(kw_final, "brazil fire fleshy fruit frugivory fruit shortage reproduction savanna vertebrates") == FALSE) %>%
  filter(str_detect(kw_final, "autccology bolivia cerrado gramineae kranz anatomy neotropics savanna") == FALSE) %>%
  filter(str_detect(kw_final, "facilitation, frailty model, hierarchical model, indirect effect") == FALSE) %>%
  filter(str_detect(kw_final, "cerrado savanna") == FALSE) %>%
  # filter(str_detect(kw_final,"brazilian savanna")==FALSE) %>%
  # filter(str_detect(kw_final,"bolivian savanna")==FALSE) %>%
  filter(str_detect(kw_final, "ivoire grass hyparrhenia diplandra. nitrogen use efficien") == FALSE)

cerrado_kw <- keywords_unique %>%
  filter(str_detect(kw_final, "cerrado") == TRUE) %>%
  filter(str_detect(kw_final, "carvocaraceae cerrado") == FALSE) %>%
  filter(str_detect(kw_final, "aconophora teligera ant araliaceae brazil cerrado didymo") == FALSE) %>%
  filter(str_detect(kw_final, "amazonia-cerrado transition") == FALSE) %>%
  filter(str_detect(kw_final, "autccology bolivia cerrado gramineae kranz anatomy neotropics savanna") == FALSE)



grassland_kw <- keywords_unique %>%
  filter(str_detect(kw_final, "grassland") == TRUE)


usa_kw <- keywords_unique %>%
  filter(str_detect(kw_final, "(usa)") == TRUE) %>%
  # filter(str_detect(kw_final,"usa")==TRUE) %>%
  # filter(str_detect(kw_final,"brazil fire fleshy fruit frugivory fruit shortage reproduction savanna vertebrates")==FALSE) %>%
  # filter(str_detect(kw_final,"autccology bolivia cerrado gramineae kranz anatomy neotropics savanna")==FALSE) %>%
  # filter(str_detect(kw_final,"facilitation, frailty model, hierarchical model, indirect effect")==FALSE) %>%
  # filter(str_detect(kw_final,"cerrado savanna")==FALSE) %>%
  # # filter(str_detect(kw_final,"brazilian savanna")==FALSE) %>%
  # filter(str_detect(kw_final,"bolivian savanna")==FALSE) %>%
  filter(str_detect(kw_final, "usa") == TRUE)


# word association
# https://uc-r.github.io/word_relationships
txt_df <- grassland_kw %>%
  mutate(line = nrow(grassland_kw)) %>%
  relocate(line, .before = 1) %>%
  rename(text = kw_final)

foo <- txt_df %>% unnest_tokens(word, text)
# install.packages("widyr")
library(widyr)
(word_pairs <- foo %>%
  pairwise_count(word, line, sort = TRUE))



(word_cor <- foo %>%
  arrange(line) %>%
  group_by(line) %>%
  filter(n() >= 3) %>%
  pairwise_cor(word, line) %>%
  filter(!is.na(correlation)))






# %>%
#   filter(str_detect(kw_final,"carvocaraceae cerrado")==FALSE) %>%
#   filter(str_detect(kw_final,"aconophora teligera ant araliaceae brazil cerrado didymo")==FALSE) %>%
#   filter(str_detect(kw_final,"amazonia-cerrado transition")==FALSE) %>%
#   filter(str_detect(kw_final,"autccology bolivia cerrado gramineae kranz anatomy neotropics savanna")==FALSE)


# library(SemNetCleaner)
# singularize("grasslands")


# keywords<-keywords %>%
#   mutate(kw_major_cat = case_when(
#     str_detect(kw_final,"dry")==TRUE~"TRUE",






















foo <- keywords %>%
  group_by(article_cat, kw_final) %>%
  summarize(n = n()) %>%
  arrange(desc(n))

# https://cran.r-project.org/web/packages/stringdist/stringdist.pdf

library(stringdist)

parents_name <- keywords_unique %>% slice(1:200)
parents_name <- parents_name$kw_final

person_id <- 1:length(parents_name)

family_id <- vector("integer", length(parents_name))


# Looping through unassigned family ids
while (sum(family_id == 0) > 0) {
  ids <- person_id[family_id == 0]

  dists <- stringdist(parents_name[family_id == 0][1],
    parents_name[family_id == 0],
    method = "lv"
  )

  matches <- ids[dists <= 2]

  family_id[matches] <- max(family_id) + 1
}

result <- data.frame(person_id, parents_name, family_id)

result <- result %>%
  arrange(family_id) %>%
  group_by(family_id) %>%
  mutate(n = n()) %>%
  arrange(desc(n))
result

# abiotic
# biotic
# abundance/range-size relationship
# acer rubrum, betula alleghaniensis, betula papyrifera
# anagrus spp
# anas spp.
# anagrus spp.
# análise de isótopos estáveis
# alnus-crispa
# alnus-rubra
# to find the "tropical" stirngs search ab, ti, de, and other
# https://www.statology.org/r-check-if-column-contains-string/
# https://www.statology.org/str_detect-in-r/


# save these and then consolidate in openrefine
# write_csv(kw,"./bibliometrics/data_raw/kw_27june_to_refine.csv")

# pool kw using open_refine ---------------------------------------------------

# CLEANED IN OPEN REFINE. EXPORT THE REFINED DATASET
# OPEN REFINE PROHECT 2475241490253
# kw_refined<-read_csv("./bibliometrics/keyword_analysis/kw-27june-to-refine-csv.csv")
#
# kw_refined<-read_csv("./bibliometrics/keyword_analysis/kw-27june-to-refine-csv.csv") %>%
#   mutate(kw = case_when(
#     kw ==  "ant plant interactions" ~ "ant-plant interactions",
#     kw ==  "tropical forests" ~ "tropical forest",
#     kw ==  "germination" ~ "seed germination",
#     kw ==  "litter" ~ "litterfall",
#     kw ==  "tropical" ~ "tropics/tropical",
#     kw ==  "tropics" ~ "tropics/tropical",
#     kw ==  "ant plant interactions" ~ "ant-plant interactions",
#     kw ==  "tropical forests" ~ "tropical forest",
#     kw ==  "germination" ~ "seed germination",
#     kw ==  "litter" ~ "litterfall",
#     kw ==  "tropical" ~ "tropics/tropical",
#     kw ==  "tropics" ~ "tropics/tropical",
#     kw ==  "tropical forestsuccession" ~ "tropical forest succession/regeneration",
#     kw ==  "tropical forest succession" ~ "tropical forest succession/regeneration",
#     kw ==  "tropical forest regeneration" ~ "tropical forest succession/regeneration",
#     kw ==  "tropical forests" ~ "tropical forest(s)",
#     kw ==  "tropical forest" ~ "tropical forest(s)",
#     kw ==  "tropical forest fragmentation" ~ "tropical forest fragments/fragmentation",
#     kw ==  "tropical forest fragments" ~ "tropical forest fragments/fragmentation",
#     kw ==  "tropical forest fragment" ~"tropical forest fragments/fragmentation",
#     kw ==  "tropical forest manage-" ~ "tropical forest management",
#     kw ==  "invertebrate herbivory" ~ "invertebrate herbivory/herbivore(s)",
#     kw ==  "invertebrate herbivores" ~ "invertebrate herbivory/herbivore(s)",
#     kw ==  "invertebrate community structure" ~ "invertebrate communities/structure",
#     kw ==  "invertebrate communities" ~ "invertebrate communities/structure",
#     kw ==  "invertebrate predators" ~ " invertebrate predator(s)",
#     kw ==  "invertebrate predator" ~ " invertebrate predator(s)",
#     kw ==  "seed dispersal networks" ~ "seed dispersal network(s)",
#     kw ==  "seed dispersal network" ~ "seed dispersal network(s)",
#     kw ==  "seed dispersal kernels" ~ "seed dispersal kernel(s)",
#     kw ==  "seed dispersal kernel" ~ "seed dispersal kernel(s)",
#     kw ==  "seed-dispersal mutualism" ~ "seed dispersal mutualism(s)",
#     kw ==  "seed dispersal mutualisms" ~ "seed dispersal mutualism(s)",
#     kw ==  "drosophila" ~ "drosophila / d melanogaster",
#     kw ==  "drosophila melanogaster" ~ "drosophila / d melanogaster",
#     kw ==  "dispersal modes" ~ "dispersal mode(s)",
#     kw ==  "dispersal mode" ~ "dispersal mode(s)",
#     kw ==  "dispersal models" ~ "dispersal model(s)",
#     kw ==  "dispersal model" ~ "dispersal model(s)",
#     kw ==  "foraging mode" ~ "foraging mode(s)",
#     kw ==  "foraging models" ~ "foraging model(s)",
#     kw ==  "foraging model" ~ "foraging model(s)",
#     kw ==  "drosophila melanogaster" ~ "drosophila / d melanogaster",
#     kw ==  "cerraddo" ~ "cerrado",
#     kw ==  "path analyses" ~ "path analysis",
#     kw ==  "feather" ~ "feather(s)",
#     kw ==  "feathers" ~ "feather(s)",
#     kw ==  "p ratio" ~ "P ratio(s)",
#     kw ==  "p ratios" ~ "P ratio(s)",
#     kw ==  "x choromosomes" ~ "x choromosomes",
#     TRUE ~ as.character(kw))) %>%
#   mutate(kw = case_when(
#     kw ==  "rain forest" ~ "(tropical) rain forest(s)",
#     kw ==  "diversity" ~ "(species) diversity/biodiversity",
#     kw ==  "species diversity" ~ "(species) diversity/biodiversity",
#     kw ==  "biodiversity" ~ "(species) diversity/biodiversity",
#     kw ==  "tropical rainforest"~"(tropical) rain forest(s)",
#     kw ==  "redundancy analysis (rda)" ~ "redundancy analysis",
#     kw ==  "plant community" ~ "plant communities",
#     kw ==  "pinus plantations"~"pine plantation(s)",
#     kw ==  "pinus plantation"~"pine plantation(s)",
#     kw ==  "lowland tropical forest" ~ "lowland tropical rain forest",
#     kw ==  "vapour pressure deficits" ~ "vapor pressure deficit",
#     kw ==  "megafaunal-dispersal syndrome" ~ "megafaunal dispersal syndrome",
#     kw ==  "amazon" ~ "amazon(ia)",
#     kw ==  "land use" ~ "land-use",
#     kw ==  "ant" ~ "ant(s)",
#     kw ==  "ants" ~ "ant(s)",
#     kw ==  "mammal" ~ "mammalia",
#     kw ==  "mammals" ~ "mammalia",
#     kw ==  "oil-palm" ~ "oil palm",
#     kw ==  "fig" ~ "fig(s)",
#     kw ==  "rodent" ~ "rodentia",
#     kw ==  "bambusoideae" ~ "bambuseae",
#     kw ==  "relative growth" ~ "relative growth (rate)",
#     kw ==  "relative growth rate" ~ "relative growth (rate)",
#     kw ==  "tropical montane cloud forest"~"tropical montane forest",
#     kw ==  "reduced impact logging" ~ "reduced-impact logging",
#     kw ==  "afrotropical" ~ "afrotropics",
#     kw ==  "insectivores" ~ "insectivores/insectivory",
#     kw ==  "insectivory" ~ "insectivores/insectivory",
#     kw ==  "land use history" ~ "land-use history",
#     kw ==  "bird community" ~ "bird communities",
#     kw ==  "agriculture intensification" ~ "agricultural intensification",
#     kw ==  "camera trapping" ~ "camera trap(ping)",
#     kw ==  "camera trap" ~ "camera trap(ping)",
#     kw ==  "leaf litterfall" ~ "leaf litter",
#     kw ==  "liana-tree interaction" ~ "liana-tree interaction (network)",
#     kw ==  "liana-tree interaction network" ~ "liana-tree interaction (network)",
#     kw ==  "canopy openness" ~ "canopy openness/openings",
#     kw ==  "canopy openings" ~ "canopy openness/openings",
#     kw ==  "insect-plant interactions" ~ "plant-insect interaction",
#     kw ==  "thermal performance" ~ "thermal performance (curves)",
#     kw ==  "thermal performance curves" ~ "thermal performance (curves)",
#     kw ==  "atlantic rain forest biome" ~ "atlantic rain forest",
#     kw ==  "atlantic rainforest" ~ "atlantic rain forest",
#     kw ==  "arboreal" ~ "arboreal/arboreality",
#     kw ==  "arboreality" ~ "arboreal/arboreality",
#     kw ==  "resprout" ~ "resprout(ing)",
#     kw ==  "resprouting" ~ "resprout(ing)",
#     kw ==  "land use change" ~ "land-use change",
#     kw ==  "forest canopies" ~ "forest canopy",
#     kw ==  "tropical mountain forests"~"tropical montane forest",
#     kw ==  "decomposition" ~ "decomposition rate",
#     kw ==  "albertine rift eco-region"~"albertine rift region",
#     kw ==  "climatic change" ~ "climate change",
#     kw ==  "neotropical" ~ "neotropics",
#     kw ==  "psittacidae" ~ "psittacids",
#     kw ==  "psittacines" ~ "psittacids",
#     kw ==  "pan troglodytes verus" ~ "pan troglodytes",
#     kw ==  "anura" ~ "anurans",
#     kw ==  "la selva biological research station" ~ "la selva biological station",
#     kw ==  "animal-plant interaction"~"plant-animal interactions",
#     kw ==  "long distance dispersal" ~ "long-distance dispersal",
#     kw ==  "twig-nesting ant species" ~ "twig-nesting ants",
#     kw ==  "tropical mountain cloud forest" ~ "tropical montane cloud forest",
#     kw ==  "life histories" ~ "life history",
#     kw ==  "seasonally dry tropical forest" ~ "tropical dry forest",
#     kw ==  "seasonal dry tropical forest" ~ "tropical dry forest",
#     kw ==  "dry-season flushing" ~ "dry season flushing",
#     kw ==  "photosynthesis rates" ~ "photosynthesis (rates)",
#     kw ==  "photosynthesis" ~ "photosynthesis (rates)",
#     kw ==  "janzen-connell model" ~ "janzen-connell",
#     kw ==  "termitidae" ~ "termite",
#     kw ==  "carbon dioxide (co2)" ~ "carbon dioxide",
#     kw ==  "tropical montane" ~ "tropical montane forest",
#     kw ==  "tropical lowland forests" ~ "tropical lowland rain forest(s)",
#     kw ==  "atlantic rain forest biome"~"atlantic rain forest",
#     kw ==  "symbiotic microbiota" ~ "symbiotic microbes",
#     kw ==  "caribbean sea" ~ "caribbean",
#     kw ==  "post-dispersal predation" ~ "postdispersal seed predation",
#     kw ==  "phyllostomid bats" ~ "phyllostomidae",
#     kw ==  "life table response experiment" ~ "life table response experiments",
#     kw ==  "tropical rainforest" ~ "tropical rain forest(s)",
#     TRUE ~ as.character(kw))) %>%
#   mutate(kw=tolower(kw)) %>%
#   mutate(kw=gsub("\"","",kw)) %>%
#   mutate(kw=gsub("\'","",kw)) %>%
#   mutate(kw=trimws(kw)) %>%
#   arrange(kw)

# open refine project2040771042669
# kw_refined2<-read_csv("./bibliometrics/keyword_analysis/kw28june-csv.csv") %>%
#   mutate(kw_to_refine=gsub("above-ground","aboveground",kw_to_refine)) %>%
#   mutate(kw_to_refine=gsub("below-ground","belowground",kw_to_refine)) %>%
#   mutate(kw_to_refine=gsub("below- ","belowground",kw_to_refine)) %>%
#   mutate(kw_to_refine=gsub("above- ","aboveground",kw_to_refine)) %>%
#   mutate(kw_to_refine = case_when(
#     kw_to_refine ==  "b matrix" ~ "b-matrix",
#     kw_to_refine ==  "type 1 error"~"type-1 error",
#     kw_to_refine ==  "type i error"~"type-1 error",
#     kw_to_refine ==  "c : p ratio"~"cp ratio",
#     kw_to_refine ==  "c : p ratios"~"cp ratio",
#     kw_to_refine ==  "k : p ratio"~"kp ratio",
#     kw_to_refine ==  "k : p ratios"~"kp ratio",
#     kw_to_refine ==  "n : k ratio"~"nk ratio",
#     kw_to_refine ==  "n : k ratios"~"nk ratio",
#     kw_to_refine ==  "n : p ratios"~"np ratios",
#     kw_to_refine ==  "g matrix"~"g-matrix",
#     kw_to_refine ==  "b matrix"~"b-matrix",
#     kw_to_refine ==  "m matrix"~"m-matrix",
#     kw_to_refine ==  "p matrix"~"p-matrix",
#     kw_to_refine ==  "barro colorado island"~"BCI",
#     kw_to_refine ==  "barro colorado-island"~"BCI",
#     kw_to_refine ==  "barro-colorado island"~"BCI",
#     kw_to_refine ==  "burro colorado island"~"BCI",
#     kw_to_refine ==  "site dependence"~"site-dependence",
#     kw_to_refine ==  "site-dependence"~"site-dependence",
#     kw_to_refine ==  "site-dependency"~"site-dependence",
#     kw_to_refine ==  "b-chromosomes"~"b-chromosome",
#     kw_to_refine ==  "f statistics"~"f-statistics",
#     kw_to_refine ==  "np ratio"~"np ratio(s)",
#     kw_to_refine ==  "np ratios"~"np ratio(s)",
#     kw_to_refine ==  "n limitation"~"n-limitation",
#     kw_to_refine ==  "rapid biodiversity assessment protocol"~"rapid biodiversity assessment",
#     kw_to_refine ==  "noninvasive sample"~"noninvasive sample/sampling",
#     kw_to_refine ==  "noninvasive sampling"~"noninvasive sample/sampling",
#     kw_to_refine ==  "road"~"roads",
#     kw_to_refine ==  "varillales"~"varillal",
#     kw_to_refine ==  "palm"~"palm(s)",
#     kw_to_refine ==  "palms"~"palm(s)",
#     kw_to_refine ==  "bird"~"bird(s)",
#     kw_to_refine ==  "birds"~"bird(s)",
#     kw_to_refine ==  "abiotic&#8208"~"abiotic",
#     kw_to_refine == "abundant centre model"~"abundant center model",
#     kw_to_refine == "barley and cereal yellow dwarf virus"~"barley and cereal yellow dwarf viruses",
#     kw_to_refine == "acer opalus ssp granatense"~"acer opalus",
#     kw_to_refine == "acer opalus subsp granatense"~"acer opalus",
#     kw_to_refine == "australian monsoon tropics"~"australian monsoonal tropics",
#     kw_to_refine == "adaptation and trade-off"~"adaptations and trade-offs",
#     kw_to_refine == "biodiversity and ecosystem functioning"~"biodiversity and ecosystem function",
#     kw_to_refine == "alternative stable state"~"alternate stable state",
#     kw_to_refine == "anas plathyrynchos"~"anas platyrhynchos",
#     kw_to_refine == "asymmetric competition"~"asymmetrical competition",
#     kw_to_refine == "binomial mixture model"~"binomial n-mixture model",
#     kw_to_refine == "barley yellow dwarf virus (bydv)"~"barley and cereal yellow dwarf viruses",
#     kw_to_refine == "barley yellow dwarf viruses (bydvs)"~"barley and cereal yellow dwarf viruses",
#     kw_to_refine == "arbuscular mycorrhiza"~"arbuscular myccorrhyiza",
#     kw_to_refine == "aguoti paca"~"agouti paca",
#     kw_to_refine == "anti-predator behavior"~"antipredatory behavior",
#     kw_to_refine == "below-ground process"~"below-ground processes",
#     kw_to_refine == "behavioural tradeoff"~"behavioral trade-off",
#     kw_to_refine == "anti-plant"~"ant-plant",
#     kw_to_refine == "bayesian hierarchical modeling"~"bayesian hierarchical model",
#     kw_to_refine == "behavior genetics"~"behavioral genetics",
#     kw_to_refine == "alternate states"~"alternative states",
#     kw_to_refine == "arciidae"~"arctiidae",
#     kw_to_refine == "area-concentrated search"~"area-concentrated searching",
#     kw_to_refine == "behavioral changes"~"behavioral change",
#     kw_to_refine == "behavioural change"~"behavioral change",
#     kw_to_refine == "age-specific breeding probabilities"~"age-specific breeding probability",
#     kw_to_refine == "alternative reproductive strategies"~"alternative reproductive strategy",
#     kw_to_refine == "anadromous fish"~"anadromous fishes",
#     kw_to_refine == "bialowieza forest"~"biaowiea forest",
#     kw_to_refine == "above and belowground herbivores"~"above- and belowground herbivory",
#     kw_to_refine == "alternate prey"~"alternative prey",
#     kw_to_refine == "arciidae"~"ariidae",
#     kw_to_refine == "anthropogenic stress"~"anthropogenic stressors",
#     kw_to_refine == "biogeochemical model"~"biogeochemical modeling",
#     kw_to_refine == "ant assemblages"~"bat assemblages",
#     kw_to_refine == "ant pollination"~"bat pollination",
#     kw_to_refine == "arboreal ants"~"arboreal plants",
#     kw_to_refine == "allogenic ecosystem engineers"~"autogenic ecosystem engineers",
#     kw_to_refine == "basic reproductive number"~"basic reproductive number r-0",
#     kw_to_refine == "alternative mating strategies"~"alternative mating strategy",
#     kw_to_refine == "above-ground competition"~"above-ground competition cue",
#     kw_to_refine == "agelaia"~"aglaia",
#     kw_to_refine == "agro-ecosystem"~"agroecosystems",
#     kw_to_refine == "akaike information criterion"~"akaikes information criteria",
#     kw_to_refine == "annual grass"~"annual grasses",
#     kw_to_refine == "baetids"~"baetis",
#     kw_to_refine == "bioenergetic model"~"bioenergetic modeling",
#     kw_to_refine == "biodiversity and ecosystem function (bef)"~"biodiversity and ecosystem function",
#     kw_to_refine == "altitudinal migrant"~"altitudinal migration",
#     kw_to_refine == "centre-periphery hypothesis"~"center-periphery hypothesis",
#     kw_to_refine == "borrelia burdgorferi"~"borrelia burgdorferi",
#     kw_to_refine == "blue-green aglae"~"blue-green algae",
#     kw_to_refine == "coastal temperate rain forests"~"coastal temperate rainforest",
#     kw_to_refine == "c-4 grassland"~"c-4 grasslands",
#     kw_to_refine == "brachyramphus marmoratus"~"brachyramphus marmotus",
#     kw_to_refine == "coadapted gene complex"~"coadapted gene complexes",
#     kw_to_refine == "coffee agro-ecosystems"~"coffee agroecosystem",
#     kw_to_refine == "comparative approach"~"comparative approaches",
#     kw_to_refine == "capreolus capreolus"~"capreolus capreolus l",
#     kw_to_refine == "cedar creek natural history area, minnesota"~"cedar creek natural history area, minnesota, usa",
#     kw_to_refine == "branching process"~"branching processes",
#     kw_to_refine == "coffee agroforest"~"coffee agroforestry",
#     kw_to_refine == "bloflies"~"blowflies",
#     kw_to_refine == "carry-over effects"~"carryover effect",
#     kw_to_refine == "colombian amazonia"~"colombian amazon",
#     kw_to_refine == "complementary resource use"~"complementary resources",
#     kw_to_refine == "catastrophic regime shifts"~"catastrophic regime-shift",
#     kw_to_refine == "coefficient of additive genetic variance"~"coefficient of additive genetic variation",
#     kw_to_refine == "coevolutionary arm races"~"coevolutionary arms race",
#     kw_to_refine == "chasmagnathus granulata"~"chasmagnathus granulatus",
#     kw_to_refine == "bodega marine reserve, california"~"bodega marine reserve, california (usa)",
#     kw_to_refine == "body size distribution"~"brood size distribution",
#     kw_to_refine == "coastal marsh"~"coastal marshes",
#     kw_to_refine == "community assembly by trait selection, cats"~"community assembly by trait selection",
#     kw_to_refine == "breeding age"~"breeding range",
#     kw_to_refine == "caesium"~"cesium",
#     kw_to_refine == "community function"~"community functioning",
#     kw_to_refine == "california floristic province, usa"~"california floristic province",
#     kw_to_refine == "chihuahuan desert, new mexico"~"chihuahuan desert, new mexico, usa",
#     kw_to_refine == "compensatory growth"~"compensatory regrowth",
#     kw_to_refine == "climate-growth relation"~"climate-growth relationship",
#     kw_to_refine == "community dynamic model"~"community dynamics modeling",
#     kw_to_refine == "competition for pollination"~"competition for pollinators",
#     kw_to_refine == "GxE interactions(s)"~"GxE interaction(s)",
#     kw_to_refine == "top-down vs. bottom-up control"~"bottom-up vs top-down control",
#     kw_to_refine == "50-ha forest dynamics plot"~"50-ha plot",
#     kw_to_refine == "<bold>g</bold>-matrix"~"g matrix",
#     kw_to_refine == "acacia species"~"acacia",
#     kw_to_refine == "africa, bat reproduction"~"african bat reproduction",
#     kw_to_refine == "age-specific reproduction"~"age-specific reproduction/survival",
#     kw_to_refine == "age-specific reproduction and survival"~"age-specific reproduction/survival",
#     kw_to_refine == "age-specific reproductive success"~"age-specific reproduction/survival",
#     kw_to_refine == "age-specific survival"~"age-specific reproduction/survival",
#     kw_to_refine == "aboveground annual net primary productivity"~"anpp",
#     kw_to_refine == "above ground net primary productivity (anpp)"~"anpp",
#     kw_to_refine == "aboveground net primary production (anpp)"~"anpp",
#     kw_to_refine == "age-dependent mortality demography"~"age-dependent mortality",
#     kw_to_refine == "age-from-stage modeling"~"age-from-stage models",
#     kw_to_refine == "abandoned agricultural lands"~"abandoned agriculture",
#     kw_to_refine == "abandoned cattle pastures"~"abandoned pastures",
#     kw_to_refine == "abandoned pasture"~"abandoned pastures",
#     kw_to_refine == "abandoned farmland"~"abandoned fields",
#     kw_to_refine == "16s rdna"~"16s rdna/rrna",
#     kw_to_refine == "16s"~"16s rdna/rrna",
#     kw_to_refine == "16s rdna sequencing"~"16s rdna/rrna",
#     kw_to_refine == "coevoltion"~"coevolution",
#     kw_to_refine == "16s rrna"~"16s rdna/rrna",
#     kw_to_refine == "16s rrna genes"~"16s rdna/rrna",
#     kw_to_refine == "16s-rrna and its gene sequencing"~"16s rdna/rrna",
#     TRUE ~ as.character(kw_to_refine))) %>%
#   mutate(kw_to_refine=tolower(kw_to_refine)) %>%
#   mutate(kw_to_refine=trimws(kw_to_refine)) %>%
#   rename(kw=kw_original) %>%
#   rename(kw_refined=kw_to_refine)
#
#
#
#
# summary(kw_refined$kw==kw_refined$kw_refined)
#

kw_refined <- left_join(kw_refined, kw_refined2) %>%
  select(-kw) %>%
  filter(kw_refined != "18th&#8211") %>%
  filter(kw_refined != "&#160") %>%
  filter(kw_refined != "&#8208") %>%
  filter(kw_refined != "&#8722") %>%
  filter(kw_refined != "&#946") %>%
  filter(kw_refined != "&#947") %>%
  filter(kw_refined != "co&#8208") %>%
  filter(kw_refined != "non&#8208") %>%
  filter(kw_refined != "wood&#8208") %>%
  filter(kw_refined != "mixed&#8208")

final_kw <- read_csv("./bibliometrics/data_clean/final_kw.csv") %>%
  rename(DI = DOI) %>%
  rename(DE = kw_output) %>%
  relocate(DE, .after = DI)

merged_refs <- read_csv("./bibliometrics/data_clean/merged_refs.csv") %>%
  select(refID, jrnl_cat, SO, PY, DI)

final_kw <- left_join(final_kw, merged_refs) %>%
  relocate((refID:PY), .after = DI) %>%
  separate(DE, c(LETTERS[seq(from = 1, to = 20)]), sep = "\\|") %>%
  pivot_longer(!DI:PY, names_to = "letter", values_to = "DE") %>%
  select(-letter) %>%
  drop_na(DE) %>%
  mutate(DE = tolower(DE)) %>%
  mutate(DE = trimws(DE)) %>%
  rename(kw_refined = DE)

# summary(is.na(kw_refined$kw_refined))

kw_refined <- bind_rows(final_kw, kw_refined)

kw_refined <- write_csv(kw_refined, "./bibliometrics/data_clean/kw_refined.csv")


# clustering --------------------------------------------------------------
# https://cran.r-project.org/web/packages/textmineR/vignettes/b_document_clustering.html
library(textmineR)

kw <- kw_refined %>%
  filter(!is.na(jrnl_cat)) %>%
  # filter(jrnl_cat=="tropical") %>%
  group_by(jrnl_cat) %>%
  sample_n(1000)


kw_dtm <- kw %>%
  count(jrnl_cat, kw_refined) %>%
  rename(document = jrnl_cat, term = kw_refined, count = n)


dtm <- CreateDtm(
  doc_vec = kw_dtm$term, # character vector of documents
  doc_names = kw_dtm$document, # document names
  ngram_window = c(1, 4), # minimum and maximum n-gram length
  stopword_vec = c(
    stopwords::stopwords("en"), # stopwords from tm
    stopwords::stopwords(source = "smart")
  ), # this is the default value
  lower = TRUE, # lowercase - this is the default value
  remove_punctuation = FALSE, # punctuation - this is the default
  remove_numbers = FALSE, # numbers - this is the default
  verbose = FALSE, # Turn off status bar for this demo
  cpus = 2
) # default is all available cpus on the system


tf_mat <- TermDocFreq(dtm)


# TF-IDF and cosine similarity
tfidf <- t(dtm[, tf_mat$term]) * tf_mat$idf

tfidf <- t(tfidf)

csim <- tfidf / sqrt(rowSums(tfidf * tfidf))

csim <- csim %*% t(csim)

cdist <- as.dist(1 - csim)


hc <- hclust(cdist, "ward.D")

clustering <- cutree(hc, 4)

plot(hc,
  main = "Hierarchical clustering of 100 NIH grant abstracts",
  ylab = "", xlab = "", yaxt = "n"
)

rect.hclust(hc, 10, border = "red")


p_words <- colSums(dtm) / sum(dtm)

cluster_words <- lapply(unique(clustering), function(x) {
  rows <- dtm[clustering == x, ]

  # for memory's sake, drop all words that don't appear in the cluster
  rows <- rows[, colSums(rows) > 0]

  colSums(rows) / sum(rows) - p_words[colnames(rows)]
})


# create a summary table of the top 5 words defining each cluster
cluster_summary <- data.frame(
  cluster = unique(clustering),
  size = as.numeric(table(clustering)),
  top_words = sapply(cluster_words, function(d) {
    paste(
      names(d)[order(d, decreasing = TRUE)][1:5],
      collapse = ", "
    )
  }),
  stringsAsFactors = FALSE
)
cluster_summary




kw <- kw_refined %>%
  select(kw_refined) %>%
  distinct(kw_refined)


library(sjmisc)
old_string <- kw$kw_refined
newstring <- group_str(old_string)
newstring <- as_tibble(newstring) %>%
  separate(value, sep = ",", into = c("first", "second", "third"))
# Christensen, A. P., & Kenett, Y. (2019, October 22).
# Semantic Network Analysis (SemNA): A Tutorial on Preprocessing, Estimating,
# and Analyzing Semantic Networks. https://doi.org/10.1037/met0000463
library(SemNetCleaner)
library(SemNeT)
load.dictionaries("general")
kw <- kw_refined %>% select(kw_refined)
clean <- textcleaner(
  data = kw, miss = 99,
  partBY = "row", dictionary = "general"
)










# kw_to_refine_28june<-kw_refined %>%
#   select(kw_refined) %>%
#   group_by(kw_refined) %>%
#   slice(1) %>%
#   mutate(kw_to_refine=kw_refined) %>%
#   rename(kw_original=kw_refined)
#
# write_csv(kw_to_refine_28june, "./bibliometrics/keyword_analysis/kw28june.csv")

# use tidystringdist to continue finding similars

#
#
#
# kw_for_comp<-as_tibble(unique(kw_refined$kw_refined)) %>% rename(kw=value) %>% arrange()
# kw_for_comp2<-kw_for_comp %>%
#   mutate(kw2=str_sub(kw, start=-9L, end=-2L)) %>%
#   mutate(kw3=str_sub(kw, start=-1L, end=-1L)) %>%
#   mutate(kw3=if_else(kw3=="s","s",""))  %>%
#   group_by(kw2) %>%
#   add_count(kw2) %>%
#   separate(kw, sep=" ", into=c("first","second","third")) %>%
#   arrange(first,second,kw3)
#
# write_csv(kw_for_comp2, "./bibliometrics/keyword_analysis/kw_for_comp2.csv")
#
#
# tidy_comb_kw <- tidy_comb_all(kw_for_comp$kw[40000:50000], kw_for_comp$kw[40000:50000])
# # tidy_comb_kw <- tidy_comb_all(kw_for_comp$kw, kw_for_comp$kw)
# kw_string_comp<-tidy_stringdist(tidy_comb_kw)
# kw_string_comp<-kw_string_comp %>%
#   filter(jw<0.05) %>%
#   filter(jw!=0) %>%
#   arrange(jw)
#
#
#
# additional_corrections<-paste("kw_to_refine == \"",kw_string_comp$V1,"\"~\"",kw_string_comp$V2,"\",",sep="") %>% tibble()
# write_csv(additional_corrections, "./bibliometrics/keyword_analysis/additional_corrections.csv")
