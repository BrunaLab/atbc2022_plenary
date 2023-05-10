
# load libraries ----------------------------------------------------------
library(tidyverse)
library(stopwords)
library(ngram)
library(tidytext)
library(igraph)
library(tidystringdist)
library(SemNetCleaner)
library(SemNeT)

# Load reference records, clean keywords and text of DE and titles -----------------

complete_data<-read_csv("./bibliometrics/data_clean/complete_data.csv") %>% 
  mutate(TI=gsub(" - "," ",TI)) %>%
    replace_na(list(pub_cat = "temperate")) %>% 
    mutate(DE = gsub("\n"," ",DE)) %>% 
    mutate(DE = gsub("\"","",DE)) %>% 
    mutate(DE = gsub("<bold>","",DE)) %>% 
    mutate(DE = gsub("</bold>","",DE)) %>% 
    mutate(DE = gsub("- ","-",DE)) %>% 
    mutate(DE = gsub("r  theory *","r* theory",DE)) %>% 
    mutate(DE = gsub("&#8208","-",DE)) %>%
    mutate(DE = gsub("&#8211","-",DE)) %>%
    mutate(DE = gsub("&#8217","'",DE)) %>%
    mutate(DE = gsub("&amp","&",DE)) 
unique(complete_data$SO) 

# open refine  ------------------------------------------------------------------

# Take the KW column and split it up, arrange as a column
keywords<-complete_data %>% 
  select(refID,DE) %>% 
  drop_na(DE) %>% 
  rename(kw_original=DE) %>% 
#   some of the key words are sperated by ",". This complicates things because some key words are also in the 
# format "biomass, microbial" (instead of microbial biomass"). Also have "STRI, Panama" or "Montana, USA" But easier to split, i think.
  mutate(kw_original = gsub(",",";",kw_original)) %>% 
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
  mutate(kw_original=trimws(kw_original)) %>% 
  mutate(kw_original=gsub("\n"," ",kw_original)) %>% 
  # mutate(kw_original=str_replace('\\n"', '')) %>% 
  mutate(kw_original=tolower(kw_original))

keywords %>% summarize(n_distinct(kw_original))

keywords<-keywords %>% 
  mutate(kw_original=gsub("above-ground","aboveground",kw_original)) %>%
  mutate(kw_original=gsub("below-ground","belowground",kw_original)) %>%
  mutate(kw_original=gsub("below- ","belowground",kw_original)) %>%  
  mutate(kw_original=gsub("above- ","aboveground",kw_original)) %>%
  mutate(kw_original=gsub("c-3"," c3",kw_original)) %>%
  mutate(kw_original=gsub("c-4"," c4",kw_original)) %>%
  mutate(kw_refined_manual = case_when(
    kw_original ==  "ant plant interactions" ~ "ant-plant interactions",
    # kw_original ==  "ant plant interactions" ~ "ant-plant interactions",
    # kw_original ==  "tropical forests" ~ "tropical forest",
    # kw_original ==  "tropical forests" ~ "tropical forest",
    kw_original ==  "germination" ~ "seed germination",
    # kw_original ==  "litter" ~ "litterfall",
    kw_original ==  "litterfall" ~ "litter",
    # kw_original ==  "tropical" ~ "tropics/tropical",
    # kw_original ==  "tropics" ~ "tropics/tropical",
    kw_original ==  "germination" ~ "seed germination",
    # kw_original ==  "tropical" ~ "tropics/tropical",
    # kw_original ==  "tropics" ~ "tropics/tropical",
    # kw_original ==  "tropical forestsuccession" ~ "tropical forest succession/regeneration",
    kw_original ==  "tropical forestsuccession" ~ "tropical forest succession",
    # kw_original ==  "tropical forest succession" ~ "tropical forest succession/regeneration",
    # kw_original ==  "tropical forest regeneration" ~ "tropical forest succession/regeneration",
    # kw_original ==  "tropical forests" ~ "tropical forest(s)",
    # kw_original ==  "tropical forest" ~ "tropical forest(s)",
    # kw_original ==  "tropical forest fragmentation" ~ "tropical forest fragments/fragmentation",
    # kw_original ==  "tropical forest fragments" ~ "tropical forest fragmentation",
    # kw_original ==  "tropical forest fragment" ~"tropical forest fragmentation",
    # kw_original ==  "tropical forest fragments" ~ "tropical forest fragments/fragmentation",
    # kw_original ==  "tropical forest fragment" ~"tropical forest fragments/fragmentation",
    kw_original ==  "tropical forest manage-" ~ "tropical forest management",
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
    kw_original ==  "cerraddo" ~ "cerrado",
    kw_original ==  "path analyses" ~ "path analysis",
    # kw_original ==  "feather" ~ "feather(s)",
    # kw_original ==  "feathers" ~ "feather(s)",
    # kw_original ==  "p ratio" ~ "P ratio(s)",
    # kw_original ==  "p ratios" ~ "P ratio(s)",
    # kw_original ==  "x choromosomes" ~ "x choromosomes",
    # kw_original ==  "rain forest" ~ "(tropical) rain forest(s)",
    kw_original ==  "rain forest" ~ "tropical rain forest",
    # kw_original ==  "diversity" ~ "(species) diversity/biodiversity",
    # kw_original ==  "species diversity" ~ "(species) diversity/biodiversity",
    kw_original ==  "species diversity" ~ "diversity",
    kw_original ==  "biodiversity" ~ "diversity",
    # kw_original ==  "biodiversity" ~ "(species) diversity/biodiversity",
    # kw_original ==  "tropical rainforest"~"(tropical) rain forest(s)",
    kw_original ==  "tropical rainforest"~"tropical rain forest",
    kw_original ==  "redundancy analysis (rda)" ~ "redundancy analysis",
    # kw_original ==  "plant community" ~ "plant communities",
    # kw_original ==  "pinus plantations"~"pine plantation(s)",
    # kw_original ==  "pinus plantation"~"pine plantation(s)",
    # kw_original ==  "lowland tropical forest" ~ "lowland tropical rain forest",
    # kw_original ==  "vapour pressure deficits" ~ "vapor pressure deficit",
    kw_original ==  "megafaunal-dispersal syndrome" ~ "megafaunal dispersal syndrome",
    # kw_original ==  "amazon" ~ "amazon(ia)",
    kw_original ==  "land use" ~ "land-use",
    # kw_original ==  "ant" ~ "ant(s)",
    # kw_original ==  "ants" ~ "ant(s)",
    # kw_original ==  "mammal" ~ "mammalia",
    # kw_original ==  "mammals" ~ "mammalia",
    kw_original ==  "oil-palm" ~ "oil palm",
    # kw_original ==  "fig" ~ "fig(s)",
    # kw_original ==  "rodent" ~ "rodentia",
    kw_original ==  "bambusoideae" ~ "bambuseae",
    # kw_original ==  "relative growth" ~ "relative growth (rate)",
    kw_original ==  "relative growth" ~ "relative growth rate",
    # kw_original ==  "relative growth rate" ~ "relative growth (rate)",
    # kw_original ==  "tropical montane cloud forest"~"tropical montane forest",
    kw_original ==  "reduced impact logging" ~ "reduced-impact logging",
    kw_original ==  "afrotropical" ~ "afrotropics",
    # kw_original ==  "insectivores" ~ "insectivores/insectivory",
    # kw_original ==  "insectivory" ~ "insectivores/insectivory",
    kw_original ==  "land use history" ~ "land-use history",
    # kw_original ==  "bird community" ~ "bird communities",
    kw_original ==  "agriculture intensification" ~ "agricultural intensification",
    # kw_original ==  "camera trapping" ~ "camera trap(ping)",
    # kw_original ==  "camera trap" ~ "camera trap(ping)",
    # kw_original ==  "leaf litterfall" ~ "leaf litter",
    kw_original ==  "leaf litterfall" ~ "litter",
    # kw_original ==  "liana-tree interaction" ~ "liana-tree interaction (network)",
    kw_original ==  "liana-tree interaction network" ~ "liana-tree interaction (network)",
    # kw_original ==  "canopy openness" ~ "canopy openness/openings",
    # kw_original ==  "canopy openings" ~ "canopy openness/openings",
    # kw_original ==  "insect-plant interactions" ~ "plant-insect interaction",
    # kw_original ==  "thermal performance" ~ "thermal performance (curves)",
    # kw_original ==  "thermal performance curves" ~ "thermal performance (curves)",
    # kw_original ==  "atlantic rain forest biome" ~ "atlantic rain forest",
    kw_original ==  "atlantic rainforest" ~ "atlantic rain forest",
    # kw_original ==  "arboreal" ~ "arboreal/arboreality",
    # kw_original ==  "arboreality" ~ "arboreal/arboreality",
    # kw_original ==  "resprout" ~ "resprout(ing)",
    # kw_original ==  "resprouting" ~ "resprout(ing)",
    kw_original ==  "land use change" ~ "land-use change",
    # kw_original ==  "forest canopies" ~ "forest canopy",
    kw_original ==  "tropical mountain forests"~"tropical montane forest",
    # kw_original ==  "decomposition" ~ "decomposition rate",
    kw_original ==  "albertine rift eco-region"~"albertine rift region",
    kw_original ==  "climatic change" ~ "climate change",
    # kw_original ==  "neotropical" ~ "neotropics",
    kw_original ==  "psittacidae" ~ "psittacids",
    kw_original ==  "psittacines" ~ "psittacids",
    kw_original ==  "pan troglodytes verus" ~ "pan troglodytes",
    kw_original ==  "anura" ~ "anurans",
    kw_original ==  "la selva biological research station" ~ "la selva biological station",
    kw_original ==  "animal-plant interaction"~"plant-animal interactions",
    kw_original ==  "long distance dispersal" ~ "long-distance dispersal",
    kw_original ==  "twig-nesting ant species" ~ "twig-nesting ants",
    kw_original ==  "tropical mountain cloud forest" ~ "tropical montane cloud forest",
    # kw_original ==  "life histories" ~ "life history",
    kw_original ==  "seasonally dry tropical forest" ~ "tropical dry forest",
    kw_original ==  "seasonal dry tropical forest" ~ "tropical dry forest",
    kw_original ==  "dry-season flushing" ~ "dry season flushing",
    # kw_original ==  "photosynthesis rates" ~ "photosynthesis (rates)",
    # kw_original ==  "photosynthesis" ~ "photosynthesis (rates)",
    kw_original ==  "janzen-connell model" ~ "janzen-connell",
    kw_original ==  "termitidae" ~ "termite",
    kw_original ==  "carbon dioxide (co2)" ~ "carbon dioxide",
    kw_original ==  "tropical montane" ~ "tropical montane forest",
    # kw_original ==  "tropical lowland forests" ~ "tropical lowland rain forest(s)",
    kw_original ==  "tropical lowland forests" ~ "tropical lowland rain forest",
    kw_original ==  "atlantic rain forest biome"~"atlantic rain forest",
    kw_original ==  "symbiotic microbiota" ~ "symbiotic microbes",
    kw_original ==  "caribbean sea" ~ "caribbean",
    kw_original ==  "post-dispersal predation" ~ "postdispersal seed predation",
    kw_original ==  "phyllostomid bats" ~ "phyllostomidae",
    kw_original ==  "life table response experiment" ~ "life table response experiments",
    # kw_original ==  "tropical rainforest" ~ "tropical rain forest(s)",
    kw_original == "b matrix" ~ "b-matrix",
    kw_original ==  "type 1 error"~"type-1 error",
    kw_original ==  "type i error"~"type-1 error",
    kw_original ==  "c : p ratio"~"cp ratio",
    kw_original ==  "c : p ratios"~"cp ratio",
    kw_original ==  "k : p ratio"~"kp ratio",
    kw_original ==  "k : p ratios"~"kp ratio",
    kw_original ==  "n : k ratio"~"nk ratio",
    kw_original ==  "n : k ratios"~"nk ratio",
    kw_original ==  "n : p ratios"~"np ratios",
    kw_original ==  "g matrix"~"g-matrix",
    kw_original ==  "b matrix"~"b-matrix",
    kw_original ==  "m matrix"~"m-matrix",
    kw_original ==  "p matrix"~"p-matrix",
    kw_original ==  "barro colorado island"~"bci",
    kw_original ==  "barro colorado-island"~"bci",
    kw_original ==  "barro-colorado island"~"bci",
    kw_original ==  "burro colorado island"~"bci",
    kw_original ==  "site-dependence"~"site-dependence",
    kw_original ==  "site-dependency"~"site-dependence",
    kw_original ==  "b-chromosomes"~"b-chromosome",
    kw_original ==  "f statistics"~"f-statistics",
    # kw_original ==  "np ratio"~"np ratio(s)",
    # kw_original ==  "np ratios"~"np ratio(s)",
    kw_original ==  "n limitation"~"n-limitation",
    kw_original ==  "rapid biodiversity assessment protocol"~"rapid biodiversity assessment",
    # kw_original ==  "noninvasive sample"~"noninvasive sample/sampling",
    # kw_original ==  "noninvasive sampling"~"noninvasive sample/sampling",
    # kw_original ==  "road"~"roads",
    kw_original ==  "varillales"~"varillal",
    # kw_original ==  "palm"~"palm(s)",
    # kw_original ==  "palms"~"palm(s)",
    # kw_original ==  "bird"~"bird(s)",
    # kw_original ==  "birds"~"bird(s)",
  #  kw_original ==  abiotic&#8208~"abiotic",
    kw_original == "abundant centre model"~"abundant center model",
    kw_original == "barley and cereal yellow dwarf virus"~"barley and cereal yellow dwarf viruses",
    kw_original == "acer opalus ssp granatense"~"acer opalus",
    kw_original == "acer opalus subsp granatense"~"acer opalus",
    kw_original == "australian monsoon tropics"~"australian monsoonal tropics",
    kw_original == "adaptation and trade-off"~"adaptations and trade-offs",
    kw_original == "biodiversity and ecosystem functioning"~"biodiversity and ecosystem function",
    kw_original == "alternative stable state"~"alternate stable state",
    kw_original == "anas plathyrynchos"~"anas platyrhynchos",
    kw_original == "asymmetric competition"~"asymmetrical competition",
    kw_original == "binomial mixture model"~"binomial n-mixture model",
    kw_original == "barley yellow dwarf virus (bydv)"~"barley and cereal yellow dwarf viruses",
    kw_original == "barley yellow dwarf viruses (bydvs)"~"barley and cereal yellow dwarf viruses",
    kw_original == "arbuscular mycorrhiza"~"arbuscular myccorrhyiza",
    kw_original == "aguoti paca"~"agouti paca",
    kw_original == "anti-predator behavior"~"antipredatory behavior",
    kw_original == "below-ground process"~"below-ground processes",
    kw_original == "behavioural tradeoff"~"behavioral trade-off",
    kw_original == "anti-plant"~"ant-plant",
    kw_original == "bayesian hierarchical modeling"~"bayesian hierarchical model",
    kw_original == "behavior genetics"~"behavioral genetics",
    kw_original == "alternate states"~"alternative states",
    kw_original == "arciidae"~"arctiidae",
    kw_original == "area-concentrated search"~"area-concentrated searching",
    kw_original == "behavioral changes"~"behavioral change",
    kw_original == "behavioural change"~"behavioral change",
    kw_original == "age-specific breeding probabilities"~"age-specific breeding probability",
    kw_original == "alternative reproductive strategies"~"alternative reproductive strategy",
    kw_original == "anadromous fish"~"anadromous fishes",
    kw_original == "bialowieza forest"~"biaowiea forest",
    kw_original == "above and belowground herbivores"~"above- and belowground herbivory",
    kw_original == "alternate prey"~"alternative prey",
    kw_original == "arciidae"~"ariidae",
    kw_original == "anthropogenic stress"~"anthropogenic stressors",
    kw_original == "biogeochemical model"~"biogeochemical modeling",
    # kw_original == "ant assemblages"~"bat assemblages",
    # kw_original == "ant pollination"~"bat pollination",
    # kw_original == "arboreal ants"~"arboreal plants",
    kw_original == "allogenic ecosystem engineers"~"autogenic ecosystem engineers",
    kw_original == "basic reproductive number"~"r0",
  kw_original == "r-o"~"r0",
    kw_original == "alternative mating strategies"~"alternative mating strategy",
    kw_original == "above-ground competition"~"above-ground competition cue",
    kw_original == "agelaia"~"aglaia",
    kw_original == "agro-ecosystem"~"agroecosystems",
    kw_original == "akaike information criterion"~"akaikes information criteria",
    # kw_original == "annual grass"~"annual grasses",
    kw_original == "baetids"~"baetis",
    # kw_original == "bioenergetic model"~"bioenergetic modeling",
    kw_original == "biodiversity and ecosystem function (bef)"~"biodiversity and ecosystem function",
    kw_original == "altitudinal migrant"~"altitudinal migration",
    kw_original == "centre-periphery hypothesis"~"center-periphery hypothesis",
    kw_original == "borrelia burdgorferi"~"borrelia burgdorferi",
    kw_original == "blue-green aglae"~"blue-green algae",
    kw_original == "coastal temperate rain forests"~"coastal temperate rainforest",
    kw_original == "c4 grassland"~"c4 grasslands",
    kw_original == "brachyramphus marmoratus"~"brachyramphus marmotus",
    kw_original == "coadapted gene complex"~"coadapted gene complexes",
    kw_original == "coffee agro-ecosystems"~"coffee agroecosystem",
    kw_original == "comparative approach"~"comparative approaches",
    kw_original == "capreolus capreolus"~"capreolus capreolus l",
    kw_original == "cedar creek natural history area, minnesota"~"cedar creek natural history area, minnesota, usa",
    kw_original == "branching process"~"branching processes",
    kw_original == "coffee agroforest"~"coffee agroforestry",
    kw_original == "bloflies"~"blowflies",
    kw_original == "carry-over effects"~"carryover effect",
    kw_original == "colombian amazonia"~"colombian amazon",
    kw_original == "complementary resource use"~"complementary resources",
    kw_original == "catastrophic regime shifts"~"catastrophic regime-shift",
    kw_original == "coefficient of additive genetic variance"~"coefficient of additive genetic variation",
    kw_original == "coevolutionary arm races"~"coevolutionary arms race",
    kw_original == "chasmagnathus granulata"~"chasmagnathus granulatus",
    kw_original == "bodega marine reserve, california"~"bodega marine reserve, california, usa",
    kw_original == "body size distribution"~"brood size distribution",
    kw_original == "coastal marsh"~"coastal marshes",
    kw_original == "community assembly by trait selection, cats"~"community assembly by trait selection",
    kw_original == "breeding age"~"breeding range",
    kw_original == "caesium"~"cesium",
    kw_original == "community function"~"community functioning",
    kw_original == "california floristic province, usa"~"california floristic province, usa",
    kw_original == "chihuahuan desert, new mexico"~"chihuahuan desert, new mexico, usa",
    kw_original == "compensatory growth"~"compensatory regrowth",
    kw_original == "climate-growth relation"~"climate-growth relationship",
    kw_original == "community dynamic model"~"community dynamics modeling",
    kw_original == "competition for pollination"~"competition for pollinators",
    # kw_original == "GxE interactions(s)"~"GxE interaction(s)",
    kw_original == "GxE interactions(s)"~"GxE interactions",
    kw_original == "top-down vs. bottom-up control"~"bottom-up vs top-down control",
    kw_original == "50-ha forest dynamics plot"~"50-ha plot",
    kw_original == "<bold>g</bold>-matrix"~"g-matrix",
    kw_original == "acacia species"~"acacia",
    kw_original == "africa, bat reproduction"~"african bat reproduction",
    # kw_original == "age-specific reproduction"~"age-specific reproduction/survival",
    # kw_original == "age-specific reproduction and survival"~"age-specific reproduction/survival",
    # kw_original == "age-specific reproductive success"~"age-specific reproduction/survival",
    # kw_original == "age-specific survival"~"age-specific reproduction/survival",
    kw_original == "aboveground annual net primary productivity"~"anpp",
    kw_original == "above ground net primary productivity (anpp)"~"anpp",
    kw_original == "aboveground net primary production (anpp)"~"anpp",
    kw_original == "age-dependent mortality demography"~"age-dependent mortality",
    kw_original == "age-from-stage modeling"~"age-from-stage models",
    kw_original == "abandoned agricultural lands"~"abandoned agriculture",
    kw_original == "abandoned cattle pastures"~"abandoned pastures",
    kw_original == "abandoned pasture"~"abandoned pastures",
    kw_original == "abandoned farmland"~"abandoned fields",
    # kw_original == "16s rdna"~"16s rdna/rrna",
  # kw_original == "16s rdna"~"16s rdna/rrna",
    # kw_original == "16s"~"16s rdna/rrna",
    # kw_original == "16s rdna sequencing"~"16s rdna/rrna",
  kw_original == "16s rdna sequencing"~"16s rdna",
    kw_original == "coevoltion"~"coevolution",
    # kw_original == "16s rrna"~"16s rdna/rrna",
    # kw_original == "16s rrna genes"~"16s rdna/rrna",
  kw_original == "16s rrna genes"~"16s rrna",
    kw_original == "16s-rrna and its gene sequencing"~"16s rrna",
  # kw_original == "16s-rrna and its gene sequencing"~"16s rdna/rrna",
    kw_original ==  "smithsonian forestgeo"~"bci",
    # kw_original ==  "smithsonian tropical research institute"~"bci",
  kw_original ==  "smithsonian tropical research institute"~"stri",
    # kw_original ==  "smithsonian tropical research institute, gamboa, panama"~"bci",
  kw_original ==  "smithsonian tropical research institute, gamboa, panama"~"stri, panama",
    kw_original ==  "site dependence"~"site-dependence",
    # kw_original ==  ", usa"~"",
   TRUE ~ as.character(kw_original))) 
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
  keywords<-keywords %>% 
    rename(kw_final=kw_refined_manual) %>% 
mutate(kw_final=gsub("sensu stricto","sensustricto",kw_final)) %>%  # Only for purposes of searching...it's two words!
  mutate(kw_final=gsub("grasslands.","grasslands.",kw_final)) %>%  # Only for purposes of searching...it's two words!
  mutate(kw_final=gsub("rain forest","rainforest",kw_final)) %>%  # Only for purposes of searching...it's two words!
  mutate(kw_final=gsub("rain-forest","rainforest",kw_final)) %>%  # Only for purposes of searching...it's two words!
  mutate(kw_final=gsub("forests","forest",kw_final)) %>% 
  mutate(kw_final=gsub("savannas","savanna",kw_final)) %>% 
  mutate(kw_final=gsub("grasslands","grassland",kw_final)) %>% 
  mutate(kw_final=gsub("savannah","savanna",kw_final)) %>% 
  mutate(kw_final=gsub("savannahs","savanna",kw_final)) %>% 
  # mutate(kw_final=gsub("(usa) ","",kw_final)) %>% 
  mutate(kw_final=gsub("reefs","reef",kw_final)) %>% 
  mutate(kw_final=gsub("reefcape","reefscape",kw_final)) %>% # this corrects back the one messed up by the prior correction
  mutate(kw_final=gsub("systems","system",kw_final)) %>% 
  mutate(kw_final = case_when(
    (str_detect(kw_original,"sloss") == TRUE) ~ "sloss",
    (str_detect(kw_original,"la selva") == TRUE) ~ "la selva",
    (str_detect(kw_original,"organization for tropical studies") == TRUE) ~ "ots",
    (str_detect(kw_original,"las cruces") == TRUE) ~ "las cruces",
    (str_detect(kw_original,"palo verde") == TRUE) ~ "palo verde",
    (str_detect(kw_original,"guanacaste") == TRUE) ~ "guanacaste",
    (str_detect(kw_original,"manu national") == TRUE) ~ "manu",
    (str_detect(kw_original,"bci") == TRUE) ~ "bci",
    (str_detect(kw_original,"republic of panama") == TRUE) ~ "panama",
    (str_detect(kw_original,"cocha cashu") == TRUE) ~ "cocha cashu",
    (str_detect(kw_original,"amazonian ecuador") == TRUE) ~ "ecuadorian amazon",
    (str_detect(kw_original,"biological dynamics of forest fragments") == TRUE) ~ "bdffp",
    kw_final == "rainforest (christman island"~"rainforest (christman island)",
    kw_final == "rainforest (christman island"~"rainforest, christman island",
    # kw_final == "longleaf pine savanna, southeastern usa"~"longleaf pine savanna",
    kw_final == "ots"~"ots-oet",
    kw_final == "ots"~"ots-oet",
    kw_final == "c grassland composition 4"~" c4 grassland composition",
    kw_final == "c grassland composition 3"~" c3 grassland composition",
    kw_final == "life table response experiment, ltre"~"ltre",
    kw_final == "ltre analysis"~"ltre",
    kw_final == "δ n 15"~"delta n15",
    kw_final == "δ c 13"~"delta c13",
    kw_final == "β-diversity"~"beta diversity",
    kw_final == "zostera-marina"~"zostera marina",
    kw_final == "zostera-capricorni"~"zostera capricorni",
    kw_final == "zooplancton"~"zooplankton",
    kw_final == "zooplancton"~"zooplankton",
    kw_final == "alaska (usa)"~"alaska",
    kw_final == "alaska (usa)"~"usa",
    kw_final == "akaikes information criteria"~"aic",
    kw_final == "akaike's information criterion (aic)"~"aic",
    kw_final == "akaike's information criterion"~"aic",
    kw_final == "akaike's information criteria"~"aic",
    kw_final == "akaike weights"~"aic",
    kw_final == "akaike information criterion (aic)"~"aic",
    kw_final == "akaike information criteria"~"aic",
    kw_final == "akaike criterion"~"aic",
    kw_final == "akaike"~"aic",
    # kw_final == "manu national park, peru"~"manu national park",
    kw_final == "manu national park (peru)"~"manu national park, peru",
    # kw_final == "manu national park (peru)"~"manu national park",
    # kw_final == "yasuni national park, amazonian ecuador"~"yasuni national park",
    # kw_final == "yasuni ecological research station, ecuador"~"yasuni ecological research station",
    kw_final == "acer-saccharum"~"acer saccharum",
    kw_final == "acer-rubrum"~"acer rubrum",
    kw_final == "acer-negundo"~"acer negundo",
    kw_final == "agro-forest system"~"agroforest system",
    # kw_final == "tropical forest"~"tropical forest(s)",
    kw_final == "aboveground‚Äìbelowground interactions"~"above- and belowground interactions",
    kw_final == "aboveground‚Äìbelowground interactions"~"above- and belowground interactions",
    TRUE ~ as.character(kw_final))) %>% 
  mutate(kw_final=gsub("behaviour","behavior",kw_final)) %>% 
  # mutate(kw_final=gsub("behavioural","reef",kw_final)) %>% 
  mutate(kw_final=gsub("colour","color",kw_final)) %>% 
  # mutate(kw_final=gsub("colouration","reef",kw_final)) %>% 
  mutate(kw_final=gsub("harbour","harbor",kw_final)) %>% 
  mutate(kw_final=gsub("adultplants","adult plant",kw_final)) %>% 
  mutate(kw_final=gsub("insectivoresinsectivory","insectivores/insectivory",kw_final)) %>% 
  mutate(kw_final=gsub("densitydependence","density dependence",kw_final)) %>% 
  mutate(kw_final=gsub("moult","molt",kw_final)) %>% 
  mutate(kw_final=gsub("neighbour","neighbor",kw_final)) %>% 
  # mutate(kw_final=gsub("neighbourhood","reef",kw_final)) %>% 
  mutate(kw_final=gsub("signalling","signaling",kw_final)) %>% 
  mutate(kw_final=gsub("modelling","modeling",kw_final)) %>% 
  mutate(kw_final=gsub("ageing","aging",kw_final)) %>% 
  mutate(kw_final=gsub("'","",kw_final)) %>% 
  mutate(kw_final=gsub("“","",kw_final)) %>% 
  mutate(kw_final=gsub("”","",kw_final)) %>% 
  mutate(kw_final=gsub("‘","",kw_final)) %>% 
  mutate(kw_final=gsub("’","",kw_final)) %>%
  mutate(kw_final=gsub("“","",kw_final)) %>% 
  mutate(kw_final=gsub("’","",kw_final)) %>%
  # mutate(kw_final=gsub(", usa"," (usa)",kw_final)) %>%
  mutate(kw_final=gsub("defence","defense",kw_final)) %>% 
  filter(!str_detect(kw_final, 'funding was provided by grants from the academy')) %>% 
  filter(!str_detect(kw_final, 'este trabajo es parte del trabajo doctoral de la autora')) %>% 
  filter(!str_detect(kw_final, 'atom percent excess')) %>% 
  filter(!str_detect(kw_final, 'fruit census fruit trap collection')) # need to fix this one

  keywords<-keywords %>% 
  mutate(kw_final=trimws(kw_final)) %>% 
  mutate(kw_final=tolower(kw_final)) %>% 
  mutate(kw_final=gsub("sensu stricto","sensustricto",kw_final)) %>%  # Only for purposes of searching...it's two words!
  mutate(kw_final=gsub("grasslands.","grasslands.",kw_final)) %>%  # Only for purposes of searching...it's two words!
  mutate(kw_final=gsub("rain forest","rainforest",kw_final)) %>%  # Only for purposes of searching...it's two words!
  mutate(kw_final=gsub("rain-forest","rainforest",kw_final)) %>%  # Only for purposes of searching...it's two words!
  mutate(kw_final=gsub("forests","forest",kw_final)) %>% 
  mutate(kw_final=gsub("savannas","savanna",kw_final)) %>% 
  mutate(kw_final=gsub("grasslands","grassland",kw_final)) %>% 
  mutate(kw_final=gsub("savannah","savanna",kw_final)) %>% 
  mutate(kw_final=gsub("savannahs","savanna",kw_final)) %>% 
  # mutate(kw_final=gsub("(usa) ","",kw_final)) %>% 
  mutate(kw_final=gsub("reefs","reef",kw_final)) %>% 
  mutate(kw_final=gsub("reefcape","reefscape",kw_final)) %>% # this corrects back the one messed up by the prior correction
  mutate(kw_final=gsub("systems","system",kw_final)) %>% 
  mutate(kw_final = case_when(
    (str_detect(kw_original,"sloss") == TRUE) ~ "sloss",
    (str_detect(kw_original,"la selva") == TRUE) ~ "la selva",
    (str_detect(kw_original,"organization for tropical studies") == TRUE) ~ "ots",
    (str_detect(kw_original,"las cruces") == TRUE) ~ "las cruces",
    (str_detect(kw_original,"palo verde") == TRUE) ~ "palo verde",
    (str_detect(kw_original,"guanacaste") == TRUE) ~ "guanacaste",
    (str_detect(kw_original,"manu national") == TRUE) ~ "manu",
    (str_detect(kw_original,"bci") == TRUE) ~ "bci",
    (str_detect(kw_original,"republic of panama") == TRUE) ~ "panama",
    (str_detect(kw_original,"cocha cashu") == TRUE) ~ "cocha cashu",
    (str_detect(kw_original,"amazonian ecuador") == TRUE) ~ "ecuadorian amazon",
    (str_detect(kw_original,"biological dynamics of forest fragments") == TRUE) ~ "bdffp",
    kw_final == "rainforest (christman island"~"rainforest (christman island)",
    kw_final == "rainforest (christman island"~"rainforest, christman island",
    # kw_final == "longleaf pine savanna, southeastern usa"~"longleaf pine savanna",
    kw_final == "ots"~"ots-oet",
    kw_final == "ots"~"ots-oet",
    kw_final == "c grassland composition 4"~" c4 grassland composition",
    kw_final == "c grassland composition 3"~" c3 grassland composition",
    kw_final == "life table response experiment, ltre"~"ltre",
    kw_final == "ltre analysis"~"ltre",
    kw_final == "δ n 15"~"delta n15",
    kw_final == "δ c 13"~"delta c13",
    kw_final == "β-diversity"~"beta diversity",
    kw_final == "zostera-marina"~"zostera marina",
    kw_final == "zostera-capricorni"~"zostera capricorni",
    kw_final == "zooplancton"~"zooplankton",
    kw_final == "zooplancton"~"zooplankton",
    kw_final == "alaska (usa)"~"alaska",
    kw_final == "alaska (usa)"~"usa",
    kw_final == "akaikes information criteria"~"aic",
    kw_final == "akaike's information criterion (aic)"~"aic",
    kw_final == "akaike's information criterion"~"aic",
    kw_final == "akaike's information criteria"~"aic",
    kw_final == "akaike weights"~"aic",
    kw_final == "akaike information criterion (aic)"~"aic",
    kw_final == "akaike information criteria"~"aic",
    kw_final == "akaike criterion"~"aic",
    kw_final == "akaike"~"aic",
    # kw_final == "manu national park, peru"~"manu national park",
    kw_final == "manu national park (peru)"~"manu national park, peru",
    # kw_final == "manu national park (peru)"~"manu national park",
    # kw_final == "yasuni national park, amazonian ecuador"~"yasuni national park",
    # kw_final == "yasuni ecological research station, ecuador"~"yasuni ecological research station",
    kw_final == "acer-saccharum"~"acer saccharum",
    kw_final == "acer-rubrum"~"acer rubrum",
    kw_final == "acer-negundo"~"acer negundo",
    kw_final == "agro-forest system"~"agroforest system",
    kw_final == "tropical forest"~"tropical forest(s)",
    kw_final == "aboveground‚Äìbelowground interactions"~"above- and belowground interactions",
    kw_final == "aboveground‚Äìbelowground interactions"~"above- and belowground interactions",
    TRUE ~ as.character(kw_final))) %>% 
  mutate(kw_final=gsub("behaviour","behavior",kw_final)) %>% 
  # mutate(kw_final=gsub("behavioural","reef",kw_final)) %>% 
  mutate(kw_final=gsub("colour","color",kw_final)) %>% 
  # mutate(kw_final=gsub("colouration","reef",kw_final)) %>% 
  mutate(kw_final=gsub("harbour","harbor",kw_final)) %>% 
  mutate(kw_final=gsub("adultplants","adult plant",kw_final)) %>% 
  mutate(kw_final=gsub("insectivoresinsectivory","insectivores/insectivory",kw_final)) %>% 
  mutate(kw_final=gsub("densitydependence","density dependence",kw_final)) %>% 
  mutate(kw_final=gsub("moult","molt",kw_final)) %>% 
  mutate(kw_final=gsub("neighbour","neighbor",kw_final)) %>% 
  # mutate(kw_final=gsub("neighbourhood","reef",kw_final)) %>% 
  mutate(kw_final=gsub("signalling","signaling",kw_final)) %>% 
  mutate(kw_final=gsub("modelling","modeling",kw_final)) %>% 
  mutate(kw_final=gsub("ageing","aging",kw_final)) %>% 
  mutate(kw_final=gsub("'","",kw_final)) %>% 
  mutate(kw_final=gsub("“","",kw_final)) %>% 
  mutate(kw_final=gsub("”","",kw_final)) %>% 
  mutate(kw_final=gsub("‘","",kw_final)) %>% 
  mutate(kw_final=gsub("’","",kw_final)) %>%
  mutate(kw_final=gsub("“","",kw_final)) %>% 
  mutate(kw_final=gsub("’","",kw_final)) %>%
  # mutate(kw_final=gsub(", usa"," (usa)",kw_final)) %>%
  mutate(kw_final=gsub("defence","defense",kw_final)) %>% 
    filter(!str_detect(kw_final, 'funding was provided by grants from the academy')) %>% 
  filter(!str_detect(kw_final, 'este trabajo es parte del trabajo doctoral de la autora')) %>% 
  filter(!str_detect(kw_final, 'atom percent excess')) %>% 
    filter(!str_detect(kw_final, 'fruit census fruit trap collection')) # need to fix this one

# keywords_clean<-keywords_clean %>% 
#   mutate(kw_final = case_when(
#   (str_detect(kw_final,", (usa)") == TRUE) ~ ", usa",
#   TRUE ~ as.character(kw_final)))
#   mutate(kw_final=gsub("//(uk)",", uk",kw_final)) 
# 
# keywords_clean<-keywords_clean %>% 
#   separate(kw_final,c("kw_final2","kw_final3"),sep=",",remove=FALSE,extra="warn") %>% 
#   separate(kw_final3,c("kw_final4","kw_final5"),sep="\\(",remove=FALSE,extra="warn")
keywords_clean_summary<-keywords %>% 
  as_tibble() %>% 
  group_by(kw_final) %>% 
  summarize(n=n())
keywords_clean_summary



library(hunspell)

## find the misspelled words
# foo<-kw_final_counts_split %>%  select(kw_original) %>% slice(1:3000)
bad.words <- hunspell(keywords_clean_summary$kw_final)
bad.words <- unique(unlist(bad.words))
sugg.words <- hunspell_suggest(bad.words)
sugg.words <- unlist(lapply(sugg.words, function(x) x[1]))
word.list <- as.data.frame(cbind(bad.words, sugg.words))

freq.word <- count(foo, kw_original)
names(freq.word)
names(word.list)
freq.word <- inner_join(freq.word, word.list, by = c(kw_original = "bad.words")) %>% mutate(sugg.words=tolower(sugg.words))
freq.word <- freq.word %>% distinct(sugg.words,kw_original) %>% mutate(unique=(sugg.words==kw_original)) %>% filter(unique==FALSE)
freq.word<- freq.word %>% 
  mutate(sugg.words=gsub(" ","",sugg.words))  %>% 
  mutate(unique=(sugg.words==kw_original)) %>% 
  filter(unique==FALSE) %>% 
  mutate(sugg.words=gsub(" ","",sugg.words))  %>% 
  mutate(unique=(sugg.words==kw_original)) %>% 
  filter(unique==FALSE) 


# keywords_clean$kw_final<-str_replace(keywords_clean$kw_final,"species\\) diversity/biodiversity", "species diversity")
# keywords_clean$kw_final<-str_replace(keywords_clean$kw_final,"[:punct:]", "")
# keywords_clean$kw_final<-str_replace(keywords_clean$kw_final,"\\)", "")
# keywords_clean$kw_final<-str_replace(keywords_clean$kw_final,"\\(", "")
# keywords_clean$kw_final<-str_replace(keywords_clean$kw_final,"\\,", "")


keywords_clean_summary<-keywords_clean %>% 
  as_tibble() %>% 
  group_by(kw_final) %>% 
  summarize(n=n()) %>% 
  arrange(desc(n))
keywords_clean_summary




library(countrycode)
keywords_clean_summary
# to avoid countries being split/be consistent re names 
# you can convert all to iso code 

# kw100<-keywords_clean_summary 
kw100<-keywords_clean_summary %>% slice(1:5000)
kw100$code<-countrycode(kw100$kw_final, origin = 'country.name', destination = 'iso3c')
kw_country<-kw100 %>% 
  filter(code!=FALSE) %>% 
  group_by(kw_final,code) %>% 
  mutate(code = case_when(
    (code ==  "CIV" & kw_final!= "ivory coast") ~ as.character(NA),
    (code ==  "AIA" & kw_final!= "anguilla") ~ as.character(NA),
    (code ==  "ARG" & kw_final!= "argentina") ~ as.character(NA),
    (code ==  "COD" & (kw_final!="dr congo"|kw_final!= "katanga dem. rep. congo")) ~ as.character(NA),
    (code ==  "AIA" & kw_final!= "anguilla") ~ as.character(NA),
    (code ==  "BRA" & str_detect(kw_final, 'brazilian')==TRUE) ~ as.character(NA),
    (code ==  "AUS" & str_detect(kw_final, 'autstralian')==TRUE) ~ as.character(NA),
    kw_final== "asclepias syriaca" ~ as.character(NA),
    kw_final== "antiguastrea antingua" ~ as.character(NA),
    TRUE ~ as.character(code)))
# delete "western "
# delete "eastern "
# delete "northern "
# delete "southern "
kw100$code<-countrycode(kw100$kw_final, origin = 'country.name', destination = 'iso3c')

keywords_clean$code<-countrycode(keywords_clean$kw_final, origin = 'country.name', destination = 'iso3c')
keywords_clean <- keywords_clean %>% 
  mutate(code = case_when(
    (code ==  "CIV" & kw_final!= "ivory coast") ~ as.character(NA),
    (code ==  "AIA" & kw_final!= "anguilla") ~ as.character(NA),
    (code ==  "ARG" & kw_final!= "argentina") ~ as.character(NA),
    (code ==  "COD" & (kw_final!="dr congo"|kw_final!= "katanga dem. rep. congo")) ~ as.character(NA),
    (code ==  "AIA" & kw_final!= "anguilla") ~ as.character(NA),
    (code ==  "BRA" & str_detect(kw_final, 'brazilian')==TRUE) ~ as.character(NA),
    (code ==  "AUS" & str_detect(kw_final, 'australian')==TRUE) ~ as.character(NA),
    kw_final== "asclepias syriaca" ~ as.character(NA),
    kw_final== "antiguastrea antingua" ~ as.character(NA),
    TRUE ~ as.character(code)))


anguilla bengalensis
anguilla-anguilla
anguilla-rostrata
argentina anserina
antiguastrea
st. lawrence river québec canada
williams lake british columbia canada
cocos nucifera
swiss long-term forest research programme lwf
swiss stone pine
austrocedrus chilensis
aristotelia chilensis
chilesius camposi
(usa)-> , usa
la selva, costa rica
carpobrotus chilensis
bathygobius cocosensis
austrian
dracocephalum austriacum
dahomey gap
bahamas mosquitofish
bolivian
ecuadorian
brazil nut
brazil-nuts
leishmania braziliensis
yukon canada->yukon,canada
boreal forest (yukon, canada)
canada jays
canada lynx
canada warbler
grassland.national park (canada)
killarney provincial park ontario canada


keywords_clean_summary<-keywords_clean %>% 
  as_tibble() %>% 
  group_by(code,kw_final) %>% 
  summarize(n=n()) %>% 
  arrange(desc(n)) %>% 
  filter(is.na(code)==FALSE)
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


library(udpipe)
x<-keywords_clean_summary %>% 
  select(kw_final) %>% 
  mutate(id=row_number()) %>% 
  slice(1:1000) %>% 
  pull(kw_final)
# x<-keywords_clean_summary %>% select(kw_final)
# x<-unlist(x)
anno <- udpipe(x, "english")
anno$unique<-(anno$token==anno$lemma)
summary(anno$unique)
lemmas<-anno[, c("doc_id", "sentence_id", "token", "lemma", "upos","unique")]
lemmas<-lemmas %>% arrange(unique)
lemmas

false_checks<-lemmas %>% 
  filter(unique==FALSE) %>% 
  group_by(token,lemma) %>% 
  tally() %>% 
  mutate(diff=(nchar(token)-nchar(lemma))) %>% 
  arrange(desc(diff))



# kw comb freq stats: https://bnosac.github.io/udpipe/docs/doc5.html
library(udpipe)
x<-keywords_clean_summary %>% 
  select(kw_final) %>% 
  mutate(id=row_number())
# %>% 
#   slice(1:10000) 
# data(brussels_reviews)
# comments <- subset(brussels_reviews, language %in% "es")
# ud_model <- udpipe_download_model(language = "spanish")
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)
x <- udpipe_annotate(ud_model, x = x$kw_final, doc_id = x$id)
x <- as.data.frame(x)
stats <- keywords_rake(x = x, term = "lemma", group = "doc_id", 
                       relevant = x$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
library(lattice)
barchart(key ~ rake, data = head(subset(stats, freq > 3), 20), col = "cadetblue", 
         main = "Keywords identified by RAKE", 
         xlab = "Rake")


# most ocurring noun
stats <- subset(x, upos %in% c("NOUN")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most occurring nouns", xlab = "Freq")

## most ocurring ADJECTIVES
stats <- subset(x, upos %in% c("ADJ")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most occurring adjectives", xlab = "Freq")

# Nouns / adjectives used in same sentence
cooc <- cooccurrence(x = subset(x, upos %in% c("NOUN", "ADJ")), 
                     term = "lemma", 
                     # group = c("doc_id", "paragraph_id", "sentence_id"))
                     group = c("doc_id"))
head(cooc,20)


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
keywords_clean<-keywords_clean %>% as_tibble()
keywords_clean$kw_final<-str_trim(keywords_clean$kw_final) 
kw_final_counts<-keywords_clean %>% 
  mutate_all(str_trim) %>% 
  select(kw_final) %>% 
  mutate(kw_final=trimws(kw_final)) %>% 
  group_by(kw_final) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  mutate(kw_final=gsub("-"," ",kw_final)) %>% 
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
  mutate(kw_original=gsub("\n"," ",kw_original)) %>% 
  # mutate(kw_original=str_replace('\\n"', '')) %>% 
  mutate(kw_original=tolower(kw_original)) %>% 
  mutate(kw_original=trimws(kw_original)) 

kw_final_counts$kw_original<-str_replace(kw_final_counts$kw_original,"\\,", "")
# library(textclean)
# kw_final_counts<-kw_final_counts %>% 
#   replace_non_ascii(kw_original, replacement = "", remove.nonconverted = TRUE)

kw_final_counts_40<-kw_final_counts %>%
  group_by(kw_original) %>% 
  summarize(n=n()) %>%
  arrange(desc(n)) %>% 
  filter(n>40)

# kw_final_counts$last_letter<-str_sub(kw_final_counts$kw_original,-1,-1)
# kw_final_counts_pl <- kw_final_counts %>% filter(last_letter=="s")
# kw_final_counts<-kw_final_counts %>% arrange(kw_original,desc(n))


library(SnowballC)
prof.tm <- mutate(kw_final_counts, word.stem = wordStem(kw_original, language = "en"))
prof.tm <- prof.tm %>% 
  mutate(stem_chk=kw_original==word.stem) %>% 
  filter(stem_chk==FALSE)



library(RecordLinkage)
prof.tm$word_sim<-levenshteinSim(prof.tm$kw_original, prof.tm$word.stem)
prof.tm<-prof.tm %>% arrange(desc(word_sim)) %>% select(-stem_chk)
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
freq.word <- inner_join(freq.word, word.list, by = c(kw_original = "bad.words")) %>% mutate(sugg.words=tolower(sugg.words))
freq.word <- freq.word %>% distinct(sugg.words,kw_original) %>% mutate(unique=(sugg.words==kw_original)) %>% filter(unique==FALSE)
freq.word<- freq.word %>% 
  mutate(sugg.words=gsub(" ","",sugg.words))  %>% 
  mutate(unique=(sugg.words==kw_original)) %>% 
  filter(unique==FALSE) %>% 
  mutate(sugg.words=gsub(" ","",sugg.words))  %>% 
  mutate(unique=(sugg.words==kw_original)) %>% 
  filter(unique==FALSE) 

library(RecordLinkage)
freq.word$word_sim<-levenshteinSim(freq.word$kw_original, freq.word$sugg.words)
freq.word<-freq.word %>% arrange(desc(word_sim))
write_csv(freq.word,"./bibliometrics/data_intermediate/freq.word.csv")

# library("SemNetCleaner")
foo<-kw_final_counts_split %>%  select(kw_original) %>% slice(1:3000)
clean <- textcleaner(data = foo, miss = NA,
                     partBY = "row", dictionary = "general")


kw_final_counts_split<- kw_final_counts %>% group_by(kw_original) %>% summarize(n=n()) %>% arrange(desc(n))


clean <- textcleaner(data = open.animals[,-c(1:2)], miss = 99,
                     partBY = "row", dictionary = "animals")

kw_final_counts %>%
  slice(1:300) %>% 
  mutate(kw_final_split = stringr::str_extract_all(kw_final_counts$kw_final,
                                         "(trait)|(forest)|(mangrove)")) %>%
  tidyr::unnest(cols = c(focal_words))

fruits <- c("trait", "forest", "mangrove")

kw_final_counts$kw_final2 <- sapply(stringr::str_extract_all(kw_final_counts$kw_final,
                                             sprintf('(%s)', paste0(fruits, collapse = '|'))), toString)

rows1<-floor(nrow(kw_final_counts)/3)
rows2<-rows1*2
rows3<-rows1*3
cola<-kw_final_counts %>% slice(1:rows1)
colb<-kw_final_counts %>% slice((rows1+1):(rows2))
colc<-kw_final_counts %>% slice((rows2+1):(rows3))
kw_final_counts<-bind_cols(cola,colb,colc)
write_csv(kw_final_counts,"./bibliometrics/data_intermediate/kw_final_counts.csv")
# rm(keywords_unique)



keywords_unique<-keywords %>% select(kw_final) %>% unique() %>% arrange(kw_final)


write_csv(keywords_unique,"./bibliometrics/data_intermediate/keywords_unique.csv")
# rm(keywords_unique)



kw_similarity<-Name.check(keywords_unique)
Name.check <- function(DataToClean) {
  CHECKFILE<-DataToClean
NamesList<-sapply(CHECKFILE$kw_final,agrep,CHECKFILE$kw_final, value=TRUE) 
NamesDF<-data.frame(
  Name1 = rep(names(NamesList), lapply(NamesList, length)),
  Name2 = unlist(NamesList))

# summary(NamesDF)
# str(NamesDF)

# Create a column to which you will add a logical condition telling you if the names are an EXACT match
NamesDF$match<-NA
NamesDF$match<-NamesDF$Name1==NamesDF$Name2
# match2<-ifelse(NamesDF$match=="TRUE",1,0) #convert TRUE/FALSEto 0/1
# NamesDF<-cbind(NamesDF,match2) 
# head(NamesDF,40)
# str(NamesDF)
NamesDF<-arrange(NamesDF,Name1,Name2) #organize in alphabetica order
NamesDF<-filter(NamesDF, match==FALSE)  # THIS DELETES ALL NAMES THAT ARE 100% MATCH 
head(NamesDF)
# Convert to chr
NamesDF$Name1<-as.character(NamesDF$Name1)
NamesDF$Name2<-as.character(NamesDF$Name2)
str(NamesDF)

# Calclulate the proportional similarity and # changes required to go from one name to another. Package RecordLinkage
NamesDF$Name_sim<-levenshteinSim(NamesDF$Name1, NamesDF$Name2)
NamesDF$Name_dist<-levenshteinDist(NamesDF$Name1, NamesDF$Name2)

# Because this does all pairwise comparisons, it results in redundancy: "e bruna vs emilio bruna" and "emilio bruna vs e bruna"
# are in different rows, even though they are the same "comparison". This deletes one of the two 
NamesDF<-NamesDF[!duplicated(t(apply(NamesDF, 1, sort))),]
# this arranges them in order from most similar (1 change required) to least similar.
# look carefully at those with a few changes, as they are likely to be a tiny spelling mistake or difference in intials


NamesDF$index<-seq.int(nrow(NamesDF)) #adds a column with an index to make it easier to id which row you need'
NamesDF <- NamesDF %>% select(index, Name1, Name2, Name_sim,Name_dist) #It's kinda ugly, but this rearranges columns (and dumps the "FALSE")
NamesDF <- arrange(NamesDF, desc(Name_sim))
# head(NamesDF)
write_csv(NamesDF, file="./bibliometrics/data_intermediate/kw_similarity.csv") #export it as a csv file


return(NamesDF)

}


keywords<-keywords %>% 
  # mutate(kw_final=gsub("agroforest","agroforest",kw_final)) %>% 
  mutate(article_cat = case_when(
    str_detect(kw_final,"tropics")==TRUE~"TRUE",
    str_detect(kw_final,"tropical")==TRUE~"TRUE",
    str_detect(kw_final,"bci")==TRUE~"TRUE", 
    str_detect(kw_final,"pasoh")==TRUE~"TRUE",
    str_detect(kw_final,"la selva")==TRUE~"TRUE",
    str_detect(kw_final,"ots-oet")==TRUE~"TRUE", 
    str_detect(kw_final,"bdffp")==TRUE~"TRUE", 
    str_detect(kw_final,"manu national park")==TRUE~"TRUE", 
    str_detect(kw_final,"cocha cashu")==TRUE~"TRUE",
    str_detect(kw_final,"amazon")==TRUE~"TRUE",
    str_detect(kw_final,"bci")==TRUE~"TRUE",
    # str_detect(kw_final,"tropic")==TRUE~"TRUE",
    str_detect(kw_final,"afrotrop")==TRUE~"TRUE",
    str_detect(kw_final,"rain forest")==TRUE~"TRUE",
    str_detect(kw_final,"dry forest")==TRUE~"TRUE",
    str_detect(kw_final,"la selva")==TRUE~"TRUE",
    str_detect(kw_final,"yasuni")==TRUE~"TRUE",
    TRUE ~ as.character("FALSE"))) %>% 
  arrange(desc(article_cat))

names(keywords)
forest_kw<-keywords_unique %>% 
  filter(str_detect(kw_final,"forest")==TRUE) 
  

# pooling keywords  -------------------------------------------------------


names(keywords)
forest_kw<-keywords_unique %>% 
  filter(str_detect(kw_final,"forest")==TRUE) 

rain_forest_kw<-forest_kw %>% 
  filter(
    (
      str_detect(kw_final,"wet") |
        str_detect(kw_final,"rain") |
        str_detect(kw_final,"african forest") |
        str_detect(kw_final,"rain")
         )
        ) %>% 
  filter(str_detect(kw_final,"forest drainage")==FALSE) %>% 
  filter(str_detect(kw_final,"forested wetlands")==FALSE) %>% 
  filter(str_detect(kw_final,"temperate rainforest")==FALSE) %>% 
  filter(str_detect(kw_final,"eucalyptus forest")==FALSE) %>% 
  filter(str_detect(kw_final,"subtropical")==FALSE) %>% 
  filter(str_detect(kw_final,"temperate")==FALSE) %>% 
  filter(str_detect(kw_final,"hemiepiphytes lianas pioneers tree growth tropical wet forest")==FALSE) %>% 
  filter(str_detect(kw_final,"litter litter nutrient content primary forest tropical rainforest")==FALSE) 



temp_rainforest_kw<-forest_kw %>% 
  filter(
    (
      str_detect(kw_final,"temperate rainforest") |
        str_detect(kw_final,"temperate rainforest")
        # str_detect(kw_final,"temperate") 
    )
  ) 


# includes temp rainforest
temp_forest_kw<-forest_kw %>% 
  filter(
    (
      str_detect(kw_final,"temperate") 
    )
  ) 


subtrop_forest_kw<-forest_kw %>% 
  filter(
    (
      str_detect(kw_final,"subtropical")
    )
  ) 

dry_forest_kw<-forest_kw %>% 
  filter(str_detect(kw_final,"dry")==TRUE)

boreal_forest_kw<-forest_kw %>% 
  filter(str_detect(kw_final,"boreal")==TRUE)
  

savanna_kw<-keywords_unique %>% 
  filter(str_detect(kw_final,"savanna")==TRUE) %>% 
filter(str_detect(kw_final,"autccology bolivia cerrado gramineae kranz anatomy neotropics savanna")==FALSE) %>% 
filter(str_detect(kw_final,"brazil fire fleshy fruit frugivory fruit shortage reproduction savanna vertebrates")==FALSE) %>% 
filter(str_detect(kw_final,"autccology bolivia cerrado gramineae kranz anatomy neotropics savanna")==FALSE) %>% 
filter(str_detect(kw_final,"facilitation, frailty model, hierarchical model, indirect effect")==FALSE) %>% 
  filter(str_detect(kw_final,"cerrado savanna")==FALSE) %>% 
  # filter(str_detect(kw_final,"brazilian savanna")==FALSE) %>% 
  # filter(str_detect(kw_final,"bolivian savanna")==FALSE) %>% 
  filter(str_detect(kw_final,"ivoire grass hyparrhenia diplandra. nitrogen use efficien")==FALSE) 
  
cerrado_kw<-keywords_unique %>% 
  filter(str_detect(kw_final,"cerrado")==TRUE) %>% 
  filter(str_detect(kw_final,"carvocaraceae cerrado")==FALSE) %>% 
  filter(str_detect(kw_final,"aconophora teligera ant araliaceae brazil cerrado didymo")==FALSE) %>% 
  filter(str_detect(kw_final,"amazonia-cerrado transition")==FALSE) %>% 
  filter(str_detect(kw_final,"autccology bolivia cerrado gramineae kranz anatomy neotropics savanna")==FALSE) 
  


grassland_kw<-keywords_unique %>% 
  filter(str_detect(kw_final,"grassland")==TRUE) 


usa_kw<-keywords_unique %>% 
  filter(str_detect(kw_final,"(usa)")==TRUE) %>% 
  # filter(str_detect(kw_final,"usa")==TRUE) %>% 
  # filter(str_detect(kw_final,"brazil fire fleshy fruit frugivory fruit shortage reproduction savanna vertebrates")==FALSE) %>% 
  # filter(str_detect(kw_final,"autccology bolivia cerrado gramineae kranz anatomy neotropics savanna")==FALSE) %>% 
  # filter(str_detect(kw_final,"facilitation, frailty model, hierarchical model, indirect effect")==FALSE) %>% 
  # filter(str_detect(kw_final,"cerrado savanna")==FALSE) %>% 
  # # filter(str_detect(kw_final,"brazilian savanna")==FALSE) %>% 
  # filter(str_detect(kw_final,"bolivian savanna")==FALSE) %>% 
  filter(str_detect(kw_final,"usa")==TRUE) 


# word association
# https://uc-r.github.io/word_relationships
txt_df<-grassland_kw %>% 
  mutate(line=nrow(grassland_kw)) %>% 
  relocate(line,.before=1) %>% 
  rename(text=kw_final)

foo<-txt_df %>% unnest_tokens(word, text)
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






















foo<-keywords %>% 
  group_by(article_cat, kw_final) %>% 
  summarize(n=n()) %>% 
  arrange(desc(n))

# https://cran.r-project.org/web/packages/stringdist/stringdist.pdf

library(stringdist)

parents_name <- keywords_unique %>% slice(1:200)
parents_name <- parents_name$kw_final

person_id <- 1:length(parents_name)

family_id <- vector("integer", length(parents_name))


#Looping through unassigned family ids
while(sum(family_id == 0) > 0){
  
  ids <- person_id[family_id == 0]
  
  dists <- stringdist(parents_name[family_id == 0][1], 
                      parents_name[family_id == 0], 
                      method = "lv")
  
  matches <- ids[dists <= 2]
  
  family_id[matches] <- max(family_id) + 1
}

result <- data.frame(person_id, parents_name, family_id)

result<-result %>% arrange(family_id) %>% group_by(family_id) %>% mutate(n=n()) %>% arrange(desc(n))
result

abiotic
biotic
abundance‚Äìrange-size relationship
acer rubrum, betula alleghaniensis, betula papyrifera
anagrus spp
anas spp.
anagrus spp.
an√°lise de is√≥topos est√°veis
alnus-crispa
alnus-rubra
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

kw_refined<-left_join(kw_refined,kw_refined2) %>% 
  select(-kw) %>% 
  filter(kw_refined!="18th&#8211") %>%
  filter(kw_refined!="&#160") %>% 
  filter(kw_refined!=  "&#8208") %>%
  filter(kw_refined!="&#8722") %>%
  filter(kw_refined!="&#946") %>%
  filter(kw_refined!="&#947") %>% 
  filter(kw_refined!="co&#8208") %>% 
  filter(kw_refined!="non&#8208") %>%
  filter(kw_refined!="wood&#8208") %>% 
  filter(kw_refined!="mixed&#8208")

final_kw<-read_csv("./bibliometrics/data_clean/final_kw.csv") %>% 
  rename(DI=DOI) %>% 
  rename(DE=kw_output) %>% 
  relocate(DE,.after=DI)

merged_refs<-read_csv("./bibliometrics/data_clean/merged_refs.csv") %>% 
  select(refID,jrnl_cat,SO,PY,DI)

final_kw<-left_join(final_kw,merged_refs) %>% 
  relocate((refID:PY),.after = DI) %>% 
  separate(DE,c(LETTERS[seq( from = 1, to = 20 )]), sep = "\\|") %>% 
  pivot_longer(!DI:PY, names_to = "letter", values_to = "DE") %>% 
  select(-letter) %>% 
  drop_na(DE) %>% 
  mutate(DE=tolower(DE)) %>% 
  mutate(DE=trimws(DE)) %>% 
  rename(kw_refined=DE)

# summary(is.na(kw_refined$kw_refined))

kw_refined<-bind_rows(final_kw,kw_refined)

kw_refined<-write_csv(kw_refined,"./bibliometrics/data_clean/kw_refined.csv")


# clustering --------------------------------------------------------------
# https://cran.r-project.org/web/packages/textmineR/vignettes/b_document_clustering.html
library(textmineR)

kw<-kw_refined %>% 
  filter(!is.na(jrnl_cat)) %>% 
  # filter(jrnl_cat=="tropical") %>% 
  group_by(jrnl_cat) %>% 
  sample_n(1000)
  

kw_dtm<-kw %>% 
  count(jrnl_cat,kw_refined) %>% 
  rename(document=jrnl_cat,term=kw_refined, count=n)


dtm <- CreateDtm(doc_vec = kw_dtm$term, # character vector of documents
                 doc_names = kw_dtm$document, # document names
                 ngram_window = c(1, 4), # minimum and maximum n-gram length
                 stopword_vec = c(stopwords::stopwords("en"), # stopwords from tm
                                  stopwords::stopwords(source = "smart")), # this is the default value
                 lower = TRUE, # lowercase - this is the default value
                 remove_punctuation = FALSE, # punctuation - this is the default
                 remove_numbers = FALSE, # numbers - this is the default
                 verbose = FALSE, # Turn off status bar for this demo
                 cpus = 2) # default is all available cpus on the system


tf_mat <- TermDocFreq(dtm)


# TF-IDF and cosine similarity
tfidf <- t(dtm[ , tf_mat$term ]) * tf_mat$idf

tfidf <- t(tfidf)

csim <- tfidf / sqrt(rowSums(tfidf * tfidf))

csim <- csim %*% t(csim)

cdist <- as.dist(1 - csim)


hc <- hclust(cdist, "ward.D")

clustering <- cutree(hc, 4)

plot(hc, main = "Hierarchical clustering of 100 NIH grant abstracts",
     ylab = "", xlab = "", yaxt = "n")

rect.hclust(hc, 10, border = "red")


p_words <- colSums(dtm) / sum(dtm)

cluster_words <- lapply(unique(clustering), function(x){
  rows <- dtm[ clustering == x , ]
  
  # for memory's sake, drop all words that don't appear in the cluster
  rows <- rows[ , colSums(rows) > 0 ]
  
  colSums(rows) / sum(rows) - p_words[ colnames(rows) ]
})


# create a summary table of the top 5 words defining each cluster
cluster_summary <- data.frame(cluster = unique(clustering),
                              size = as.numeric(table(clustering)),
                              top_words = sapply(cluster_words, function(d){
                                paste(
                                  names(d)[ order(d, decreasing = TRUE) ][ 1:5 ], 
                                  collapse = ", ")
                              }),
                              stringsAsFactors = FALSE)
cluster_summary




kw<-kw_refined %>% 
  select(kw_refined) %>% 
  distinct(kw_refined)


library(sjmisc)
old_string<-kw$kw_refined
newstring <- group_str(old_string)
newstring <- as_tibble(newstring) %>% 
  separate(value, sep=",", into=c("first","second","third")) 
# Christensen, A. P., & Kenett, Y. (2019, October 22). 
# Semantic Network Analysis (SemNA): A Tutorial on Preprocessing, Estimating, 
# and Analyzing Semantic Networks. https://doi.org/10.1037/met0000463
library(SemNetCleaner)
library(SemNeT)
load.dictionaries("general")
kw<-kw_refined %>% select(kw_refined)
clean <- textcleaner(data = kw, miss = 99,
                     partBY = "row", dictionary = "general")










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




