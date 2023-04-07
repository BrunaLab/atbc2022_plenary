
# read in packages --------------------------------------------------------

library(tidyverse)
library(refsplitr)
library(countrycode)
library(janitor)
library(tictoc)
library(opencage)
library(usethis)


# now refsplitr -----------------------------------------------------------


# 1) wos_unique_refs_authors_prelim
#   refs unique to WOS in output format from authors_clean()

head(complete_data)



# wos_unique_refs<-read_csv("./bibliometrics/data_clean/complete_data.csv") 
unique(complete_data$SO)
# foo<-complete_data %>% filter(is.na(SO))
# wos_unique_refs<-wos_unique_refs %>% 
#   filter(SO!="rbt")
# unique()
missingAU<-complete_data %>% 
  filter(is.na(AF)) %>% 
  group_by(SO,PY) %>% 
  summarize(n())

complete_data<-complete_data %>% filter(is.na(AU)==FALSE) %>% ungroup()
glimpse(complete_data)
# Testing if scopus and wos are bohth working
# 
# scopus_100<-complete_data %>%       
#   filter(source=="scopus") %>% 
#   slice(400:500) %>% 
#   slice(263,127:128) %>% 
#   mutate_all(as.character())
#   slice_sample(n=5000)
#   # filter(source=="wos") %>% 
#   # slice_head(n=126)
#   
#   scopus_100$source<-as.character(scopus_100$source)
# 
#   
# 
# scopus_100_long<-scopus_100 %>% 
#   mutate(counter=c(1,2,3)) %>% 
#   relocate(counter,.before=1) %>% 
# pivot_longer(cols=source:refID,names_to = "code",values_to = "value",names_repair ="check_unique" ) %>% 
#   pivot_wider(names_from = "counter",values_from = "value")
# 
# # look at the " "!!!!
# #127
# 211
# 263
# scopus_100$C1[1]<-scopus_100$C1[3]
# scopus_100$AU[1]<-scopus_100$AU[3]
# scopus_100$AF[1]<-scopus_100$AF[3]
# names(scopus_100)
# 
# scopus_100[1]<-scopus_100[3]
# wos_100<-complete_data %>% 
#   filter(source=="scopus") %>% 
#   slice_sample(n=100)
# # 
# summary(is.na(scopus_100$AF))
# summary(is.na(scopus_100$AU))
# 
# scopus_unique<-scopus_unique %>% 
# mutate(AU = if_else(is.na(AU), "missing",AU))
# 
# # scopus_100$OI<-gsub("-","/",scopus_100$OI)
# # scopus_100$OI<-paste("-","/",scopus_100$OI)
complete_data_authors<-authors_clean(complete_data)
complete_data_authors_prelim<-complete_data_authors$prelim
complete_data_authors_review<-complete_data_authors$review

summary(as.factor(complete_data_authors_prelim$OI))
write_rds(complete_data_authors,"./bibliometrics/data_clean/complete_data_authors.rds")
write_csv(complete_data_authors_prelim,"./bibliometrics/data_clean/complete_data_authors_prelim.csv")
write_csv(complete_data_authors_review,"./bibliometrics/data_clean/complete_data_authors_review.csv")






# # 2) common_refs_authors_prelim
# #   read in from both scopus AND wos. removed from the scopus and wos files and
# #   cleaned up separately, then processed with authors_clean() 
# 
# # load "wos_refs_raw": original search after processed by refernces_read() ---
# common_refs<-read_csv("./bibliometrics/data_clean/common_refs.csv") 
# names(common_refs2)
# names(common_refs2)
# head(common_refs_authors_prelim)
# names(common_refs_authors_prelim)
# names(common_refs)


# %>% 
#   filter(refID!=47764) 
# 
# Remove Duplicate Column Names
duplicated_names <- duplicated(colnames(common_refs))
common_refs<-common_refs[!duplicated_names]

common_refs<-common_refs %>% 
  mutate(AF=if_else(is.na(AF), AU,AF)) %>% 
  mutate(AU=if_else(is.na(AU), AF,AU)) %>% 
  filter(!is.na(AF)) %>% 
  filter(!is.na(AU)) 

# common_refs<-common_refs %>% filter(SO!="rbt")
# 
# names(common_refs)
# 
#   select(filename=filename_wos,AB,AF,AU,BP,C1,CR,DE,DI,
#          EM,EP,FN,FU,PD_wos,PG_wos,PT_wos,PU_wos,PY_wos,RI_wos,OI_wos,PM_wos,
#          RP_wos,SC_wos,SN,TC,TI,UT,VL_wos,WC_wos,Z9_wos,refID) %>% 
# 
# # 
# %>% 
#   mutate(EM='---@---') %>% 
#   mutate(RP=C1) %>% 
#   mutate(RI=C1) %>% 

# common_refs2<-common_refs %>% slice(1:8000)
common_refs2<-common_refs %>% slice(8000:12000)
# common_refs2<-common_refs %>% slice(12000:12000)
common_refs_authors<-authors_clean(common_refs)
common_refs_authors_prelim<-common_refs_authors$prelim
common_refs_authors_review<-common_refs_authors$review

write_rds(common_refs_authors,"./bibliometrics/data_clean/common_refs_authors.rds")
write_csv(common_refs_authors_prelim,"./bibliometrics/data_clean/common_refs_authors_prelim.csv")
write_csv(common_refs_authors_review,"./bibliometrics/data_clean/common_refs_authors_review.csv")



#  3) `scopus_unique_refs`
#   Unique to scopus. 
#   Need to put in format that looks like output from authors_clean()
head(scopus_unique_refs)
names(scopus_unique_refs)
names(scopus2_authors_affils)

scopus2_authors_affils<-read_csv("./bibliometrics/data_clean/scopus2_authors_affils.csv") %>% 
  unite("address",university:country, sep = ", ", remove=FALSE, na.rm=TRUE) %>% 
  relocate(refID,.before=1) %>% 
  mutate_all(as.character)

scopus2_unique<-read_csv("./bibliometrics/data_clean/scopus_unique_refs.csv") %>% 
  mutate_all(as.character)

# names(scopus_unique_refs)
# scopus_unique_refs$refID
# scopus_unique$refID

scopus2_authors_prelim<-inner_join(scopus2_authors_affils,
                                   scopus_unique,
                                   by=c("SO","PY","refID")) %>% 
  arrange(refID,author_count.x) %>% 
  relocate(author_count.x,author_count.x,.after=1) %>% 
  rename(author_count=author_count.x) %>% 
  group_by(authid) %>% 
  fill(c(city,country), .direction=c("down")) %>% 
  ungroup()


scopus2_authors_affils
scopus2_authors_prelim %>% count(country) %>% arrange(desc(n))
scopus2_authors_prelim %>% select(author_count,author_count.y) %>% arrange(desc(n))
# still need to find any with missing affils and fill in with autid from author_affils
scopus2_authors_prelim %>% count(SO) %>% arrange(desc(n))
write_csv(scopus2_authors_prelim,"./bibliometrics/data_clean/scopus_authors_prelim.csv")



# bind together unique to WOS and both SCOPUS/WOS--------------------------

common_refs_authors_prelim<-read_csv("./bibliometrics/data_clean/common_refs_authors_prelim.csv") 
wos_unique_authors_prelim<-read_csv("./bibliometrics/data_clean/wos_refs_35_end_authors_prelim.csv")%>% 
  mutate(refID=as.character(refID))
all_wos_authors_prelim<-bind_rows(wos_unique_authors_prelim, 
                                  common_refs_authors_prelim)



# correct the author country, city ----------------------------------------------


unique(all_wos_authors_prelim$country)

all_wos_authors_prelim<-all_wos_authors_prelim %>% 
  mutate(country=tolower(country)) %>% 
  mutate(country = case_when(
    country ==  "edo venezuela" ~ "venezuela",
    country ==  "democratic republic congo" ~ "democratic republic of congo",
    country ==  "viet nam" ~ "vietnam",
    country ==  "virgin islands (us)" ~ "us virgin islands",
    country ==  "usa" & city=="aa" ~ "panama",
    country ==  "franklin and marshall college"~"usa",
    country ==  "universidad de el salvador"  ~"el salvador",
    country ==  "dominican republic (the)" ~ "domincan republic",
    country ==  "us"~ "usa", 
    country ==  "japan international cooperation agency" ~"japan",
    country ==  "the netherlands"    ~"netherlands",
    country ==  "república argentina"~"argentina",
    country ==  "indian agricultural research institute"  ~"india",
    country ==  "argentina_"   ~"argentina",
    country ==  "asociación de especialistas en cocodrilos-costa rica" ~"costa rica",
    country ==  "senegambia" ~"senegal",
    country ==  "british virgin isl"~"british virgin islands",
    country ==  "st kitts & nevi" ~"st kitts and nevis",
    country ==  "university of costa rica"~"costa rica",
    country ==  "dominican rep"  ~"dominican republic",
    country ==  "dem rep congo"  ~ "democratic republic of congo",
    country ==  "bermuda institute of ocean sciences"~"bermuda",
    country ==  "united states of america" ~"usa",
    country ==  "sao tome & prin"   ~"sao tome and principe",
    country ==  "Colombia"~"colombia",
    country ==  "Cuba"  ~"cuba",
    country ==  "Cuba"  ~"cuba",
    country ==  "pr china" ~"china",
    country ==  "ak" ~ "usa",
    country ==  "alaska" ~ "usa",
    country ==  "alemania" ~ "germany",
    country ==  "austl"~ "australia",
    country ==  "author for correspondence"~ "italy",
    country ==  "ba"~ "brazil",
    country ==  "baja california méxico"~ "mexico",
    country ==  "bogotá"~ "colombia",
    country ==  "brasil"~ "brazil",
    country ==  "brewster academy"~ "colombia",
    country ==  "bundes republik"~ "germany",
    country ==  "c z"~ "czech republic",
    country ==  "cent afr republ"~ "central african republic",
    country ==  "central america director of science"~ "costa rica",
    country ==  "centre de recherches océanologiques (cro)"~ "ivory coast",
    country ==  "centro de investigación científica y educación superior de ensenada (cicese)"~ "mexico",
    country ==  "centro de investigación en ciencias del mar y limnología (cimar)"~ "costa rica",
    country ==  "centro de investigación y de estudios avanzados"~ "mexico",
    country ==  "centro de investigaciones de ambiente de camagüey"~ "Cuba",
    country ==  "centro de investigaciones geofísicas (cigefi)"~ "costa rica",
    country ==  "centro nacional de investigaciones agropecuarias"~ "ecuador",
    country ==  "centro nacional de pesquisa para a conservação de predadores naturais (cenap"~ "brazil",
    country ==  "centro tropical de investigación y enseñanza"~ "costa rica",
    country ==  "ciidir sinaloa"~ "mexico",
    country ==  "cinvestav-ipn"~ "mexico",
    country ==  "columbia"~ "colombia",
    country ==  "comisión de investigaciones científicas (cic)"~ "argentina",
    country ==  "comisión nacional para el conocimiento y uso de la biodiversidad (conabio)"~ "mexico",
    country ==  "conicet"~ "argentina",
    country ==  "consejo nacional de investigaciones científicas y técnicas (conicet)"~ "argentina_",
    country ==  "corresponding author"~ "india",
    country ==  "corresponding autor"~ "costa rica",
    country ==  "cz"~ "panama",
    country ==  "czechoslovakia"~ "czech republic",
    country ==  "d f"~ "mexico",
    country ==  "departamento de acuacultura"~ "mexico",
    country ==  "departamento de aprovechamiento y manejo de recursos acuáticos"~ "mexico",
    country ==  "departamento de ciencias del centro universitario de la costa"~ "mexico",
    country ==  "departamento de estudios para el desarrollo sustentable de la zona costera"~ "mexico",
    country ==  "departamento de química"~ "venezuela",
    country ==  "departamento producción agrícola y animal"~ "mexico",
    country ==  "dept biología de organismos"~ "venezuela",
    country ==  "deutschland"~ "germany",
    country ==  "df"~ "mexico",
    country ==  "dpto de biología"~ "cuba",
    country ==  "ecological and environmental sciences"~ "usa",
    country ==  "england"~ "uk",
    country ==  "escuela de ciencias aplicadas del mar"~ "venezuela",
    country ==  "escuela nacional de ciencias biológicas del instituto politécnico nacional"~ "mexico",
    country ==  "estado de méxico"~ "mexico",
    country ==  "american university" ~ "usa",
    country ==  "author for correspondence" ~ "italy",
    country ==  "bonaire" ~ "bonaire",
    country ==  "campo experimental" ~ "mexico",
    country ==  "cascadia research collective" ~ "usa",
    country ==  "catalina island conservancy" ~ "usa",
    country ==  "comisión de investigaciones científicas (cic) de la provincia de buenos aires" ~ "argentina",
    country ==  "comisión de investigaciones científicas de la provincia de buenos aires" ~ "argentina",
    country ==  "czench republic" ~ "czech republic",
    country ==  "edinburgh university" ~ "uk",
    country ==  "eeuu" ~ "usa",
    country ==  "españa" ~ "spain",
    country ==  "fl" ~ "usa",
    country ==  "florida a and m university" ~ "usa",
    country ==  "florida international university" ~ "usa",
    country ==  "georgia southern university" ~ "usa",
    country ==  "fed rep ger" ~ "germany",
    country ==  "hanyang university" ~ "usa",
    country ==  "harvard university" ~ "usa",
    country ==  "italia" ~ "italy",
    country ==  "jardín botánico wilson" ~ "costa rica",
    country ==  "ma" ~ "usa",
    country ==  "md" ~ "usa",
    country ==  "mex" ~ "mexico",
    country ==  "méxico" ~ "mexico",
    country ==  "méxico df" ~ "mexico",
    country ==  "méxico;" ~ "mexico",
    country ==  "n wal" ~ "uk",
    country ==  "nj" ~ "usa",
    country ==  "north ireland" ~ "uk",
    country ==  "panamá" ~ "panama",
    country ==  "panthera" ~ "usa",
    country ==  "papua n guinea" ~ "papua new guinea",
    country ==  "GIN"~ "guinea",
    country ==  "PNG"~ "papua new guinea",
    country ==  "papua n guinea" ~ "papua new guinea",
    country ==  "patuxent wildlife research center" ~ "usa",
    country ==  "perú" ~ "peru",
    country ==  "pontificia universidad javeriana" ~ "colombia",
    country ==  "república de panamá" ~ "panama",
    country ==  "smithsonian tropical research institute" ~ "panama",
    country ==  "stjohns river water management district" ~ "usa",
    country ==  "texas a & m university at galveston" ~ "usa",
    country ==  "texas tech university" ~ "usa",
    country ==  "the academy of natural sciences" ~ "usa",
    country ==  "the field museum" ~ "usa",
    country ==  "the royal swedish academy" ~ "sweden",
    country ==  "uiversité de la polynésie française" ~ "french polynesia",
    country ==  "unam" ~ "mexico",
    country ==  "universidad autónoma de campeche" ~ "mexico",
    country ==  "universidad autónoma de san luis potosí" ~ "mexico",
    country ==  "universidad autónoma de sinaloa" ~ "mexico",
    country ==  "universidad autónoma de tlaxcala" ~ "mexico",
    country ==  "universidad autónoma metropolitana unidad iztapalapa" ~ "mexico",
    country ==  "universidad autónoma metropolitana-iztapalapa" ~ "mexico",
    country ==  "universidad autónoma nuevo león" ~ "mexico",
    country ==  "universidad de guadalajara" ~ "mexico",
    country ==  "universidad de puerto rico en humacao" ~ "puerto rico",
    country ==  "MAC" ~ "Macau",
    country ==  "PRI" ~ "Puerto Rico",
    country ==  "universidad de puerto rico en humacao" ~ "puerto rico",
    country ==  "universidad del estado de méxico" ~ "mexico",
    country ==  "universidad javeriana" ~ "colombia",
    country ==  "universidad juárez autónoma de tabasco" ~ "mexico",
    country ==  "universidad nacional autónoma de méxico" ~ "mexico",
    country ==  "universidad tecnológica de tabasco" ~ "mexico",
    country ==  "universidad veracruzana córdoba" ~ "mexico",
    country ==  "universidade católica de são paulo" ~ "brazil",
    country ==  "universidade de são paulo" ~ "brazil",
    country ==  "universidade do estado de mato grosso" ~ "brazil",
    country ==  "universidade do estado do rio de janeiro" ~ "brazil",
    country ==  "universidade estadual de santa cruz-rodovia ilhéus-itabuna" ~ "brazil",
    country ==  "universidade estadual do ceará" ~ "brazil",
    country ==  "universidade federal de pernambuco" ~ "brazil",
    country ==  "universidade federal de uberlândia" ~ "brazil",
    country ==  "universidade federal rural do rio de janeiro" ~ "brazil",
    country ==  "université du québec à montréal" ~ "canada",
    country ==  "universities of amsterdam and wageningen" ~ "netherlands",
    country ==  "university of california" ~ "usa",
    country ==  "university of hawaii" ~ "usa",
    country ==  "university of minnesota" ~ "usa",
    country ==  "university of south alabama" ~ "usa",
    country ==  "university of the andes" ~ "colombia",
    country ==  "university of toledo" ~ "usa",
    country ==  "university of wisconsin" ~ "usa",
    country ==  "ut" ~ "usa",
    country ==  "va" ~ "usa",
    country ==  "wales" ~ "uk",
    country ==  "yugoslavia" ~ "serbia",
    country ==  "universidad autónoma metropolitana" ~ "mexico",
    country ==  "the school for field studies" ~ "costa rica",
    country ==  "policlínica y diagnóstico veterinario" ~ "mexico",
    country ==  "ponce school of medicine and health sciences" ~ "puerto rico",
    country ==  "pr" ~ "puerto rico",
    country ==  "presidencia municipal" ~ "mexico",
    country ==  "programa de investigación científica aplicada" ~ "costa rica",
    country ==  "programa de investigación científica aplicada (tramil" ~ "costa rica",
    country ==  "pronatura noroeste ac" ~ "mexico",
    country ==  "región pacífico sur del inapesca" ~ "mexico",
    country ==  "reproducción-fisiología y biotecnología" ~ "colombia",
    country ==  "robert gordon university" ~ "uk",
    country ==  "rosenstiel school for marine and atmospheric science" ~ "usa",
    country ==  "rosenstiel school of marine and atmospheric science" ~ "usa",
    country ==  "sax" ~ "costa rica",
    country ==  "scotland" ~ "uk",
    country ==  "seascape caribbean" ~ "jamaica",
    country ==  "sede universitaria municipal de qüemado de güines" ~ "cuba",
    country ==  "sistema nacional de areas de conservación" ~ "costa rica",
    country ==  "smithsonian institution" ~ "usa",
    country ==  "st george’s university" ~ "grenada",
    country ==  "the nature conservancy" ~ "costa rica",
    country ==  "trent university" ~ "canada",
    country ==  "unavco" ~ "usa",
    country ==  "united arab rep" ~ "egypt",
    country ==  "universidad antonio nariño" ~ "colombia",
    country ==  "universidad autónoma chapingo" ~ "mexico",
    country ==  "universidad de concepción" ~ "chile",
    country ==  "universidad de fukuoka" ~ "japan",
    country ==  "universidad de mar" ~ "mexico",
    country ==  "universidad de nariño" ~ "colombia",
    country ==  "universidad de oriente" ~ "cuba",
    country ==  "universidad del país vasco" ~ "spain",
    country ==  "universidad del tolima" ~ "colombia",
    country ==  "universidad del valle" ~ "colombia",
    country ==  "universidad jorge tadeo lozano" ~ "colombia",
    country ==  "universidad nacional de la plata" ~ "argentina",
    country ==  "universidad simón bolívar" ~ "venezuela",
    country ==  "universidad nacional" ~ "costa rica",
    country ==  "université de cocody-abidjan" ~ "ivory coast",
    country ==  "wi" ~ "usa",
    country ==  "university of the virgin islands" ~ "us virgin islands",
    country ==  "university of the west indies" ~ "jamaica",
    country ==  "universtiy of hull" ~ "uk",
    country ==  "unrc" ~ "argentina",
    country ==  "usmanu dan fodiyo university" ~ "nigeria",
    country ==  "w ind assoc st" ~ "jamaica",
    # country ==  "west indies" ~ NA, #some of these are jamaica, others trinidad
    country ==  "centre national de recherche agronomique (cnra)" ~ "ivory coast",
    country ==  "centro de investigaciones geofísicas (cigefi)" ~ "costa rica",
    country ==  "centro zootoxicológico de misiones" ~ "argentina",
    country ==  "cic" ~ "argentina",
    country ==  "cicimar - ipn" ~ "mexico",
    country ==  "conservación internacional" ~ "costa rica",
    country ==  "division of fish and wildlife" ~ "usa",
    country ==  "dpto de biología" ~ "cuba",
    country ==  "escuela de ciencias" ~ "venezuela",
    country ==  "escuela nacional de ciencias biológicas" ~ "mexico",
    country ==  "escuela superior de ecología marina gran vía tropical" ~ "mexico",
    country ==  "escuela superior de sanidad dr ramón carrillo" ~ "argentina",
    country ==  "estudiante del posgrado del instituto de ecología" ~ "mexico",
    country ==  "facultad de ciencias naturales y museo de la plata" ~ "argentina",
    country ==  "fish and wildlife research institute" ~ "usa",
    country ==  "florida caribbean science center" ~ "usa",
    country ==  "fundación keto" ~ "costa rica",
    country ==  "fundación para el ecodesarrollo y la conservación (fundaeco)" ~ "guatemala",
    country ==  "fundación universidad de bogotá jorge tadeo lozano" ~ "colombia",
    country ==  "global vision international" ~ "usa",
    country ==  "grupo de exploraciones científicas minas de aroa" ~ "venezuela",
    country ==  "grupo ecología y conservación de fauna silvestre" ~ "colombia",
    country ==  "herbario amo" ~ "mexico",
    country ==  "hnb garhwal university" ~ "india",
    country ==  "hospital nacional de niños" ~ "costa rica",
    country ==  "il" ~ "usa",
    country ==  "inba - conicet" ~ "argentina",
    country ==  "inc" ~ "usa",
    country ==  "rep congo" ~ "republic of the congo",
    country ==  "institute for tropical marine ecology inc" ~ "dominica",
    country ==  "institute for tropical marine ecology inc (itme)" ~ "dominica",
    country ==  "instituto costarricense de electricidad" ~ "costa rica",
    country ==  "instituto de biodiversidad (inbio)" ~ "costa rica",
    country ==  "instituto de investigaciones en biomedicina y ciencias aplicadas" ~ "venezuela",
    country ==  "instituto de investigaciones marinas y costeras" ~ "colombia",
    country ==  "instituto de pesca" ~ "brazil",
    country ==  "instituto nacional de biodiversidad" ~ "costa rica",
    country ==  "instituto nacional de ciencias médicas y de la nutrición salvador zubirán" ~ "cuba",
    country ==  "instituto nacional de la pesca" ~ "mexico",
    country ==  "instituto nacional de pesca" ~ "mexico",
    country ==  "instituto para estudios de ecosistemas tropicales" ~ "puerto rico",
    country ==  "instituto politécnico nacional" ~ "mexico",
    country ==  "instituto politécnico nacional (cicimar" ~ "mexico",
    country ==  "instituto venezolano de investigaciones científicas" ~ "venezuela",
    country ==  "intituto politécnico nacional" ~ "mexico",
    country ==  "casilla 4040 correo 3" ~"chile",
    country ==  "lab sist plantas vasculares" ~ "argentina",
    country ==  "laboratorio de ensayos biológicos" ~ "costa rica",
    country ==  "macalester college" ~ "usa",
    country ==  "milwaukee public museum" ~ "usa",
    country ==  "ministerio de agricultura y ganadería" ~ "el salvador",
    country ==  "museo de ciencias naturales de guanare (mcng-unellez)" ~ "venezuela",
    country ==  "museo de la plata" ~ "argentina",
    country ==  "museo marino de margarita boca del río" ~ "venezuela",
    country ==  "national marine fisheries service" ~ "usa",
    country ==  "natural history museum of los angeles county" ~ "usa",
    country ==  "negros oriental state university" ~ "philippines",
    country ==  "neth antilles" ~ "curacao",
    country ==  "north-eastern hill university" ~ "india",
    country ==  "nrc on plant biotechnology" ~ "india",
    country ==  "núcleo de sucre universidad de oriente" ~ "venezuela",
    country ==  "ophelia" ~ "usa",  
    country == "centro de investigación de cetáceos de costa rica ceic"~ "costa rica",
    country == "cote ivoire"~ "ivory coast",
    country == "peoples r china"~ "china",
    country == "trin & tobago"~ "trinidade and tobago",
    country == "trinid tobago"~ "trinidade and tobago",
    country == "trinidad and tobago"~ "trinidade and tobago",
    country == "trinidad tobago"~ "trinidade and tobago",
    country == "u arab emirates"~ "united arab emirates",
    country == "united states"~ "usa",
    country == "universidad central de venezuela"~ "venezuela",
    country == "universidad de costa rica"~ "costa rica",
    country == "universidad de puerto rico"~ "puerto rico",
    country == "universidad nacional de colombia"~ "colombia",
    country == "universidad nacional de colombia sede medellín"~ "colombia",
    country == "universidad nacional de costa rica"~ "costa rica",
    country == "university of puerto rico"~ "puerto rico",
    country == "us geological survey"~ "usa",
    country == "west germany"~ "germany",
    country == "instituto oceanográfico de venezuela"~ "venezuela",
    country == "institute of ecology"~ "sweden",
    country == "united kingdom"~ "uk",
    country == "fachhochschule hildesheim"~"germany",
    country == "fundación ecoandina"~"argentina",
    country == "cote d'ivoire"~"ivory coast",
    country == "russian federation"~"russia",
    country == "centro oceanográfico de gijón" ~ "spain",
    country == "abt verhaltensforschung"~"austria",
    country == "bashundhara r" ~ "bangladesh",
    country == "libyan arab jamahiriya"~"libya",
    TRUE ~ as.character(country))) %>%
  mutate(city = case_when(city== "- mcgregor rd"~"cairns",
                          city== "chetmal"~"chetumal",
                          city== "aa"& country=="panama"~"balboa",
                          city== "ur"~"libreville",
                          city== "pob"~"libreville",
                          city== "bp"~"libreville",
                          city== "a c"~"xalapa",
                          city== "emvt"~"harare",
                          city== "emvt"~"harare",
                          city== "bag"~"chinhoyi",
                          city== "private bag"~"chinhoyi",
                          state== "bulawayo"~"bulawayo",
                          state== "chinhoyi"~"chinhoyi",
                          city== "cimep"~state,
                          city== "cimep"~state,
                          city== "conicet"~state,
                          city== "conicet unc"~state,
                          city== "inibioma"~state,
                          city== "lecotono crub"~state,
                          city== "pidba"~state,
                          city== "liey"~state,
                          city== "highett vic"~"highett",
                          city== "icivet litoral"~"santo tome",
                          state== "ghent"~"ghent",
                          state== "antwerp"~"antwerp",
                          state== "leuven"~"leuven",
                          city== "n ryde"~"north ryde",
                          city== "s perth"~"perth",
                          city== "n terrace adelaide"~"adelaide",
                          city== "floreat pk"~"floreat",
                          city== "highett vic"~"highett",
                          city== "lord howe isl"~"lord howe island",
                          city== "bedford pk"~"bedford park",
                          city== "black mt"~"black mountain",
                          city== "mt gambier"~"mount gambier",
                          city== "dutton pk"~"mount dutton",
                          city== " cr leiden"~"leiden",
                          city== "unidchetumal"~"chetumal",
                          city== " copenhagen o"~ "copenhagen",
                          city== "proyecto curricular ingn forestal"~ "bogota",
                          city== "dc" & country=="usa" ~ "washington",
                          TRUE ~ as.character(city))) %>%
  mutate(city=na_if(city,"na")) %>%
  mutate(country=na_if(country,"na")) %>%
  mutate(city=gsub("aa ","",city)) %>% 
  mutate(city=gsub("ab ","",city)) %>% 
  mutate(city=gsub("ac ","",city)) %>% 
  mutate(city=gsub("ad ","",city)) %>% 
  mutate(city=gsub("ap ","",city)) %>% 
  mutate(city=if_else(str_detect(city,"dept "), state,city)) %>% # there are a bunch where the "city" is "dept of...". replace those with the inst
  mutate(city=if_else(str_detect(city," dept"), state,city)) %>% # there are a bunch where the "city" is "dept of...". replace those with the inst
  mutate(city=if_else(str_detect(city,"ecol"), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"zool"), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"genética y evol"), state,city)) %>% 
  mutate(city=if_else(str_detect(city," inst"), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"inst "), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"environm"), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"apartado"), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"apdo"), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"arctic "), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"toxicol"), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"acad"), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"az "), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"bot "), state,city)) %>% 
  mutate(city=if_else(str_detect(city," bot"), state,city)) %>% 
  mutate(city=if_else(str_detect(city," cent"), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"cent "), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"technol"), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"aj "), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"campus "), state,city)) %>% 
  mutate(city=if_else(str_detect(city," campus"), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"ambient"), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"escola"), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"escuela"), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"sch "), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"university "), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"campus "), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"agr "), state,city)) %>% 
  mutate(city=if_else(str_detect(city," agr"), state,city)) %>% 
  mutate(city=if_else(str_detect(city," & "), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"ab "), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"ae "), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"african"), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"europ"), state,city)) %>% 
  mutate(city=if_else(str_detect(city," lab"), state,city)) %>% 
  mutate(city=if_else(str_detect(city," project"), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"cnrs"), state,city)) %>% 
  mutate(city=if_else(str_detect(city," ecosist"), state,city)) %>% 
  mutate(city=if_else(str_detect(city," nat"), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"nat "), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"unesp"), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"div "), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"fcis"), state,city)) %>% 
  mutate(city=if_else(str_detect(city," div"), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"unit "), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"evolut "), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"ctr "), state,city)) %>% 
  mutate(city=if_else(str_detect(city," ctr"), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"unidade "), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"aa "), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"ab "), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"ab "), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"res "), state,city)) %>% 
  mutate(city=if_else(str_detect(city," res"), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"programa "), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"ufscar"), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"ufrj"), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"sede "), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"vegetal "), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"biol "), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"herbario"), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"herbarium"), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"geog"), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"natl "), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"cirad"), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"conserva"), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"univ"), state,city)) %>% 
  mutate(city=if_else(str_detect(city," eco"), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"-"), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"- "), state,city)) %>% 
  mutate(city=if_else(str_detect(city," rd"), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"inra"), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"floresta"), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"plant "), state,city)) %>% 
  mutate(city=if_else(str_detect(city," plant"), state,city)) %>% 
  mutate(city=if_else(str_detect(city," anim "), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"dipartimento "), state,city)) %>% 
  mutate(city=if_else(str_detect(city," invest "), state,city)) %>% 
  mutate(city=if_else(str_detect(city," calle "), state,city)) %>% 
  mutate(city=if_else(str_detect(city," sci "), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"sci "), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"ecosyst "), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"via "), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"entomol"), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"calle "), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"rua "), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"rue "), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"unite "), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"umr "), state,city)) %>% 
  mutate(city=if_else(str_detect(city," managment"), state,city)) %>% 
  mutate(city=if_else(str_detect(city," state"), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"state "), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"&"), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"soil"), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"biodiv"), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"sci"), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"state "), state,city)) %>% 
  mutate(city=if_else(str_detect(city," program"), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"programme"), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"museu "), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"museum"), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"museo "), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"csic"), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"pharm"), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"unit "), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"evolut"), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"ave "), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"biol"), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"umr"), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"cibio"), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"wageningen"), "wageningen",city)) %>% 
  mutate(city=if_else(str_detect(city,"groningen"), "groningen",city)) %>% 
  mutate(city=if_else(str_detect(city,"haren"), "haren",city)) %>% 
  mutate(city=if_else(str_detect(city,"utrecht"), "utrecht",city)) %>% 
  mutate(city=if_else(str_detect(city,"heteren"), "heteren",city)) %>% 
  mutate(city=if_else(str_detect(city,"amsterdam"), "amsterdam",city)) %>% 
  mutate(city=if_else(str_detect(city,"zurich"), "zurich",city)) %>% 
  mutate(city=if_else(str_detect(city,"aarhus"), "aarhus",city)) %>% 
  mutate(city=if_else(str_detect(city,"nijmegen"), "nijmegen",city)) %>% 
  mutate(city=if_else(str_detect(city,"virudr"), "virudunagar",city)) %>% 
  mutate(city=if_else(str_detect(city,"jdh"), "junagadh",city)) %>% 
  mutate(city=if_else(str_detect(city,"kaka"), "dharwad",city)) %>% 
  mutate(city=if_else(str_detect(city,"amherst"), "amherst",city)) %>% 
  mutate(city=if_else(str_detect(city,"pto iguazu"), "puerto iguazu",city)) %>% 
  mutate(city=if_else(str_detect(city,"av "), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"grp"), state,city)) %>% 
  mutate(city=if_else(str_detect(city,"yeddram"), "yeddumailaram",city)) %>% 
  mutate(city=if_else(str_detect(city,"baja california sur"), "isla natividad",city)) %>% 
  mutate(city=if_else(str_detect(city,"bellaterra cerdanyola"), "barcelona",city)) %>% 
  mutate(city=if_else(str_detect(city,"garcia calderon"), "iquitos",city)) %>% 
  mutate(city=if_else(str_detect(city,"grp"), "state",city)) %>% 
  mutate(city=if_else(str_detect(city,"grp"), "state",city)) %>% 
  mutate(city=if_else((country=="spain" & city=="val"), "valencia",city)) %>% 
  mutate(city=if_else((country=="brazil" & city=="bia"), "brasilia",city)) %>% 
  mutate(city=if_else((country=="brazil" & city=="ig"), "igrapiuna",city)) %>% 
  mutate(city=if_else((country=="brazil" & city=="mg"), "florestal",city)) %>% 
  mutate(city=if_else((country=="brazil" & city=="mt"), "alta floresta",city)) %>% 
  mutate(city=if_else((country=="brazil" & city=="pa"), "paranagua",city)) %>% 
  mutate(city=if_else((country=="brazil" & city=="rs"), "erechim",city)) %>% 
  mutate(city=if_else((country=="canada" & city=="on"), "ontario",city)) %>% 
  mutate(city=if_else((country=="canada" & city=="ab"), university,city)) %>% 
  mutate(city=gsub("univ ","",city)) %>% 
  mutate(city=if_else((country=="spain" & city=="g"), "granada",city)) %>% 
  mutate(city=if_else((country=="argentina" & postal_code=="encia"), "mendoza",city)) %>%  
  mutate(city=if_else((country=="argentina" & city=="df"), "buenos aires",city)) %>%  
  mutate(city=if_else((country=="argentina" & city=="pr"), "buenos aires",city)) %>%  
  mutate(postal_code=na_if(postal_code,"encia")) %>% 
  mutate(city=if_else((country=="argentina" & is.na(city)), state,city)) %>% 
  mutate(city=if_else((country=="canada" & city=="pq"& str_detect(address,"montreal")), "montreal",city)) %>%
  mutate(city=if_else((country=="canada" & city=="pq"& str_detect(address,"guelph")), "guelph",city)) %>%
  mutate(city=if_else((country=="canada" & city=="pq"& str_detect(address,"waterloo")), "waterloo",city)) %>%
  mutate(city=if_else((country=="canada" & city=="pq"& str_detect(address,"toronto")), "toronto",city)) %>%
  mutate(city=if_else((country=="canada" & city=="pq"& str_detect(address,"sherbrooke")), "sherbrooke",city)) %>%
  mutate(city=if_else((country=="canada" & city=="pq"& str_detect(address,"kingston")), "kingston",city)) %>%
  mutate(city=if_else((country=="canada" & city=="pq"& str_detect(address,"ottawa")), "ottawa",city)) %>% 
  mutate(city=if_else((country=="canada" & city=="pq"& str_detect(address,"london")), "london",city)) %>% 
  mutate(city=if_else((country=="canada" & city=="pq"& str_detect(address,"vancouver")), "vancouver",city)) %>% 
  mutate(city=if_else((country=="canada" & city=="pq"& str_detect(address,"victoria")), "victoria",city)) %>% 
  mutate(city=if_else((country=="canada" & city=="ontario"& str_detect(address,"guelph")), "guelph",city)) %>%
  mutate(city=if_else((country=="canada" & city=="ontario"& str_detect(address,"waterloo")), "waterloo",city)) %>%
  mutate(city=if_else((country=="canada" & city=="ontario"& str_detect(address,"toronto")), "toronto",city)) %>% 
  mutate(city=if_else((country=="canada" & city=="bc"& str_detect(address,"vancouver")), "vancouver",city)) %>% 
  mutate(city=if_else((country=="canada" & city=="bc"& str_detect(address,"victoria")), "victoria",city)) %>%
  mutate(city=if_else((country=="canada" & city=="bc"& str_detect(address,"burnaby")), "burnaby",city)) %>%
  mutate(city=if_else((country=="canada" & city=="bc"& str_detect(address,"nelson")), "nelson",city)) %>%
  mutate(city=if_else((country=="canada" & city=="bc"& str_detect(address,"prince george")), "prince george",city)) %>%
  mutate(city=if_else((country=="canada" & city=="bc"& str_detect(address,"vancouver")), "vancouver",city)) %>% 
  mutate(city=if_else((country=="canada" & city=="bc"& str_detect(address,"victoria")), "victoria",city)) %>%
  mutate(city=if_else((country=="canada" & city=="bc"& str_detect(address,"kelowna")), "kelowna",city)) %>%
  mutate(city=if_else((country=="canada" & city=="bc"& str_detect(address,"nanaimo")), "nanaimo",city)) %>%
  mutate(city=if_else((country=="canada" & city=="ontario"& str_detect(address,"ottawa")), "ottawa",city)) %>%
  mutate(city=if_else((country=="canada" & city=="ontario"& str_detect(address,"burnaby")), "burnaby",city)) %>%
  mutate(city=if_else((country=="canada" & city=="ontario"& str_detect(address,"edmonton")), "edmonton",city)) %>%
  mutate(city=if_else((country=="canada" & city=="ontario"& str_detect(address,"thunder bay")), "thunder bay",city)) %>%
  mutate(city=if_else((country=="canada" & city=="ontario"& str_detect(address,"hamilton")), "hamilton",city)) %>%
  mutate(city=if_else((country=="canada" & city=="ontario"& str_detect(address,"kingston")), "kingston",city)) %>%
  mutate(city=if_else((country=="canada" & city=="ontario"& str_detect(address,"peterborough")), "peterborough",city)) %>%
  mutate(city=if_else((country=="usa" & city=="pa" & str_detect(address,"university pk")), "university park",city)) %>%
  mutate(city=if_else((country=="canada" & city=="ottawa ka oy"), "ottawa",city)) %>%  
  mutate(city=if_else((country=="canada" & city=="ottawa kia oc"), "ottawa",city)) %>% 
  mutate(city=if_else((country=="usa" & city=="ca" & str_detect(address,"los angeles")), "los angeles",city)) %>%
  mutate(city=if_else((country=="usa" & city=="ca" & str_detect(address,"davis")), "davis",city)) %>%
  mutate(city=if_else((country=="usa" & city=="ca" & str_detect(address,"berkeley")), "berkeley",city)) %>%
  mutate(city=if_else((country=="usa" & city=="ca" & str_detect(address,"irvine")), "irvine",city)) %>%
  mutate(city=if_else((country=="usa" & city=="ca" & str_detect(address,"stanford")), "stanford",city)) %>%
  mutate(city=if_else((country=="usa" & city=="ca" & str_detect(address,"santa barbara")), "santa barbara",city)) %>%
  mutate(city=if_else((country=="usa" & city=="ca" & str_detect(address,"san diego")), "san diego",city)) %>%
  mutate(city=if_else((country=="usa" & city=="ca" & str_detect(address,"riverside")), "riverside",city)) %>%
  mutate(city=if_else((country=="usa" & city=="ca" & str_detect(address,"claremont")), "claremont",city)) %>%
  mutate(city=if_else((country=="usa" & city=="ca" & str_detect(address,"fresno")), "fresno",city)) %>%
  mutate(city=if_else((country=="usa" & city=="ca" & str_detect(address,"bodega bay")), "bodega bay",city)) %>%
  mutate(city=if_else((country=="usa" & city=="ca" & str_detect(address,"riverside")), "riverside",city)) %>% 
  mutate(city=if_else((country=="usa" & city=="mi" & str_detect(address,"ann arbor")), "ann arbor",city)) %>% 
  mutate(city=if_else((country=="usa" & city=="nc" & str_detect(address,"durham")), "durham",city)) %>% 
  mutate(city=if_else((country=="usa" & city=="nc" & str_detect(address,"chapel hill")), "chapel hill",city)) %>% 
  mutate(city=if_else((country=="usa" & city=="il" & str_detect(address,"chicago")), "chicago",city)) %>% 
  mutate(city=if_else((country=="usa" & city=="wa" & str_detect(address,"seattle")), "seattle",city)) %>% 
  mutate(city=if_else((country=="usa" & city=="il" & str_detect(address,"pullman")), "pullman",city)) %>% 
  mutate(city=if_else((country=="usa" & city=="ma" & str_detect(address,"cambridge")), "cambridge",city)) %>% 
  mutate(city=if_else((country=="usa" & city=="ma" & str_detect(address,"boston")), "boston",city)) %>% 
  mutate(city=if_else((country=="usa" & city=="ma" & str_detect(address,"woods hole")), "woods hole",city)) %>% 
  mutate(city=if_else((country=="usa" & city=="ma" & str_detect(address,"amherst")), "amherst",city)) %>% 
  mutate(city=if_else((country=="finland" & city=="hki"), "helsinki",city)) %>% 
  mutate(city=if_else((country=="finland" & city=="f hki"), "helsinki",city)) %>% 
  mutate(city=if_else((country=="spain" & city=="g" & str_detect(address,"granada")), "granada",city)) %>%
  mutate(city=if_else((country=="usa" & city=="ny" & str_detect(address,"syracuse")), "syracuse",city)) %>% 
  mutate(city=if_else((country=="usa" & city=="ny" & str_detect(address,"canton")), "canton",city)) %>% 
  mutate(city=if_else((country=="usa" & city=="ny" & str_detect(address,"flushing")), "flushing",city)) %>% 
  mutate(city=if_else((country=="usa" & city=="ny" & str_detect(address,"bronx")), "bronx",city)) %>% 
  mutate(city=if_else((country=="usa" & city=="ny" & str_detect(address,"garden city")), "garden city",city)) %>% 
  mutate(city=if_else((country=="usa" & city=="ny" & str_detect(address,"ithaca")), "ithaca",city)) %>% 
  mutate(city=if_else((country=="usa" & city=="ny" & str_detect(address,"stony brook")), "stony brook",city)) %>% 
  mutate(city=if_else((country=="usa" & city=="ny" & str_detect(address,"rochester")), "rochester",city)) %>% 
  mutate(city=if_else((country=="usa" & city=="ny" & str_detect(address,"binghamton")), "binghamton",city)) %>% 
  mutate(city=if_else((country=="usa" & city=="ny" & str_detect(address,"albany")), "albany",city)) %>% 
  mutate(city=if_else((country=="canada" & city=="pq" & str_detect(address,"ste anne de bellevue")), "ste anne de bellevue",city)) %>%
  mutate(city=if_else((country=="canada" & city=="pq" & str_detect(address,"quebec city")), "quebec city",city)) %>%
  mutate(city=if_else((country=="canada" & city=="pq" & str_detect(address,"rouyn noranda")), "rouyn noranda",city)) %>%
  mutate(city=if_else((country=="canada" & city=="pq" & str_detect(address,"rimouski")), "rimouski",city)) %>%
  mutate(city=if_else((country=="usa" & city=="tx" & str_detect(address,"lubbock")), "lubbock",city)) %>%
  mutate(city=if_else((country=="usa" & city=="tx" & str_detect(address,"austin")), "austin",city)) %>%
  mutate(city=if_else((country=="usa" & city=="tx" & str_detect(address,"houston")), "houston",city)) %>%
  mutate(city=if_else((country=="usa" & city=="tx" & str_detect(address,"dallas")), "dallas",city)) %>%
  mutate(city=if_else((country=="usa" & city=="fl" & str_detect(address,"gainesville")), "gainesville",city)) %>%
  mutate(city=if_else((country=="usa" & city=="fl" & str_detect(address,"sarasota")), "sarasota",city)) %>%
  mutate(city=if_else((country=="usa" & city=="fl" & str_detect(address,"miami")), "miami",city)) %>%
  mutate(city=if_else((country=="usa" & city=="fl" & str_detect(address,"university pk")), "university park",city)) %>%
  mutate(city=if_else((country=="usa" & city=="fl" & str_detect(address,"tallahassee")), "tallahassee",city)) %>%
  mutate(city=if_else((country=="usa" & city=="fl" & str_detect(address,"orlando")), "orlando",city)) %>%
  mutate(city=if_else((country=="usa" & city=="fl" & str_detect(address,"coral gables")), "coral gables",city)) %>%
  mutate(city=if_else((country=="usa" & city=="fl" & str_detect(address,"tampa")), "tampa",city)) %>%
  mutate(city=if_else((country=="usa" & city=="fl" & str_detect(address,"jacksonville")), "jacksonville",city)) %>%
  mutate(city=if_else((country=="usa" & city=="fl" & str_detect(address,"kennedy space ctr")), "kennedy space center",city)) %>%
  mutate(city=if_else((country=="usa" & city=="nj" & str_detect(address,"princeton")), "princeton",city)) %>%
  mutate(city=if_else((country=="usa" & city=="nj" & str_detect(address,"newark")), "newark",city)) %>%
  mutate(city=if_else((country=="usa" & city=="nj" & str_detect(address,"piscataway")), "piscataway",city)) %>%
  mutate(city=if_else((country=="usa" & city=="nj" & str_detect(address,"brunswick")), "brunswick",city)) %>%
  mutate(city=if_else((country=="usa" & city=="ct" & str_detect(address,"new haven")), "new haven",city)) %>%
  mutate(city=if_else((country=="usa" & city=="ct" & str_detect(address,"storrs")), "storrs",city)) %>%
  mutate(city=if_else((country=="usa" & city=="in" & str_detect(address,"terre haute")), "terre haute",city)) %>%
  mutate(city=if_else((country=="usa" & city=="in" & str_detect(address,"ft wayne")), "fort wayne",city)) %>%
  mutate(city=if_else((country=="usa" & city=="in" & str_detect(address,"indianapolis")), "indianapolis",city)) %>%
  mutate(city=if_else((country=="usa" & city=="in" & str_detect(address,"richmond")), "richmond",city)) %>%
  mutate(city=if_else((country=="usa" & city=="in" & str_detect(address,"notre dame")), "notre dame",city)) %>%
  mutate(city=if_else((country=="usa" & city=="in" & str_detect(address,"w lafayette")), "west lafayette",city)) %>%
  mutate(city=if_else((country=="usa" & city=="hi" & str_detect(address,"honolulu")), "honolulu",city)) %>%
  mutate(city=if_else((country=="usa" & city=="tx" & str_detect(address,"dallas")), "dallas",city)) %>%
  mutate(city=if_else((country=="usa" & city=="tx" & str_detect(address,"college station")), "college station",city)) %>%
  mutate(city=if_else((country=="usa" & city=="college pk"), "college park",city))  %>% 
  mutate(code = countrycode(country, origin = 'country.name', destination = 'iso3c')) %>%
  mutate(code = case_when(country ==  "curacao" ~ "cuw",
                          country ==  "bonaire" ~ "bes",
                          country ==  "us virgin islands" ~ "bes",
                          TRUE ~ as.character(code))) 
  
  # all_wos_authors_prelim$country<-tolower(all_wos_authors_prelim$country)

# see countries in alphavbetical order 


countries<-all_wos_authors_prelim %>% select(country) %>% group_by(country) %>% slice(1) %>% arrange()
unique(all_wos_authors_prelim$country)
# TODO: "republic of the congo"    


all_wos_authors_prelim<-all_wos_authors_prelim %>% mutate_all(trimws)

all_wos_authors_prelim %>% filter(is.na(country)) %>% count(SO,city,state) %>% arrange()

all_wos_authors_prelim<-all_wos_authors_prelim %>% distinct(AU,TA,author_order,.keep_all=TRUE)
foo<-all_wos_authors_prelim %>% 
  filter(is.na(country)) %>% 
  group_by(SO,PY) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)*100) %>% 
  arrange(desc(freq))




all_wos_authors_prelim %>% count(AU,TA,author_order) %>% arrange(desc(n))

wos_authors<-all_wos_authors_prelim
# scopus refs -------------------------------------------------------------


scopus_authors<-read_csv("./bibliometrics/data_clean/scopus2_authors_prelim.csv") %>% 
  arrange(refID,author_count) %>% 
  mutate(country=tolower(country)) %>% 
  mutate(country = case_when(
    country ==  "united states" ~ "usa",
    country ==  "united kingdom"  ~ "uk",
    country ==  "russian federation"~"russia",
    TRUE ~ as.character(country))) %>%
  mutate(code = countrycode(country, origin = 'country.name', destination = 'iso3c')) %>%
  mutate(code = case_when(country ==  "curacao" ~ "CUW",
                          country ==  "bonaire" ~ "BES",
                          country ==  "us virgin islands" ~ "BES",
                          TRUE ~ as.character(code)))



# read in references files ------------------------------------------------

wos_unique_refs<-read_csv("./bibliometrics/data_clean/wos_unique_refs.csv") %>% 
  select(refID,DI,SO,PY) %>%
  mutate_all(as.character)

common_refs<-read_csv("./bibliometrics/data_clean/common_refs.csv") %>% 
  select(refID,DI,SO,PY)%>%
  mutate_all(as.character)
wos_DI<-bind_rows(wos_unique_refs,common_refs) %>% 
  distinct()
unique(wos_authors$code)
unique(scopus_authors$code)
names(wos_authors)
names(scopus_authors)
scopus_slim<-scopus_authors %>% select(refID,author_order=author_count,SO,PY,DI,university,city,country,code) %>% 
  mutate_all(as.character)
wos_slim <- wos_authors %>% select(refID,author_order,SO,PY,university,city,country,code)%>% 
  mutate_all(as.character) %>% 
  left_join(wos_DI)


# add decade --------------------------------------------------------------


all_refs_slim<-bind_rows(scopus_slim,wos_slim) %>% 
  mutate(PY=as.numeric(PY)) %>% 
  filter(PY>1960) %>% 
  filter(PY<2021)
hist(all_refs_slim$PY)





breaks <- seq(from=1961,to=2021,by=10)
# specify interval/bin labels
tags <- c("[61-70)","[71-80)", "[81-90)", "[91-20)", "[01-10)", "[11-21)")
# bucketing values into bins

group_tags <- cut(all_refs_slim$PY, 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE, 
                  labels=tags)


# inspect bins
summary(group_tags)

# georeference ------------------------------------------------------------


library(maps)
data("world.cities")
world.cities<-world.cities %>% 
  rename(city=name) %>% 
  mutate(city=tolower(city)) %>% 
  rename(country=country.etc) %>%
  mutate(country=tolower(country))


all_refs_slim<-all_refs_slim %>% mutate(decade = as.numeric(PY) - as.numeric(PY) %% 10)
hist(all_refs_slim$decade)
names(all_refs_slim)
names(world.cities)
world.cities<-as_tibble(world.cities)
head(world.cities)


all_refs_slim<-left_join(all_refs_slim,world.cities,by=c("city", "country"))
all_refs_slim<-all_refs_slim %>% relocate(lat,long,.before=1) %>% 
  arrange(lat,long)

all_refs_slim <-all_refs_slim %>%
  mutate(lat = case_when(city== "ithaca"~ "42.44",
                         city== "davis"~ "38.54",
                         city== "athens"~ "33.95",
                         city== "st paul"~ "44.95",
                         city== "ft collins"~ "40.59",
                         city== "ascot"~ "51.40",
                         TRUE ~ as.character(lat))) %>%
  mutate(long = case_when(city== "ithaca"~ "-76.50",
                          city== "davis"~ "-121.74",
                          city== "athens"~ "-83.36",
                          city== "st paul"~ "-93.09",
                          city== "ft collins"~ "-105.08",
                          city== "ascot"~ "-0.68",
                          city== "e lansing"~ "",
                          TRUE ~ as.character(long)))


all_authors_prelim_LL <-all_refs_slim %>% filter(!is.na(lat)) 

# which ones no lat/long? -------------------------------------------------

all_authors_prelim_no_LL <-all_refs_slim %>% filter(is.na(lat)) 
# all_authors_prelim_no_LL <- all_authors_prelim_no_LL %>% 
#   filter(SO!="rbt") %>% 
#   filter(SO!="trop_ecol")

names(all_authors_prelim_no_LL)
all_authors_prelim_no_LL %>% count(SO)
# unique city and country (to geocode)
city_country<-all_authors_prelim_no_LL %>% distinct(city,country,code)

# open cage setup ---------------------------------------------------------

library(opencage)
library(usethis)
# https://opencagedata.com/pricing#geocoding-onetime
oc_config(
  key = Sys.getenv("OPENCAGE_KEY"),
  # rate_sec = 1L,
  rate_sec = 15,
  no_record = TRUE
)

# oc_config(rate_sec = 15)

# open cage 1 ---------------------------------------------------------------


city_country
city_country<-city_country %>% 
  mutate(countrycode=countrycode(country,origin = 'country.name',destination='iso2c'))

unique(city_country$countrycode)
city_country_georef<-city_country %>% 
  unite(home,city,country,sep=",") %>% 
  drop_na(countrycode) %>% 
  oc_forward_df(placename = home, countrycode=countrycode)
write_csv(city_georef,"./bibliometrics/data_clean/city_georef.csv")


# keywords analysis -------------------------------------------------------



library(tidyverse)
library(stopwords)
library(ngram)
library(tidytext)
library(igraph)
library(tidystringdist)


wos_unique_refs<-read_csv("./bibliometrics/data_clean/wos_unique_refs.csv") %>% 
  select(refID,DI,SO,PY,TI,DE) %>%
  mutate_all(as.character) %>% 
  mutate(TI=gsub(" - "," ",TI)) %>% 
  mutate_all(as.character)

common_refs<-read_csv("./bibliometrics/data_clean/common_refs.csv") %>% 
  select(refID,DI,SO,PY,TI=`TI...9`,DE)%>%
  mutate_all(as.character) %>% 
  mutate(TI=gsub(" - "," ",TI))%>% 
  mutate_all(as.character)

scopus_papers<-read_csv("./bibliometrics/data_clean/scopus2_papers.csv") %>% 
  mutate(TI=gsub(" - "," ",TI)) %>% 
  select(refID,DI,SO,PY,TI,DE)%>% 
  mutate_all(as.character)

merged_refs<-bind_rows(scopus_papers,common_refs,wos_unique_refs) %>% 
  mutate(jrnl_cat = case_when(
    SO ==  "bitr"~"tropical",
    SO ==  "evol"~"global",
    SO ==  "jae"~"global",
    SO ==  "ecology"~"global",
    SO ==  "jecol"~"global",  
    SO ==  "jte"~"tropical",
    SO ==  "trop_ecol"~"tropical",
    SO ==  "amnat"~"global",
    SO ==  "rbt"~"tropical",
    SO ==  "tcs"~"tropical",
    TRUE ~ as.character(SO))) 



kw_refined<-read_csv("./keyword_analysis/kw_refined.csv") %>% 
  rename(kw=)

top_kw<-kw_refined %>% 
  group_by(kw) %>%
  tally() %>% 
  arrange(desc(n))
top_kw






kw<-kw_refined %>% filter(!is.na(jrnl_cat))
# https://stackoverflow.com/questions/66030942/tidytext-clustering
kw_cluster<-kw %>% 
  count(jrnl_cat, SO,kw_refined, sort = TRUE) %>%
  cast_sparse(SO, kw_refined, n)


class(kw_cluster)
dim(kw_cluster)

kfit <- kmeans(kw_cluster, centers = 4)

enframe(kfit$cluster, value = "cluster") %>%
  # separate(name, into = c("jrnl_type"), sep = "_") %>%
  count(name, cluster) %>%
  arrange(cluster)


# topic models ------------------------------------------------------------


# https://www.tidytextmining.com/topicmodeling.html
# library(topicmodels)
# data("AssociatedPress")
unique(kw_refined$SO)
kw<-kw_refined %>% filter(!is.na(jrnl_cat))


# kw_dtm<-kw %>% 
#   count(SO,kw_refined) %>% 
#   rename(document=SO,term=kw_refined, count=n) %>% 
#   cast_dtm(document,term,count)
kw_dtm<-kw %>% 
  count(jrnl_cat,kw_refined) %>% 
  rename(document=jrnl_cat,term=kw_refined, count=n) %>% 
  cast_dtm(document,term,count)

# set a seed so that the output of the model is predictable
kw_lda <- LDA(kw_dtm, k = 4, control = list(seed = 1234))
kw_lda

kw_topics <- tidy(kw_lda, matrix = "beta")
kw_topics

kw_top_terms <- kw_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)
kw_top_terms


kw_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()



beta_wide <- kw_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>% 
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_wide %>%
  slice(1:30) %>% 
  select(term,log_ratio) %>% 
  arrange(log_ratio) %>% 
  mutate(term = reorder(term,log_ratio)) %>%
  ggplot(aes(log_ratio, term)) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered()


kw_documents <- tidy(kw_lda, matrix = "gamma")
kw_documents

tidy(kw_dtm) %>%
  filter(document == "tropical") %>%
  arrange(desc(count))


tidy(kw_dtm) %>%
  filter(document == "global") %>%
  arrange(desc(count))


# kw wordclouds -----------------------------------------------------------


# wordclouds
library(wordcloud)
set.seed(1234) # for reproducibility 
wordcloud(
  words = kw_comp$all_non_trop_pubs, 
  # words = kw_trop$kw, 
  freq = kw_comp$n.x, 
  min.freq = 1,
  max.words=50, 
  random.order=FALSE, 
  # rot.per=0.35,
  colors=brewer.pal(8, "Dark2"))
# 
# 
# setdiff(kw_trop$kw,kw_not$kw)
# setdiff(kw_not$kw,kw_trop$kw)
# intersect(kw_trop$kw,kw_not$kw)


# analysis - title words --------------------------------------------------



# clean up the titles ----------------------------------------------------


merged_refs<-merged_refs %>% select(refID,jrnl_cat,SO,PY,TI) %>% 
  # drop_na(TI) %>% 
  mutate(TI=gsub(" - "," ",TI)) %>%
  mutate(TI=gsub(":","",TI)) %>% 
  mutate(TI=gsub(",","",TI)) %>% 
  mutate(TI=gsub(";","",TI)) %>% 
  mutate(TI=gsub("species diversity","species-diversity",TI)) %>% 
  mutate(TI=gsub("tropical forest","tropical-forest",TI)) %>% 
  mutate(TI=gsub("dry forest","dry-forest",TI)) %>% 
  mutate(TI=gsub("rain forest","rain-forest",TI)) %>% 
  mutate(TI=gsub("seed forest","seed-forest",TI))



# parse titles and clean --------------------------------------------------


tw<-merged_refs %>% 
  drop_na(TI) %>% 
  rename(tw=TI) %>%   
  separate(tw,c(LETTERS[seq( from = 1, to = 60 )]), sep = " ") %>% 
  pivot_longer(!refID:PY, names_to = "letter", values_to = "tw") %>% 
  select(-letter) %>% 
  drop_na(tw) %>% 
  mutate(tw=trimws(tw)) %>% 
  mutate(tw=gsub("\n"," ",tw)) %>% 
  filter(!(tw %in% stopwords(source = "snowball"))) %>%  # deletes the stopwords
  mutate(tw=tolower(tw))





tw<-merged_refs %>% select(refID,jrnl_cat,SO,PY,TI) %>% 
  
  rename(tw=TI) %>% 
  mutate(tw=gsub(" - "," ",tw)) %>% 
  separate(tw,c(LETTERS[seq( from = 1, to = 60 )]), sep = " ") %>% 
  pivot_longer(!refID:PY, names_to = "letter", values_to = "tw") %>% 
  select(-letter) %>% 
  drop_na(tw) %>% 
  mutate(tw=trimws(tw)) %>% 
  filter(!(tw %in% stopwords(source = "snowball"))) %>%  # deletes the stopwords
  mutate(tw=tolower(tw)) %>% 
  mutate(tw=gsub("\n"," ",tw))



pubs_with_kw_tw<-merged_refs %>% select(refID,jrnl_cat,SO,PY,TI,DE) %>% 
  drop_na(TI,DE) %>% 
  rename(tw=TI) %>% 
  rename(kw=DE)

tw_both<-pubs_with_kw_tw %>% 
  select(-kw) %>% 
  mutate(tw=gsub(":","",tw)) %>% 
  mutate(tw=gsub(",","",tw)) %>% 
  mutate(tw=gsub(";","",tw)) %>% 
  mutate(tw=gsub("species diversity","species-diversity",tw)) %>% 
  mutate(tw=gsub("tropical forest","tropical-forest",tw)) %>% 
  mutate(tw=gsub("dry forest","dry-forest",tw)) %>% 
  mutate(tw=gsub("rain forest","rain-forest",tw)) %>% 
  mutate(tw=gsub("seed forest","seed-forest",tw)) %>% 
  separate(tw,c(LETTERS[seq( from = 1, to = 60 )]), sep = " ") %>% 
  pivot_longer(!refID:PY, names_to = "letter", values_to = "tw") %>% 
  select(-letter) %>% 
  drop_na(tw) %>% 
  mutate(tw=trimws(tw)) %>% 
  mutate(tw=gsub("\n"," ",tw)) %>% 
  filter(!(tw %in% stopwords(source = "snowball"))) %>%  # deletes the stopwords
  mutate(tw=tolower(tw))
# tw_both$tw<-gsub("\n"," ",tw_both$tw)

kw_both<-pubs_with_kw_tw %>% 
  select(-tw) %>% 
  separate(kw,c(LETTERS[seq( from = 1, to = 20 )]), sep = ";") %>% 
  pivot_longer(!refID:PY, names_to = "letter", values_to = "kw") %>% 
  select(-letter) %>% 
  drop_na(kw) %>% 
  mutate(kw=trimws(kw)) %>% 
  mutate(kw=gsub("\n"," ",kw)) %>% 
  mutate(kw=tolower(kw))
# kw_both$kw<-gsub("\n"," ",kw_both$kw)


# together<-full_join(tw_both,kw_both)
# no_kw<-together %>% filter(is.na(kw))
# both<-together %>% filter(is.na(kw))
# summary(together$tw==together$kw)
# 
# kw$kw<-gsub("\n"," ",kw$kw)

# unique(together$kw)
# unique(together$tw)
kw_bitr<-kw_both %>% 
  filter(SO=="bitr") %>% 
  drop_na(kw) %>% 
  group_by(jrnl_cat,kw) %>%
  tally() %>% 
  arrange(desc(n))
kw_bitr

tw_bitr<-tw_both %>% 
  filter(SO=="bitr") %>% 
  drop_na(tw) %>% 
  group_by(jrnl_cat,tw) %>%
  tally() %>% 
  arrange(desc(n))
tw_bitr

kw_bitr
tw_bitr

kw_bitr_2join<-kw_both %>% 
  # filter(SO=="bitr") %>% 
  drop_na(kw) %>% 
  rename(tw_kw=kw)

tw_bitr_2join<-tw_both %>% 
  # filter(SO=="bitr") %>% 
  drop_na(tw) %>% 
  rename(tw_kw=tw)

joint_tw_kw<-bind_rows(kw_bitr_2join,tw_bitr_2join) 

joint_tw_kw_global<-joint_tw_kw %>% 
  filter(jrnl_cat=="global") %>% 
  group_by(tw_kw) %>%
  tally() %>% 
  arrange(desc(n))
joint_tw_kw_tropical<-joint_tw_kw %>% 
  filter(jrnl_cat=="tropical") %>% 
  group_by(tw_kw) %>%
  tally() %>% 
  arrange(desc(n))
joint_tw_kw_global
joint_tw_kw_tropical


joint_tw_kw




top_kw_trop<-kw_refined %>%
  filter(jrnl_cat=="tropical") %>% 
  group_by(kw) %>%
  tally() %>% 
  arrange(desc(n))
top_kw_trop<-top_kw_trop %>% slice(1:30)

top_kw_global<-kw_refined %>%
  filter(jrnl_cat=="global") %>% 
  group_by(kw) %>%
  tally() %>% 
  arrange(desc(n))
top_kw_global<-top_kw_global %>% slice(1:30)


# top ttitle words --------------------------------------------------------


top_tw<-tw %>% 
  group_by(tw) %>%
  tally() %>% 
  arrange(desc(n))
top_tw

top_tw_trop<-tw %>%
  filter(jrnl_cat=="tropical") %>% 
  group_by(tw) %>%
  tally() %>% 
  arrange(desc(n))


top_tw_global<-tw %>%
  filter(jrnl_cat=="global") %>% 
  group_by(tw) %>%
  tally() %>% 
  arrange(desc(n))
top_tw_global




# attempte to parse ngrams from title (not sep by ; like kw are) ----------
# https://www.tidytextmining.com/ngrams.html
ngram_data<-merged_refs %>% filter(jrnl_cat=="tropical")
ngram_data<-merged_refs %>% filter(jrnl_cat=="global")
ngram_data<-merged_refs

tw<-ngram_data %>% select(refID,jrnl_cat,SO,PY,TI) %>% 
  drop_na(TI) %>% 
  rename(tw=TI) %>% 
  mutate(tw=gsub(" - "," ",tw)) 

tw_bigrams <-tw %>% 
  select(tw) %>% 
  # slice(1:10) %>% 
  unnest_tokens(bigram, tw, token = "ngrams", n = 2) %>% 
  count(bigram, sort = TRUE) 
bigrams_separated <- tw_bigrams %>% 
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated  %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  arrange(desc(n)) %>% 
  slice(1:1000)
bigrams_filtered %>% slice(1:20)

tw_trigrams <-tw %>% 
  select(tw) %>% 
  # slice(1:10) %>% 
  unnest_tokens(trigram, tw, token = "ngrams", n = 3) %>% 
  count(trigram, sort = TRUE) 
trigrams_separated <- tw_trigrams %>% 
  separate(trigram, c("word1", "word2","word3"), sep = " ")
trigrams_filtered <- trigrams_separated  %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word3 %in% stop_words$word) %>% 
  arrange(desc(n)) %>% 
  slice(1:1000)
trigrams_filtered %>% slice(1:20)


tw_fourgrams <-tw %>% 
  select(tw) %>% 
  # slice(1:10) %>% 
  unnest_tokens(fourgram, tw, token = "ngrams", n = 4) %>% 
  count(fourgram, sort = TRUE) 
fourgrams_separated <- tw_fourgrams %>% 
  separate(fourgram, c("word1", "word2","word3","word4"), sep = " ")
fourgrams_filtered <- fourgrams_separated  %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word3 %in% stop_words$word) %>% 
  filter(!word4 %in% stop_words$word) %>% 
  arrange(desc(n)) %>% 
  slice(1:1000)
fourgrams_filtered

tw_fivegrams <-tw %>% 
  select(tw) %>% 
  # slice(1:10) %>% 
  unnest_tokens(fivegram, tw, token = "ngrams", n = 5) %>% 
  count(fivegram, sort = TRUE) 
fivegrams_separated <- tw_fivegrams %>% 
  separate(fivegram, c("word1", "word2","word3","word4","word5"), sep = " ")
fivegrams_filtered <- fivegrams_separated  %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word3 %in% stop_words$word) %>% 
  filter(!word4 %in% stop_words$word) %>% 
  filter(!word5 %in% stop_words$word) %>% 
  arrange(desc(n)) %>% 
  slice(1:1000)
fivegrams_filtered


tw_sixgrams <-tw %>% 
  select(tw) %>% 
  # slice(1:10) %>% 
  unnest_tokens(sixgram, tw, token = "ngrams", n = 6) %>% 
  count(sixgram, sort = TRUE) 
sixgrams_separated <- tw_sixgrams %>% 
  separate(sixgram, c("word1", "word2","word3","word4","word5","word6"), sep = " ")
sixgrams_filtered <- sixgrams_separated  %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word3 %in% stop_words$word) %>% 
  filter(!word4 %in% stop_words$word) %>% 
  filter(!word5 %in% stop_words$word) %>% 
  filter(!word6 %in% stop_words$word) %>% 
  arrange(desc(n)) %>% 
  slice(1:1000)
sixgrams_filtered




bigram_graph <- bigrams_filtered %>%
  filter(n > 40) %>%
  graph_from_data_frame()


library(ggraph)
set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

set.seed(2020)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


























all_refs_slim %>% distinct(refID) %>% nrow()


all_refs_slim %>% nrow()










breaks <- seq(from=1961,to=2021,by=10)
# specify interval/bin labels
tags <- c("[61-70)","[71-80)", "[81-90)", "[91-20)", "[01-10)", "[11-21)")
# bucketing values into bins

group_tags <- cut(all_authors_prelim$PY, 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE, 
                  labels=tags)


# inspect bins
summary(group_tags)

library(maps)
data("world.cities")
world.cities<-world.cities %>% 
  rename(city=name) %>% 
  mutate(city=tolower(city)) %>% 
  rename(country=country.etc) %>%
  mutate(country=tolower(country))


# tropical articles in ecoecvo journals  ----------------------------------

# This will allow you to exclude the ones that are tropical from the eco evo
# journals for comparison with the tropical journals

ecoevo_trop_DI<-read_csv("./bibliometrics/data_clean/ecoevo_trop.csv")  %>%
  mutate_all(as.character) %>% 
  mutate_all(tolower) %>% 
  mutate_all(trimws) %>% 
  select(DI,PY,SO) %>% 
  drop_na() %>% 
  # mutate(PY=as.numeric(PY)) %>% 
  mutate(SO = case_when(
    SO ==  "biotropica" ~ "bitr",
    SO ==  "evolution" ~ "evol",
    SO ==  "journal of animal ecology" ~ "jae",
    SO ==  "ecology" ~ "ecology",
    SO ==  "journal of ecology" ~ "jecol",  
    SO ==  "journal of applied ecology" ~ "jappecol",  
    SO ==  "journal of tropical ecology" ~ "jte",
    SO ==  "tropical ecology" ~ "trop_ecol",
    SO ==  "american naturalist" ~ "amnat",
    SO ==  "revista de biologia tropical" ~ "rbt",
    SO ==  "tropical conservation science" ~ "tcs",
    TRUE ~ as.character(SO))) %>% 
  mutate(pub_cat="tropical") %>% 
  tibble()






# mapping -----------------------------------------------------------------


georef<-read_csv("./bibliometrics/data_clean/FINAL_AUTHORS_prelim_georef.csv")


