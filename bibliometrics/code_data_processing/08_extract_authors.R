library(tidyverse)
library(refsplitr)
library(countrycode)

# references that were both in the SCOPUs and WOS searches ----------------



# load "wos_refs_raw": original search after processed by refernces_read() ---
common_refs<-read_csv("./biblio2/data_clean/common_refs.csv") %>% 
  filter(SO!="rbt") %>% 
  rename(TI=`TI...9`) %>% 
  rename(FU=`FU...59`) %>% 
  rename(PM=`PM...62`)

names(common_refs)

head(common_refs)

# common_refs<-common_refs %>% slice(1:8000)
# common_refs2<-common_refs %>% slice(10000:16522)
# common_refs2<-common_refs %>% slice(12000:12000)
common_refs_authors<-authors_clean(common_refs)
common_refs_authors_prelim<-common_refs_authors$prelim
common_refs_authors_review<-common_refs_authors$review

write_rds(common_refs_authors,"./biblio2/data_clean/common_refs_authors.rds")
write_csv(common_refs_authors_prelim,"./biblio2/data_clean/common_refs_authors_prelim.csv")
write_csv(common_refs_authors_review,"./biblio2/data_clean/common_refs_authors_review.csv")


# unique to WOS -----------------------------------------------------------

wos_refs<-read_csv("./biblio2/data_clean/wos_unique_refs.csv") %>% 
  mutate(SO = case_when(
    SO ==  "BIOTROPICA" ~ "bitr",
    SO ==  "Biotropica" ~ "bitr",
    SO ==  "EVOLUTION" ~ "evol",
    SO ==  "JOURNAL OF ANIMAL ECOLOGY" ~ "jae",
    SO ==  "ECOLOGY" ~ "ecology",
    SO ==  "JOURNAL OF ECOLOGY" ~ "jecol",
    SO ==  "JOURNAL OF TROPICAL ECOLOGY" ~ "jte",
    SO ==  "TROPICAL ECOLOGY" ~ "trop_ecol",
    SO ==  "AMERICAN NATURALIST" ~ "amnat",
    SO ==  "Revista de Biología Tropical" ~ "rbt",
    SO ==  "TROPICAL CONSERVATION SCIENCE" ~ "tcs",
    SO ==  "REVISTA DE BIOLOGIA TROPICAL" ~ "rbt",
    TRUE ~ as.character(SO))) %>%
  filter(SO !="CURRENT SCIENCE") %>%
  filter(SO !="current science") %>%
  filter(SO!="rbt") %>% 
  filter(DI!="10.15517/rbt.v64i4.21219") # this one makes authors clean throw an error 
  
  unique(wos_refs$SO)
  

# wos_ref<-wos_refs %>% slice(1:15000)
# wos_ref15_35K<-wos_refs %>% slice(15000:35000)
  wos_ref_35_end<-wos_refs %>% slice(35001:46451)
# wos_ref<-wos_refs %>% slice(47000:48000)
# wos_ref<-wos_refs %>% slice(47764:47764)

wos_refs_authors<-authors_clean(wos_refs)
wos_refs_authors_prelim<-wos_refs_authors1_15K$prelim
wos_refs_authors_review<-wos_refs_authors1_15K$review
head(wos_refs_authors)
write_rds(wos_refs_authors,"./biblio2/data_clean/wos_refs_authors1_15K.rds")
write_csv(wos_refs_authors_prelim,"./biblio2/data_clean/wos_refs_authors1_15K_prelim.csv")
write_csv(wos_refs_authors_review,"./biblio2/data_clean/wos_refs_authors1_15K_review.csv")




wos_refs_authors_15_35K<-authors_clean(wos_ref15_35K)
wos_refs_authors_15_35_prelim<-wos_refs_authors_15_35K$prelim
wos_refs_authors_15_35_review<-wos_refs_authors_15_35K$review
head(wos_refs_authors_15_35_prelim)
write_rds(wos_refs_authors_15_35K,"./biblio2/data_clean/wos_refs_authors_15_35_prelim.rds")
write_csv(wos_refs_authors_15_35_prelim,"./biblio2/data_clean/wos_refs_authors_15_35_prelim.csv")
write_csv(wos_refs_authors_15_35_review,"./biblio2/data_clean/wos_refs_authors_15_35_review.csv")






wos_ref_35_end_authors<-authors_clean(wos_ref_35_end)
wos_ref_35_end_authors_prelim<-wos_ref_35_end_authors$prelim
wos_ref_35_end_authors_review<-wos_ref_35_end_authors$review

write_rds(wos_ref_35_end_authors,"./biblio2/data_clean/wos_refs_35_end_authors.rds")
write_csv(wos_ref_35_end_authors_prelim,"./biblio2/data_clean/wos_refs_35_end_authors_prelim.csv")
write_csv(wos_ref_35_end_authors_review,"./biblio2/data_clean/wos_refs_35_end_authors_review.csv")



# bind up the wos unique refs ----------------------------------------------------



wos_refs_authors_prelim<-read_csv("./biblio2/data_clean/wos_refs_authors1_15K_prelim.csv")
wos_refs_authors_15_35_prelim<-read_csv("./biblio2/data_clean/wos_refs_authors_15_35_prelim.csv")
wos_ref_35_end_authors_prelim<-read_csv("./biblio2/data_clean/wos_refs_35_end_authors_prelim.csv")



wos_unique_authors_prelim<-bind_rows(wos_refs_authors_prelim,
                    wos_refs_authors_15_35_prelim, 
                    wos_ref_35_end_authors_prelim)
write_csv(wos_unique_authors_prelim,"./biblio2/data_clean/wos_refs_35_end_authors_prelim.csv")



unique(wos_unique_authors_prelim$country)

# bind togwther unique to WOS and both SCOPUS/WOS--------------------------

common_refs_authors_prelim<-read_csv("./biblio2/data_clean/common_refs_authors_prelim.csv") 
wos_unique_authors_prelim<-read_csv("./biblio2/data_clean/wos_refs_35_end_authors_prelim.csv")%>% 
  mutate(refID=as.character(refID))
all_wos_authors_prelim<-bind_rows(wos_unique_authors_prelim, 
                                  common_refs_authors_prelim)

unique(all_wos_authors_prelim$country)

all_wos_authors_prelim<-all_wos_authors_prelim %>% 
  mutate(country = case_when(
    country ==  "edo venezuela" ~ "venezuela",
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
  mutate(code = case_when(country ==  "curacao" ~ "CUW",
                          country ==  "bonaire" ~ "BES",
                          country ==  "us virgin islands" ~ "BES",
                          TRUE ~ as.character(code)))


unique(all_wos_authors_prelim$country)



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