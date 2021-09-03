# 'apt-get install libudunits2-dev' needs to be installed in the terminal
# for sf and tigris
install.packages("tigris")
install.packages("sf")
library(sf)
library(ggplot2)
library(dplyr)
library(pht)
library(viridis)
library(DBI)
library(odbc)
library(tigris)
library(getPass)
library(scales)
# Load geodata -----------------------------------------------------------------

source("geo.R")

shapes <- get_geo()

county_shapes <- shapes[[1]] %>%
  mutate(STCNTY = paste0(STATEFP, COUNTYFP)) %>%
  filter(!(STUSPS %in% c("AK", "HI")))

state_shapes <- shapes[[2]] %>%
  filter(!(STUSPS %in% c("AK", "HI")))

# Load from EDW ----------------------------------------------------------------

# Connect to EDW and query for data

conn <- DBI::dbConnect(
  odbc::odbc(),
  dsn = "EDWP_USER",
  uid = getPass::getPass("CN_NUMBER"),
  pwd = getPass::getPass("Password")
)

sql_file <- "Auth_Heatmap.sql"

raw_data <-
  readChar(sql_file, file.info(sql_file)$size) %>%
  DBI::dbGetQuery(conn, .)

DBI::dbDisconnect(conn)

# Filter raw data to only be in state residents
#raw_data<-raw_data %>% filter(County_State=="NC")

map_data <- county_shapes %>%
  left_join(raw_data, by = c("STCNTY" = "COUNTY_FIPS"))


# Make Maps --------------------------------------------------------------------

mod_theme <-
  theme_minimal() +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        panel.grid.major = element_line(colour = "transparent"), 
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#FFFFFF", color = NA),
        panel.background = element_rect(fill = "#FFFFFF", color = NA),
        legend.background = element_rect(fill = "#FFFFFF", color = NA),
        panel.border = element_blank(), 
        legend.position = "bottom",
        plot.caption = element_text(hjust = 0))


# AGG_CNT - How many auths did we get since jun 14th
AGG_CNT<-map_data
AGG_CNT$AGG_CNT<- ifelse(AGG_CNT$Mem_Count <500,NA, AGG_CNT$AGG_CNT)
AGG_CNT$AGG_CNT <- ifelse(AGG_CNT$AGG_CNT == 0, NA, AGG_CNT$AGG_CNT)
AGG_CNT$AGG_CNT_log <- log10(AGG_CNT$AGG_CNT)


# Agg_per_mem - How many auths did we get per member since jun 14th
agg_per_mem<-map_data
agg_per_mem$AGG_PER_MEM<- ifelse(agg_per_mem$Mem_Count <500,NA, agg_per_mem$AGG_PER_MEM)
agg_per_mem$AGG_PER_MEM <- ifelse(agg_per_mem$AGG_PER_MEM == 0, NA, agg_per_mem$AGG_PER_MEM)

#MOM_CHG_PCT
MOM_CHG_PCT<-map_data
MOM_CHG_PCT$MoM_CHG_PCT<- ifelse(MOM_CHG_PCT$Mem_Count <100,NA, MOM_CHG_PCT$MoM_CHG_PCT)
MOM_CHG_PCT$MoM_CHG_PCT <- ifelse(MOM_CHG_PCT$MoM_CHG_PCT == 0, NA, MOM_CHG_PCT$MoM_CHG_PCT)

#MOM_14DAvg_CHG_PCT
MOM_14DAvg_CHG_PCT<-map_data
MOM_14DAvg_CHG_PCT$MoM_14DAvg_CHG_PCT<- ifelse(MOM_14DAvg_CHG_PCT$Mem_Count <100,NA, MOM_14DAvg_CHG_PCT$MoM_CHG_PCT)
MOM_14DAvg_CHG_PCT$MoM_14DAvg_CHG_PCT <- ifelse(MOM_14DAvg_CHG_PCT$MoM_CHG_PCT == 0, NA, MOM_14DAvg_CHG_PCT$MoM_CHG_PCT)

#End_Beg_CHG_PCT -Change between last two weeks of jun vs first two weeks of July
End_Beg_CHG_PCT<-map_data
End_Beg_CHG_PCT$End_Beg_CHG_PCT<- ifelse(End_Beg_CHG_PCT$Mem_Count <100,NA, End_Beg_CHG_PCT$MoM_CHG_PCT)
End_Beg_CHG_PCT$End_Beg_CHG_PCT <- ifelse(End_Beg_CHG_PCT$End_Beg_CHG_PCT <= 0, NA, End_Beg_CHG_PCT$End_Beg_CHG_PCT)
End_Beg_CHG_PCT$End_Beg_CHG_PCT_log <-log(End_Beg_CHG_PCT$End_Beg_CHG_PCT)
#End_Beg_CHG_PCT_PER_MEM -Change between last two weeks of jun vs first two weeks of July per mem
End_Beg_CHG_PCT_PER_MEM<-map_data
End_Beg_CHG_PCT_PER_MEM$End_Beg_CHG_PCT_PER_MEM<- ifelse(End_Beg_CHG_PCT_PER_MEM$Mem_Count <100,NA, End_Beg_CHG_PCT_PER_MEM$End_Beg_CHG_PCT_PER_MEM)
End_Beg_CHG_PCT_PER_MEM$End_Beg_CHG_PCT_PER_MEM <- ifelse(End_Beg_CHG_PCT_PER_MEM$End_Beg_CHG_PCT_PER_MEM <= 0, NA, End_Beg_CHG_PCT_PER_MEM$End_Beg_CHG_PCT_PER_MEM)
End_Beg_CHG_PCT_PER_MEM$End_Beg_CHG_PCT_PER_MEM_log <-log(End_Beg_CHG_PCT_PER_MEM$End_Beg_CHG_PCT_PER_MEM)

ggplot(agg_per_mem) +
  geom_sf(aes(fill = AGG_PER_MEM),
          size = 0.05) +
  geom_sf(data = state_shapes, size = 0.1, fill = NA) +
  pht::ph_map_theme(4) +
  scale_fill_distiller(guide = guide_colorbar("IP Auths Per Member",
                                              title.position = "top",
                                              barwidth = 15,
                                              barheight = .7,
                                              ticks.colour = "#222222"),
      
                       labels = pht::ph_fnum,
                       #labels = function(x) round(10**x,0),
                       palette = "OrRd",
                       direction = 1,
                       na.value = "white",
                       #breaks =  log(c(.01, .1, 1, 2, 4, 8))
                       ) +
  labs(title = "COVID-19 Heatmap - Recent IP Auths",
       subtitle = "Since June 14th, where are the auths concentrated?",
       caption = "Counties in white have not had an increase in auths or have less than 500 Members")+
mod_theme

ggsave("img/covid_auth_per_mem.png")

#create log value of mapped value
# exponentiation on trans
#exp
#function(x) pht::ph_fperc(exp(x))
#breaks = c(1, 2, 3, 4)


