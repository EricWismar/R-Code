library(getPass)
library(DBI)
library(odbc)
library(dplyr)
library(ggplot2)

# Load from EDW ----------------------------------------------------------------

# Connect to EDW and query for data

uid <-getPass::getPass("CN_NUMBER")
pwd<-getPass::getPass("Password")

conn <- DBI::dbConnect(
  odbc::odbc(),
  dsn = "EDWP_USER",
  uid = uid,
  pwd = pwd
)

sql_file <- "Vax_Hesitancy_Dose_Per_Mem.sql"

raw_data <-
  readChar(sql_file, file.info(sql_file)$size) %>%
  DBI::dbGetQuery(conn, .)

DBI::dbDisconnect(conn)

regions_df<- read.csv("Regions.csv")
regions_df = subset(regions_df, select = -c(X) )

df<-inner_join(raw_data, regions_df, by = c("state"="State.Code"), copy = FALSE)



df[1:1000,]%>%
  arrange(desc(member_count))%>%
  mutate(County=factor(County,County))%>%
  ggplot(aes(x=EST_HESITANT,y=Dose_Per_Mem,size=member_count, color=Regions))+
  geom_point(alpha=0.5)+
  scale_size(range=c(.1,24), name="Member Count")+
  scale_color_manual(values = c("Coastal West & Sun Belt" = "#C0504D", "East & Northeast" = "#4F81BD", "Midwest & Mountain West"="#9BBB59"))+
  ggtitle("Doses Per Member by County Hesitancy", subtitle = "PDP")+ 
  xlab("Estimated County Hesitancy")+
  ylab("Doses/Member")+
  theme(panel.background = element_rect(fill='white', colour='grey'))+
  scale_x_continuous(labels = scales::percent)+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.subtitle = element_text(hjust = 0.5))+
  ylim(0.0,.70)+xlim(0.07,.32)
