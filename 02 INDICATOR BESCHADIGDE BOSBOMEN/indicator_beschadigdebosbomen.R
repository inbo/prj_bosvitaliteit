library(tidyverse)
library(mgcv)
library(INBOtheme)
if (!('remotes' %in% rownames(installed.packages() ))) install.packages('remotes')
if (!('git2rdata' %in% rownames(installed.packages() ))) remotes::install_github('inbo/git2rdata')

### >>> Variabelen

### Data
jaren <- 1987:2021  #!!!! ieder jaar wijzigen


server   <- "inbo-sql07-prd.inbo.be"
database <- "D0004_00_Bosvitaliteit"

### Figuurlayout
theme_set(theme_inbo(10))
theme_inbo <- theme_update(axis.title.x = element_text(colour = "black"), #was inbo.hoofd of #843860
                           axis.title.y = element_text(colour = "black"), #was inbo.hoofd of #843860
                           plot.title = element_text(colour = "black"),
                           legend.key = element_rect(fill = "white"))
update_geom_defaults("point", aes(size = 2))
update_geom_defaults("line", aes(size = 0.25))
update_geom_defaults("point", aes(size = 1))
update_geom_defaults("line", aes(size = 0.25))
fig.width <- 147 / 25.4
fig.height <- 103 / 25.4


### >>> Query

natuurindicatoren_sql <- paste(
  "select
  p.PLOT_NUM as PlotNr
  , b.BOOM_BNR as BoomNr
  , w.WRNG_JAA as Jaar
  , s.SPEC_EUR_CDE as Soortnummer
  , m.WRME_OMT as Omtrek
  , m.WRME_LFT as Leeftijd
  , m.WRME_UCBL_CDE as BladverliesNetto

  from
  tblProefvlak p left join
  tblWaarneming w on w.WRNG_PLOT_ID = p.PLOT_ID left join
  tblWaarnemingMeting m on m.WRME_WRNG_ID = w.WRNG_ID left join
  tblBoom b on b.BOOM_ID = m.WRME_BOOM_ID left join
  tblSoort s on s.SPEC_ID = b.BOOM_SPEC_ID",
  paste0(" \nwhere w.WRNG_JAA in (", paste(jaren, collapse = ","), ")")
)


### >>> Data ophalen

conn <- DBI::dbConnect(odbc::odbc(),
                       Driver = "SQL Server",
                       Server = server,
                       Database = database,
                       Trusted_Connection = "True")

dfNatuurindicatoren <- DBI::dbGetQuery(conn, natuurindicatoren_sql)
save(dfNatuurindicatoren, file = "dfNatuurindicatoren.Rdata")






### >>> Data transformaties

dfNI <- dfNatuurindicatoren %>%
  mutate(BladverliesNetto = as.numeric(BladverliesNetto),
         Beschadigd = BladverliesNetto > 25,
         jaar = Jaar,
         Jaar = NULL) %>%
  group_by(jaar)

dfNIS <- dfNI %>%
  summarize(beschadigd = sum(Beschadigd, na.rm = TRUE),
            gezond = sum(!Beschadigd, na.rm = TRUE),
            totaal = n()) %>%
  mutate(schade_pct = beschadigd / totaal * 100)
#write_delim(dfNIS, path = "beschadigde_bosbomen.tsv", delim = "\t") #oude methode
git2rdata::write_vc(dfNIS, file = "beschadigde_bosbomen.tsv", sorting = "jaar")

############################

vlaanderen_europa <- read.delim("vlaanderen_europa_temp.txt", sep = "\t")
vlaanderen_europa$niveau <- factor(vlaanderen_europa$niveau)
git2rdata::write_vc(vlaanderen_europa, file = "vlaanderen_europa.tsv", sorting = c("Jaar", "niveau"), optimize = FALSE)


##################################################################################

### >>> Indicatormodel (NIET MEER NODIG OM TE GEBRUIKEN, enkel ter illustratie ---> officiele berekning zit in het indicatoren repository, enkel dfNIS doorgeven)

jni <- jaren

modelIndicator <- gam(schade_pct ~ s(jaar, k=5), data = dfNIS)
newdata <- data.frame(jaar = c(jni, jni[length(jni)] + 1:4))
newdata$future <- ifelse(newdata$jaar > jni[length(jni)], "Unobserved", "Observed")
newdata$predict <- predict(modelIndicator, newdata = newdata)
plot(modelIndicator)
preds <- predict(modelIndicator, newdata, type = "link", se.fit = TRUE)
newdata$lwr <- preds$fit - 1.96 * preds$se.fit
newdata$upr <- preds$fit + 1.96 * preds$se.fit
newdata$fit <- preds$fit


### >>> Plotdata

DataFig <- data.frame(jaar = dfNIS$jaar, waarde = dfNIS$schade_pct)
DataFig <- right_join(DataFig, newdata[c("jaar", "fit", "lwr", "upr")], by = c("jaar" = "jaar"))
DataFig$spreiding <- TRUE
DataFig$type <-  "Trend"
data <- subset(DataFig, jaar <= jni[length(jni)])
data$eenheid <- "beschadigde bosbomen (%)"
IND <- "Aandeel beschadigde bosbomen "
Sub_ID <- ""


### >>> Plot

###PNG

p <- ggplot(data, aes(x = jaar, y = fit, ymin = lwr, ymax = upr, linetype = type)) + ylab("beschadigde bosbomen (%)") +
  geom_ribbon(alpha = 0.1, aes(fill = spreiding)) +
  geom_line() +
  geom_point(aes(y = waarde)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.text.y = element_text(color = "black")) +
  scale_y_continuous(paste(unique(data$eenheid))) +
  scale_linetype_discrete(guide = guide_legend(title = NULL,
                                               override.aes = list(fill = NA),
                                               label.theme = element_text(color = "black", size = 10))) +
  scale_fill_manual(na.translate = FALSE,
                    labels = c("onzekerheid\nop de trend"),
                    values = c("TRUE" = inbo_steun_blauw),
                    guide = guide_legend(title = NULL,
                                         label.theme = element_text(color = "black", size = 10)))  +
  ggtitle(paste(IND, Sub_ID, sep = "\n"))

ggsave(p, width = fig.width, height = fig.height,
       file = paste0("",
                     paste(gsub(" ", "",
                                paste(IND, Sub_ID, sep = "__")), "png", sep = ".")))

print(p)

###EPS

p <- ggplot(data, aes(x = jaar, y = fit, ymin = lwr, ymax = upr, linetype = type)) + ylab("beschadigde bosbomen (%)") +
  geom_ribbon(alpha = 1, aes(fill = spreiding)) +
  geom_line() +
  geom_point(aes(y = waarde)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.text.y = element_text(color = "black")) +
  scale_y_continuous(paste(unique(data$eenheid))) +
  scale_linetype_discrete(guide = guide_legend(title = NULL,
                                               override.aes = list(fill = NA),
                                               label.theme = element_text(color = "black", size = 10))) +
  scale_fill_manual(na.translate = FALSE, labels = c("onzekerheid\nop de trend"),
                    values = c("TRUE" = inbo_hoofd),
                    guide = guide_legend(title = NULL,
                                         label.theme = element_text(color = "black", size = 10)))  +
  ggtitle(paste(IND, Sub_ID, sep = "\n"))

print(p)

ggsave(p, width = fig.width, height = fig.height,
       file = paste0("",
                     paste(gsub(" ", "",
                                paste(IND, Sub_ID, sep = "__")), "eps", sep = ".")))

write.csv2(file = "Meetpunten indicator.csv", DataFig)

### PLOT vergelijking met Europa

p <- ggplot(vlaanderen_europa, aes(x = Jaar, y = Aandeel, color = niveau)) +
  geom_path() + geom_point()

print(p)

