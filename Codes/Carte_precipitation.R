#Code source------
library(RColorBrewer)
library(latex2exp)
library(ggplot2)
library(readxl)
#Importation des données
df <- read_excel("C:/Users/User/Desktop/Projet CARP/Données en fonction des maxima des precipitations/Matrice des paramètres.xlsx", 
                 sheet = "Précipitation")
#View(df)
attach(df)

# Premier code ---------
precip  <- data.frame(
  Latitude = rep(LAT, 24),
  Longitude = rep(LON, 24),
  val_precip = c(df$`2023`,df$`2022`,df$`2021`,df$`2020`,df$`2019`,df$`2018`,df$`2017`,df$`2016`,df$`2015`,
                 df$`2014`,df$`2013`,df$`2012`,df$`2011`,df$`2010`,df$`2009`,df$`2008`,df$`2007`,df$`2006`,
                 df$`2005`,df$`2004`,df$`2003`,df$`2002`,df$`2001`,df$`2000`) ,
  Year  = sort(c(rep(2000,length(LAT)), rep(2001,length(LAT)),  rep(2002,length(LAT)), rep(2003,length(LAT)),  
                 rep(2004,length(LAT)), rep(2005,length(LAT)),  rep(2006,length(LAT)), rep(2007,length(LAT)),  
                 rep(2008,length(LAT)), rep(2009,length(LAT)),  rep(2010,length(LAT)), rep(2011,length(LAT)),  
                 rep(2012,length(LAT)), rep(2013,length(LAT)),  rep(2014,length(LAT)), rep(2015,length(LAT)),    
                 rep(2016,length(LAT)), rep(2017,length(LAT)),  rep(2018,length(LAT)), rep(2019,length(LAT)),  
                 rep(2020,length(LAT)), rep(2021,length(LAT)),  rep(2022,length(LAT)), rep(2023,length(LAT))), T)
)

attach(precip)

annees = colnames(df[,-c(1, 2, 3)])


ggplot() +
  # Couleur des frontiÃ¨res
  geom_point(data = precip, aes(x = Longitude, y = Latitude, color =  val_precip),
             size = 5, stat = "identity" #, shape = "square"
  ) + # Points colorÃ©s par tempÃ©rature
  #scale_color_gradient(low = c("darkblue","lightblue"), high = c("red")) + # DÃ©gradÃ© de couleur
  facet_wrap(~ Year) +
  scale_color_gradient(low = c("lightblue","darkblue", "green"), high = c("yellow", "orange", "red")) + # Dégradé de couleur
  
  #scale_color_gradient(low = c("darkblue","lightblue", "green"), high = c("yellow", "orange", "red")) + # DÃ©gradÃ© de couleur
  #scale_fill_viridis_d(option = "A")+
  
  scale_x_continuous(name = "Longitude", 
                     breaks = seq(-17, 11, by = 6),
                     labels = function(x) paste0(abs(x), ifelse(x < 0, "°W", "°E"))) + # Ajout de W ou E
  scale_y_continuous(name = "Latitude", 
                     breaks = seq(4, 20, by = 4),
                     labels = function(y) paste0(y, "°N")) +
  
  labs(title = "Carte des précipitations maximales annuelles", 
       #  x = "Longitude",
       #    y = "Latitude",
       color = "Values (mm/jr)")+
  theme_minimal() +
  
  borders("world", regions = c("Benin", "Burkina Faso", "Côte-d'Ivoire", 
                               "Gambia", "Ghana", "Guinea", "Guinea-Bissau", 
                               "Liberia", "Mali", "Mauritania", "Niger", 
                               "Nigeria", "Senegal", "Sierra Leone", "Togo"), 
          colour = "black", size=.8) +
  
  theme(legend.title = element_text(size = 10, face = "bold", ), 
        legend.position="right", 
        legend.text = element_text(size = 8, face = "bold"),
        #legend.position.inside =c(15,18), 
        plot.title = element_text(hjust = 0.5, size = 18),
        strip.text = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 8, face = "bold"),
        axis.text.y = element_text(size = 8, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"), 
        axis.title.y = element_text(size = 12, face = "bold"))+
  coord_fixed( xlim = c(-16.75, 10.1), ylim = c(4.25, 17.75)) 


# precipitation 2023 -------

library(RColorBrewer)
library(latex2exp)
library(ggplot2)
library(readxl)
#Importation des données
df <- read_excel("C:/Users/User/Desktop/Projet CARP/Données en fonction des maxima des precipitations/Matrice des paramètres.xlsx", 
                 sheet = "Précipitation")
#View(df)
attach(df)

# Premier code ---------
precip  <- data.frame(
  Latitude = rep(LAT, 24),
  Longitude = rep(LON, 24),
  val_precip = c(df$`2023`,df$`2022`,df$`2021`,df$`2020`,df$`2019`,df$`2018`,df$`2017`,df$`2016`,df$`2015`,
                 df$`2014`,df$`2013`,df$`2012`,df$`2011`,df$`2010`,df$`2009`,df$`2008`,df$`2007`,df$`2006`,
                 df$`2005`,df$`2004`,df$`2003`,df$`2002`,df$`2001`,df$`2000`) ,
  Year  = sort(c(rep(2000,length(LAT)), rep(2001,length(LAT)),  rep(2002,length(LAT)), rep(2003,length(LAT)),  
                 rep(2004,length(LAT)), rep(2005,length(LAT)),  rep(2006,length(LAT)), rep(2007,length(LAT)),  
                 rep(2008,length(LAT)), rep(2009,length(LAT)),  rep(2010,length(LAT)), rep(2011,length(LAT)),  
                 rep(2012,length(LAT)), rep(2013,length(LAT)),  rep(2014,length(LAT)), rep(2015,length(LAT)),    
                 rep(2016,length(LAT)), rep(2017,length(LAT)),  rep(2018,length(LAT)), rep(2019,length(LAT)),  
                 rep(2020,length(LAT)), rep(2021,length(LAT)),  rep(2022,length(LAT)), rep(2023,length(LAT))), T)
)

attach(precip)

annees = colnames(df[,-c(1, 2, 3)])

intervalle = c()
# Boucle sur chaque colonne du bloc
for (i in seq_along(annees)) {
  # Extraire la colonne
  an <- df[[annees[i]]]
  
  # Découper les valeurs de précipitation en intervalles
  decoupe <- cut(an,
                 breaks = c(-Inf, 50, 100, 150, 250, Inf),
                 labels = c("[0, 50]", "]50, 100]", 
                            "]100, 150]", "]150, 250]",
                            "]250, MAX]"))
  
  # Stocker les résultats dans une liste
  intervalle <- append(intervalle, decoupe)
}
#intervalle = intervalle[-c(1:nrow(precip))] 

precip$intervalle = intervalle

View(precip)

ggplot() +
  # Couleur des frontiÃ¨res
  geom_point(data = precip, aes(x = Longitude, y = Latitude, color =  intervalle),
             size = 5, stat = "identity" #, shape = "square"
  ) + # Points colorÃ©s par tempÃ©rature
  #scale_color_gradient(low = c("darkblue","lightblue"), high = c("red")) + # DÃ©gradÃ© de couleur
  facet_wrap(~ Year) +
  
  scale_color_manual(values = c("[0, 50]"    = "skyblue", 
                                "]50, 100]"  = "blue",
                                "]100, 150]" = "green",
                                "]150, 250]" = "orange",
                                "]250, MAX]" = "red")) +
  
  #scale_color_gradient(low = c("darkblue","lightblue", "green"), high = c("yellow", "orange", "red")) + # DÃ©gradÃ© de couleur
  #scale_fill_viridis_d(option = "A")+
  
  scale_x_continuous(name = "Longitude", 
                     breaks = seq(-17, 11, by = 6),
                     labels = function(x) paste0(abs(x), ifelse(x < 0, "°W", "°E"))) + # Ajout de W ou E
  scale_y_continuous(name = "Latitude", 
                     breaks = seq(4, 20, by = 4),
                     labels = function(y) paste0(y, "°N")) +
  
  labs(title = "Carte des précipitations", 
       #  x = "Longitude",
       #    y = "Latitude",
       color = "mm/jr :")+
  theme_minimal() +
  
  borders("world", regions = c("Benin", "Burkina Faso", "Côte-d'Ivoire", 
                               "Gambia", "Ghana", "Guinea", "Guinea-Bissau", 
                               "Liberia", "Mali", "Mauritania", "Niger", 
                               "Nigeria", "Senegal", "Sierra Leone", "Togo"), 
          colour = "black", size=.8) +
  
  theme(legend.title = element_text(size = 10, face = "bold", ), 
        legend.position="top", 
        legend.text = element_text(size = 8, face = "bold"),
        #legend.position.inside =c(15,18), 
        plot.title = element_text(hjust = 0.5, size = 18),
        strip.text = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 8, face = "bold"),
        axis.text.y = element_text(size = 8, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"), 
        axis.title.y = element_text(size = 12, face = "bold"))+
  coord_fixed( xlim = c(-16.75, 10.1), ylim = c(4.25, 17.75)) 


library(raster) # Analyse et modélisation géographoque des données 
library(maps)   # Affichage des cartes
library(ggplot2)
library(sf)
library(readxl)

precipitation <- read_excel("C:/Users/User/Desktop/Projet CARP/Données en fonction des maxima des precipitations/Matrice des paramètres.xlsx", 
                            sheet = "Précipitation")

# Les pays de l'Afrique de l'Ouest
pays = c("Benin", "Burkina Faso", "Cabo Verde", "Côte d'Ivoire", 
         "Gambia", "Ghana", "Guinea", "Guinea-Bissau", 
         "Liberia", "Mali", "Mauritania", "Niger", "Nigeria", 
         "Senegal", "Sierra Leone", "Togo")

# coordonnées spatiales de lat et lon. raster:: coordinates()
coordinates(precipitation) <- ~ LON + LAT 
# Notation des coordonnées sous forme de dégrés ouest et nord. raster:: coordinates()
proj4string(precipitation) <- CRS("+proj=longlat +datum=WGS84")

# Convertir les données en format sf pour ggplot2
precipitation_sf <- st_as_sf(precipitation) # Convert foreign object to an sf object


#colonnes = c("2023", "2022", "2021", "2000", "2019", "2018", "2017", "2016",
#             "2015", "2014", "2013", "2012", "2011", "2010", "2009", "2008",
#             "2007", "2006", "2005", "2004", "2003", "2002", "2001", "2000")


precipitation_sf$intervalle <- cut(precipitation_sf$"2023",
                                   breaks = c(-Inf, 50, 100, 150, 250, Inf),
                                   labels = c("[0, 50]", "]50, 100]", 
                                              "]100, 150]", "]150, 250]",
                                              "]250, MAX]"))

# Créer une carte
precip2023 = ggplot(precipitation_sf) +
  geom_sf(aes(color = intervalle), size = 1) +
  scale_color_manual(values = c("[0, 50]"    = "orange", 
                                "]50, 100]"  = "green",
                                "]100, 150]" = "skyblue",
                                "]150, 250]" = "blue",
                                "]250, MAX]" = "darkblue")) +
  labs(title = "2023", color = "Précipitation (mm/day)") +
  borders("world",regions = pays, colour = "black") + 
  coord_sf(ylim = c(3, 18), xlim = c(-17, 10.5)) + 
  theme_minimal()+
  theme(axis.title.x = element_blank(),  # Supprimer les titres des axes
        axis.title.y = element_blank(), 
        #axis.text.x = element_blank(),   # Supprimer les labels des axes
        #axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),   # Supprimer les ticks des axes
        axis.ticks.y = element_blank())

precip2023

#Deuxième code ############################

library(raster) # Analyse et modélisation géographique des données 
library(maps)   # Affichage des cartes
library(ggplot2)
library(sf)
library(readxl)

# Les pays de l'Afrique de l'Ouest
pays = c("Benin", "Burkina Faso", "Cabo Verde", "Côte d'Ivoire", 
         "Gambia", "Ghana", "Guinea", "Guinea-Bissau", 
         "Liberia", "Mali", "Mauritania", "Niger", "Nigeria", 
         "Senegal", "Sierra Leone", "Togo")


carte_precipitation = function(an){
  precipitation <- read_excel("C:/Users/User/Desktop/Projet CARP/Données en fonction des maxima des precipitations/Matrice des paramètres.xlsx", 
                              sheet = "Précipitation")
  
  
  # coordonnées spatiales de lat et lon. raster:: coordinates()
  coordinates(precipitation) <- ~ LON + LAT 
  # Notation des coordonnées sous forme de dégrés ouest et nord. raster:: coordinates()
  proj4string(precipitation) <- CRS("+proj=longlat +datum=WGS84")
  
  # Convertir les données en format sf pour ggplot2
  precipitation_sf <- st_as_sf(precipitation) # Convert foreign object to an sf object

  annees = colnames(precipitation_sf)
  indice_an = which(annees == an)
 # for (annee in annees) {
    # Découper les valeurs de précipitation en intervalle
    precipitation_sf$intervalle <- cut(precipitation_sf[[indice_an]],
                                       breaks = c(-Inf, 25, 50, 100, 200, Inf),
                                       labels = c("[0, 25]", "]25, 50]", 
                                                  "]50, 100]", "]100, 200]",
                                                  "]200, MAX]"))

   # Créer une carte
    return(ggplot(precipitation_sf) +
      geom_sf(aes(color = intervalle), size = 7) +
      scale_color_manual(values = c("[0, 25]"    = "orange", 
                                    "]25, 50]"  = "green",
                                    "]50, 100]" = "skyblue",
                                    "]100, 200]" = "blue",
                                    "]200, MAX]" = "darkblue")) +
      labs(title = an, color = "Précipitation (mm/jr)") +
      theme(plot.title = element_text(size = 8)) + 
      borders("world",regions = pays, colour = "black") + 
      coord_sf(ylim = c(3, 18), xlim = c(-17, 10.5)) + 
      theme_minimal()+
        
        if (an %in% c("2000", "2004", "2008", "2012", "2016", "2024")) {
          theme(axis.title.x = element_blank(),  # Supprimer les titres des axes
                axis.title.y = element_blank(), 
                axis.text.x = element_blank(),   # Supprimer les labels des axes
                #axis.text.y = element_blank(),
                axis.ticks.x = element_blank(),   # Supprimer les ticks des axes
                axis.ticks.y = element_blank())
        } else if(an %in% c("2021", "2022", "2023")) {
          theme(axis.title.x = element_blank(),  # Supprimer les titres des axes
                axis.title.y = element_blank(), 
                #axis.text.x = element_blank(),   # Supprimer les labels des axes
                axis.text.y = element_blank(),
                axis.ticks.x = element_blank(),   # Supprimer les ticks des axes
                axis.ticks.y = element_blank())
        } else if(an == "2020"){
          theme(axis.title.x = element_blank(),  # Supprimer les titres des axes
                axis.title.y = element_blank(), 
                #axis.text.x = element_blank(),   # Supprimer les labels des axes
                #axis.text.y = element_blank(),
                axis.ticks.x = element_blank(),   # Supprimer les ticks des axes
                axis.ticks.y = element_blank())
        } else{
          theme(axis.title.x = element_blank(),  # Supprimer les titres des axes
                axis.title.y = element_blank(), 
                axis.text.x = element_blank(),   # Supprimer les labels des axes
                axis.text.y = element_blank(),
                axis.ticks.x = element_blank(),   # Supprimer les ticks des axes
                axis.ticks.y = element_blank())
        }
    )
    #    }
    
}

  
precip2000 = carte_precipitation(2000)
precip2001 = carte_precipitation(2001)
precip2002 = carte_precipitation(2002)
precip2003 = carte_precipitation(2003)
precip2004 = carte_precipitation(2004)
precip2005 = carte_precipitation(2005)
precip2006 = carte_precipitation(2006)
precip2007 = carte_precipitation(2007)
precip2008 = carte_precipitation(2008)
precip2009 = carte_precipitation(2009)
precip2010 = carte_precipitation(2010)
precip2011 = carte_precipitation(2011)
precip2012 = carte_precipitation(2012)
precip2013 = carte_precipitation(2013)
precip2014 = carte_precipitation(2014)
precip2015 = carte_precipitation(2015)
precip2016 = carte_precipitation(2016)
precip2017 = carte_precipitation(2017)
precip2018 = carte_precipitation(2018)
precip2019 = carte_precipitation(2019)
precip2020 = carte_precipitation(2020)
precip2021 = carte_precipitation(2021)
precip2022 = carte_precipitation(2022)
precip2023 = carte_precipitation(2023)

library(cowplot)
# Ajouter une légende commune
legende <- get_legend(precip2023 + theme(legend.position="right"))

library(grid)
library(gridExtra)
grid.arrange(precip2000 + theme(legend.position="none"), 
             precip2001 + theme(legend.position="none"), 
             precip2002 + theme(legend.position="none"),
             precip2003 + theme(legend.position="none"),
             precip2004 + theme(legend.position="none"),
             precip2005 + theme(legend.position="none"),
             precip2006 + theme(legend.position="none"),
             precip2007 + theme(legend.position="none"),
             precip2008 + theme(legend.position="none"),
             precip2009 + theme(legend.position="none"),
             precip2010 + theme(legend.position="none"),
             precip2011 + theme(legend.position="none"),
             precip2012 + theme(legend.position="none"),
             precip2013 + theme(legend.position="none"),
             precip2014 + theme(legend.position="none"),
             precip2015 + theme(legend.position="none"),
             precip2016 + theme(legend.position="none"),
             precip2017 + theme(legend.position="none"),
             precip2018 + theme(legend.position="none"),
             precip2019 + theme(legend.position="none"),
             precip2020 + theme(legend.position="none"),
             precip2021 + theme(legend.position="none"),
             precip2022 + theme(legend.position="none"),
             precip2023 + theme(legend.position="none"),
             top = textGrob("Carte des précipitations maximales annuelles", gp=gpar(fontsize=8)),
             bottom = textGrob("Latitude", gp=gpar(fontsize=8)), 
             left = textGrob("Longitude", rot = 90, gp=gpar(fontsize=8)),
             legende,  ncol = 4)
  
  
  
  
