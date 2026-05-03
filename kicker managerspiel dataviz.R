##### Kicker Managerspiel Analyse #####
##
##### Zu ladende Datei muss folgendes Format haben:
##
##### Spalte = Spieltage (erste Spalte = Mitspielername)
##### Zeile = Mitspieler
##### Keine Summe for Zeile oder Spalte



##### Packages #####
library (tidyverse)
library (readxl)
library (viridis)
library (gganimate)
library (gifski)
library (RColorBrewer)
library (ggrepel)
#library (gt)
library (ggimage)
library (crosstable)
#library (ggradar)
library (plotly)
library (scales)


##### Loading Data and Setting Constants & Parameters #####
file<-"Saison2024_25_ZeGermans.xlsx"
#file<-"Saison2024_25_FC08.xlsx"
df_xls<-read_xlsx(file)
df_loss<-read_xlsx(file, sheet = "verpasst")


## momentaner Spieltag
if (is.na(df_xls[2,35])==TRUE) {
  matchday<-34-as.numeric(table(is.na(df_xls[1,]))[2])
  } else {
  matchday<-34
  }

if (is.na(df_loss[2,35])==TRUE) {
  matchday<-34-as.numeric(table(is.na(df_loss[1,]))[2])
} else {
  matchday<-34
}


#Rename column that contains Manager names
names(df_xls)[1]<-"Spieler"
names(df_loss)[1]<-"Spieler"

## Number of Players/Managers
np<-length(df_xls$Spieler)


## Remove NAs of matchdays that have not been played, yet
if (matchday!=34) {
  df_xls<-df_xls[,0:matchday+1]
}

if (matchday!=34) {
  df_loss<-df_loss[,0:matchday+1]
}


## create matchday list
Spieltag<-list(1:34)



## Colouring
palette<-"PuOr"

farbe<-c(brewer.pal(n = np, name = palette))

farbe_schnitt<-c(farbe,"#f03b20")

  
  
### copyright
copyright<-"kicker Managerspiel Analüüüüüüse (c) volkmeister"


## Unicode
letter<-"\u00F6"

schnitt<-round(mean(colMeans(df_xls[,2:matchday+1])),1)


##transpose df_xls
df_tp<-as.data.frame(t(df_xls))
colnames(df_tp)<-df_tp[1,] ## create column names from first row 
df_tp<-df_tp[-c(1),]## delete first row, which carries names
df_tp$Spieltag<-df_tp[,1]
df_tp$Spieltag<-1:matchday
df_tp[] <- lapply(df_tp, as.numeric) ## convert entire df to numeric

df<-pivot_longer(df_tp, cols=1:np, names_to = "Spieler",values_to = "Punkte", values_drop_na = FALSE)

##and also df_loss
df_tp1<-as.data.frame(t(df_loss))
colnames(df_tp1)<-df_tp1[1,] ## create column names from first row 
df_tp1<-df_tp1[-c(1),]## delete first row, which carries names
df_tp1$Spieltag<-df_tp1[,1]
df_tp1$Spieltag<-1:matchday
df_tp1[] <- lapply(df_tp1, as.numeric) ## convert entire df to numeric

df_vp<-pivot_longer(df_tp1, cols=1:np, names_to = "Spieler",values_to = "Punkte", values_drop_na = FALSE)




##### Animation of ranking #####

## create cumulative table from df_xls
df_cum<-df_xls

for (j in 1:np){   ## do it for each player
  for (i in 2:matchday+1){  ## first match day is kept, from second match day results are cumulative / first col is playernames
    
    df_cum[j,i]<-df_cum[j,i-1]+df_xls[j,i]
    
  }
 }

df_cum<-as.data.frame(t(df_cum))
colnames(df_cum)<-df_cum[1,] ## create column names from first row 
df_cum<-df_cum[-c(1),]## delete first row
df_cum[] <- lapply(df_cum, as.numeric) ## convert entire df to numeric
df_cum<-cbind(df_cum,list(1:matchday))
colnames(df_cum)[np+1]<-"Spieltag"

df_cum<-pivot_longer(df_cum, cols=1:np, names_to = "Spieler",values_to = "Punkte", values_drop_na = FALSE)
df_cum$Spieltag<-as.numeric(df_cum$Spieltag)
df_cum$Kurzname<-str_sub(df_cum$Spieler,1,3)

a<-df_cum %>%
  ggplot(aes(x=Spieltag, y=Punkte, group=Spieler, color=Spieler)) +
  geom_line() +
  geom_point() +
  geom_label_repel(aes(label=paste0(Kurzname,"-",as.character(Punkte)), color=Spieler),
                   force        = 0.5,
                   nudge_x      = -0.25,
                   direction    = "y",
                   hjust        = 1.5,
                   segment.size = 0.2) +
  labs(title = "Saisonverlauf",
       subtitle = "Animation mit Spielernamen nach Ligaplatzierung geordnet",
       caption = "kicker Managerspiel Analüüüüüüse (c) volkmeister")+
  scale_color_brewer(palette="Paired") +
  ylab("Gesamtpunkte") +
  theme_minimal() +
  transition_reveal(Spieltag)

animate(a, renderer = gifski_renderer(), device = "png", 
        end_pause=40, 
        height = 800, 
        width =1000, 
        fps=1.5)

anim_save(paste0("plots/Saison_",str_sub(file,15,20),"_",as.character(matchday),"_LineGraph.gif"))

### Static version

df_cum %>%
  ggplot(aes(x=Spieltag, y=Punkte, group=Spieler, color=Spieler)) +
  geom_line() +
  geom_point() +
  labs(title = "Saisonverlauf",
       caption = "kicker Managerspiel Analüüüüüüse (c) volkmeister")+
  scale_color_brewer(palette=palette) +
  ylab("Gesamtpunkte") +
  theme_minimal()
  
#geom_vline(xintercept = 17.5, linetype="longdash",size=0.5)

ggsave(paste0("plots/Verlauf",str_sub(file,15,20),"_",as.character(matchday),".jpeg"), device="jpeg")



##### Static table for points lost

df_cum_loss<-df_loss

for (j in 1:np){   ## do it for each player
  for (i in 2:matchday+1){  ## first match day is kept, from second match day results are cumulative / first col is playernames
    
    df_cum_loss[j,i]<-df_cum_loss[j,i-1]+df_loss[j,i]
    
  }
}

df_cum_loss<-as.data.frame(t(df_cum_loss))
colnames(df_cum_loss)<-df_cum_loss[1,] ## create column names from first row 
df_cum_loss<-df_cum_loss[-c(1),]## delete first row
df_cum_loss[] <- lapply(df_cum_loss, as.numeric) ## convert entire df to numeric
df_cum_loss<-cbind(df_cum_loss,list(1:matchday))
colnames(df_cum_loss)[np+1]<-"Spieltag"

df_cum_loss<-pivot_longer(df_cum_loss, cols=1:np, names_to = "Spieler",values_to = "Punkte", values_drop_na = FALSE)
df_cum_loss$Spieltag<-as.numeric(df_cum_loss$Spieltag)
df_cum_loss$Kurzname<-str_sub(df_cum_loss$Spieler,1,3)


df_cum_loss %>%
  ggplot(aes(x=Spieltag, y=Punkte, group=Spieler, color=Spieler)) +
  geom_line() +
  geom_point() +
  labs(title = "Hätte, hätte ... - Verpasste Punkte",
       caption = "kicker Managerspiel Analüüüüüüse (c) volkmeister")+
  scale_color_brewer(palette=palette) +
  ylab("Verlorene Punkte (gesamt)") +
  theme_minimal()

ggsave(paste0("plots/Verlauf_verlust",str_sub(file,15,20),"_",as.character(matchday),".jpeg"), device="jpeg")


##### Point curves per match day (facet rows by player) #####

df_ind<-pivot_longer(df_tp, cols=1:np, names_to = "Spieler",values_to = "Punkte", values_drop_na = FALSE)

df_ind %>%
  ggplot(aes(x=Spieltag, y=Punkte, color=Spieler, label=Punkte)) + 
  geom_line(aes(size=1)) +
  geom_hline(yintercept = schnitt,linetype='dotted', size=1) +
  geom_smooth(method='loess') + 
  geom_label (size=3, color='grey') + 
  facet_grid(rows=vars(Spieler)) +
  scale_x_continuous(breaks = c(1,5,10,15,20,25,30,34)) +
  geom_vline(xintercept = 17.5, linetype="dotted", size=0.5) +
  labs(title = "Individuelle Punktekurven",
       subtitle = "über die Saison hinweg, mit Trendlinie und ligaweitem Punkteschnitt und Winterpause (gestrichelte Linien)",
       caption = "kicker Managerspiel Analüüüüüüse (c) volkmeister") +
  theme_minimal() +
  theme(plot.title = element_text(size=22),
        legend.position="none")
  

ggsave(paste0("plots/Verlauf_Individuell",str_sub(file,15,20),"_",as.character(matchday),".jpeg"), 
       width = 21, 
       height = 14,
       device="jpeg")



##### Point Missed curves per match day (facet rows by player) #####

df_ind<-pivot_longer(df_tp1, cols=1:np, names_to = "Spieler",values_to = "Punkte", values_drop_na = FALSE)

df_ind %>%
  ggplot(aes(x=Spieltag, y=Punkte, color=Spieler, label=Punkte)) + 
  geom_line() +
  geom_label (size=3, color='grey') + 
  scale_x_continuous(breaks = c(1,5,10,15,20,25,30,34)) +
  labs(title = "Hätte, hätte ... - Verpasste Punkte",
       subtitle = "über die Saison hinweg, mit Trendlinie und ligaweitem Punkteschnitt und Winterpause (gestrichelte Linien)",
       caption = "kicker Managerspiel Analüüüüüüse (c) volkmeister") +
  theme_minimal() +
  theme(plot.title = element_text(size=22),
        legend.position="right")


ggsave(paste0("plots/Verlauf_Verlust_Ind",str_sub(file,15,20),"_",as.character(matchday),".jpeg"), 
       width = 21, 
       height = 14,
       device="jpeg")



##### Medallienspiegel #####
df_medal<-df_xls

for (i in 1:matchday+1){
df_medal[,i]<-rank(-df_medal[,i])  
}

df_medal[1:matchday+1]<-floor(df_medal[1:matchday+1]) ## RANK command splits shared ranks by adding decimals (e.g. 1.5 for both players sharing 1st). The floor command takes the integer without decimals

df_placing<-as.data.frame(t(df_medal))
colnames(df_placing)<-df_placing[1,] ## create column names from first row 
df_placing<-df_placing[-c(1),]## delete first row, which carries names
df_placing$Spieltag<-1:matchday
df_placing[] <- lapply(df_placing, as.numeric) ## convert entire df to numeric

df_pos<-pivot_longer(df_placing, cols=1:np, names_to = "Spieler",values_to = "Platzierung", values_drop_na = FALSE)

ft <- df_pos %>% group_by(Platzierung, Spieler) %>% summarise(freq=n())

ft$image<-ft$Platzierung
ft$image[ft$image==1]<-"pics/gold.png"
ft$image[ft$image==2]<- "pics/silver.png"
ft$image[ft$image==3]<- "pics/bronze.png"

ft$pos<-ft$Platzierung


for (i in 1:length(levels(as.factor(ft$Spieler)))){
  ## Pick first name from the players list using levels[1]
  name<-levels(as.factor(ft$Spieler))[i]
  bronze<-ft$freq[ft$Spieler==name & ft$Platzierung==3]
  silver<-ft$freq[ft$Spieler==name & ft$Platzierung==2]
  gold<-ft$freq[ft$Spieler==name & ft$Platzierung==1]
  
  if (length(bronze)==0) bronze<-0
  if (length(silver)==0) silver<-0
  if (length(gold)==0) gold<-0
  ## For bronze medal use 3rd /2
  ft$pos[ft$Spieler==name & ft$Platzierung==3]<-bronze/2
  ## For silver medal use 3rd+2nd/2
  ft$pos[ft$Spieler==name & ft$Platzierung==2]<-bronze+silver/2
  ## For gold use 3rd+2nd+1st/2
  ft$pos[ft$Spieler==name & ft$Platzierung==1]<-bronze+silver+gold/2
  
  
}


ft %>%
  subset (Platzierung<=3) %>%
  ggplot(aes(x=factor(Spieler), y=as.numeric(freq), label=freq, fill=factor(Platzierung))) +
  geom_col () +
  geom_image(aes(x=Spieler, y=pos, image=image), asp = 3/2, nudge_x=0.2, size=0.03) +
  geom_label(aes(label = freq), position = position_stack(vjust=0.5)) +
  scale_fill_manual(values=c("#F2CC54","#D7D7D7", "#AD8A56"), name="Medaillenspiegel") +
  labs(title = "Medaillenspiegel", subtitle = "Gold, Silber und Bronze über alle Spieltage der Saison hinweg",
       caption = copyright)+
  xlab("Spieler") + 
  ylab("Anzahl der Medaillen") +
  scale_y_continuous(breaks= pretty_breaks()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="none")
  
ggsave(paste0("plots/Medaillen_",str_sub(file,15,20),"_",as.character(matchday),".jpeg"),
       device="jpeg",
       scale = 1.5)


# #ffeda0, "#999999", "#E69F00"

  
#show_html(ftable(xtabs(freq ~ Platzierung+Spieler, data=ft)))


df_pos %>%
  crosstable(Platzierung, by="Spieler", total="both") %>%
  as_flextable(keep_id=T)



##### Season-long positions by player (facet rows) #####

#df_pos$Platzierung<-as.factor(df_pos$Platzierung)

df_pos %>%
  ggplot(aes(x=Spieltag, y=Platzierung, color=Spieler, label=Platzierung)) + 
  geom_line(aes(size=0.5)) +
  geom_smooth(method='loess') + 
  geom_label (size=2, color='grey') + 
  facet_grid(rows=vars(Spieler)) +
  labs(title = "Individuelle Platzierung",
       subtitle = "über die Saison hinweg",
       caption = "kicker Managerspiel Analüüüüüüse (c) volkmeister") +
  scale_y_reverse(breaks=pretty_breaks()) +
  ylim(np,1) +
  theme_minimal() +
  theme(plot.title = element_text(size=22),
        legend.position="none")


ggsave(paste0("plots/Platzierung_Individuell",str_sub(file,15,20),"_",as.character(matchday),".jpeg"), 
       width = 21, 
       height = 14,
       device="jpeg")




##### Rote Laterne ####

ft_rl<- ft %>% 
    subset(Platzierung==np) 

ft_rl$image<-"pics/rote_laterne.png"


## insert the players with zero Red Lanterns

list1<-sort(unique(ft_rl$Spieler))
list2<-sort(unique(ft$Spieler))
m<-0
list<-c()

for (i in 1:length(list2)) {
  if (list2[i] %in% list1) {
    m<-i
    }
  
  if (m<i) {
    list<-c(list,list2[i])
  }
}


for (i in 1:length(ft_rl$Spieler)){
  laternen<-ft_rl$freq[i]
  steps<-laternen-1
  zeile<-ft_rl[i,]
  if (laternen >1) {
    for (n in 1:steps){
      zeile$freq<-n
      ft_rl<-rbind(ft_rl, zeile)
    }
  }
}


for (i in 1:length(list)){
  vec<-ft_rl[1,]
  vec$Spieler<-list[i]
  vec$freq<-0
  rbind(ft_rl,vec)
  
}


ft_rl %>%
ggplot(aes(Spieler, freq)) +
    geom_dotplot(method="histodot", binaxis = "y", binwidth=1, stackgroups=T, dotsize = 0.1) +
    geom_image(aes(x=Spieler, y=freq, image=image), size=0.04, asp = 1.5) +
    coord_flip() +
    labs(title = "Rote Laternen", subtitle = "Letzte Plätze pro Spieler über die Saison hinweg",
       caption = copyright)+
    xlab("Spieler") + 
    ylab("Anzahl der roten Laternen") +
    theme_minimal()

h<-10
max<-max(ft_rl$freq)
w<-2+max*5

ggsave(paste0("plots/Laterne_",str_sub(file,15,20),"_",as.character(matchday),".jpeg"), 
       width = w,
       height = h,
       scale = 1.3,
       units = "cm",
       device="jpeg")


#### Bar chart animaion ####

## update df_cum

df_cum$Platzierung<-df_pos$Platzierung

df_cum$Label<-word(df_cum$Spieler,1)

df_cum<-df_cum[order(df_cum$Spieltag, df_cum$Label),]

#design for loop with stepsize = np

for (i in seq(1, length(df_cum$Spieler), np)){
    
    begin<-i
    end<-i+(np-1)
    temp<-rank(-df_cum$Punkte[begin:end],ties.method = "first")
    df_cum$Platzierung[begin:end]<-temp
    
}
  

# Plot
spieltag_plot <- ggplot(data = df_cum, aes(y = Platzierung, group = Spieler)) +
                    geom_tile(aes(x = Punkte/2, width = Punkte, fill=Spieler),
                              height = 0.8,
                              alpha = 0.7,
                              #fill=Spieler,
                              color = NA) +
                    geom_text(aes(x = 0, label = Label),
                              hjust = 1,
                              nudge_x = -50,
                              size = 11 / .pt) +
                    geom_text(aes(x = Punkte, label = as.character(Punkte)),
                              hjust = 0,
                              nudge_x = 20,
                              size = 14 / .pt) +
                    coord_cartesian(clip = "off") +
                    scale_y_reverse(breaks = seq(1, np, by = 1)) +
                    scale_x_continuous(labels = label_number(scale_cut = cut_short_scale())) +
                    scale_fill_manual(values=farbe) +
                    labs(
                              title = "Managerliga - Gesamtpunkte pro Spieltag",
                              subtitle = "Spieltag: {closest_state}",
                              caption = copyright,
                              x = "Gesamtpunkte",
                              y = NULL) +
                    theme_minimal() +
                    theme(
                              plot.margin = margin(80, 120, 80, 120),
                              plot.title = element_text(
                                face = "bold",
                                size = 16),
                              plot.subtitle = element_text(
                              face = "bold",
                              color = "black")) +
                    transition_states(
                              states = Spieltag,
                              wrap = FALSE,
                              transition_length = 1.5,
                              state_length = 2.5) +
                    ease_aes('cubic-in-out')

# Create animation
spieltag_anim <- animate(
  plot = spieltag_plot,
  nframes = 250, fps = 25,
  width = 1200, height = 1000,
  end_pause = 15
)

# Save animation
anim_save(paste0("plots/Saison_",str_sub(file,15,20),"_",as.character(matchday),".gif"))



##### Spannbreite (Punkte) - Boxplot #####

means <- aggregate(Punkte ~  Spieler, df, mean)
means$Punkte<-round(means$Punkte, digits=1)


df %>%
ggplot(aes(Punkte,Spieler, fill=Spieler)) + 
  geom_boxplot() + 
  labs(title="Statistische Verteilung der Spieltagspunkte pro Manager",x="Punkte", y = "Manager") +
  scale_x_continuous(minor_breaks = seq(-20 , 100, 5), breaks = seq(0, 100, 10)) +
  scale_fill_manual(values=farbe) +
  coord_cartesian(xlim = c(-15, 110))+
  stat_summary(fun=mean, colour="red", geom="point", 
               shape=18, size=3, show.legend=FALSE) + 
  geom_label(data = means,vjust=1.5, aes(label = Punkte)) +
  theme_minimal() +
  theme(legend.position="none")


ggsave(paste0("plots/Spannbreite_",str_sub(file,15,20),"_",as.character(matchday),".jpeg"), 
       device="jpeg",
       width = 3600,
        height = 1800,
       units = "px")



##### Haette Haette Fahrradkette #####

means_vp <- aggregate(Punkte ~  Spieler, df_vp, mean)
means_vp$Punkte<-round(means_vp$Punkte, digits=1)


df_vp %>%
  ggplot(aes(Punkte,Spieler, fill=Spieler)) + 
  geom_boxplot() + 
  labs(title="Hätte, hätte Fahrradkette... - Punkte auf der Bank",x="Punkte", y = "Manager") +
  scale_x_continuous(minor_breaks = seq(-20 , 50, 5), breaks = seq(0, 50, 10)) +
  scale_fill_manual(values=farbe) +
  coord_cartesian(xlim = c(-15, 50))+
  stat_summary(fun=mean, colour="red", geom="point", 
               shape=18, size=3, show.legend=FALSE) + 
  geom_label(data = means_vp,vjust=1.5, aes(label = Punkte)) +
  theme_minimal() +
  theme(legend.position="none")


ggsave(paste0("plots/Hätte_Hätte_",str_sub(file,15,20),"_",as.character(matchday),".jpeg"),
       device="jpeg",
       scale = 1.2,
       width = 3600,
       height = 1800,
       units = "px")




##### Winterpause - Boxplot #####

df$runde[df$Spieltag <=17 ] <- "Hinrunde"
df$runde[df$Spieltag >17 ] <- "Rückrunde"

means <- aggregate(Punkte ~  Spieler, df, mean)
means$Punkte<-round(means$Punkte, digits=1)




df %>%
  ggplot(aes(Punkte,Spieler, fill=Spieler)) + 
  geom_boxplot() + 
  labs(title="Wer hat die besten Transfers? Hin- und Rückrunde im Vergleich",x="Punkte", y = "Manager") +
  scale_x_continuous(minor_breaks = seq(-20 , 100, 5), breaks = seq(0, 100, 10)) +
  scale_fill_manual(values=farbe) +
  #coord_cartesian(xlim = c(-15, 90))+
  stat_summary(fun.y=mean, colour="red", geom="point", 
               shape=18, size=2,show.legend=FALSE) + 
  stat_summary(aes(label=round(..x..,0)),fun.x=mean, color="black", fill="white", alpha=0.8, vjust=1.5, geom="label", 
               shape=18, size=3,show.legend=FALSE) + 
  theme_minimal() +
  theme(legend.position="none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  coord_flip() +
  facet_grid(.~runde)

ggsave(paste0("plots/Hin_Rueck_",str_sub(file,15,20),"_",as.character(matchday),".jpeg"), device="jpeg")

##### Spieltagsdurchschnitt (Boxplot) ####

means_tag <- aggregate(Punkte ~ Spieltag, df, mean)
means_tag$Punkte<-round(means_tag$Punkte, digits=1)

cols <- colorRampPalette(brewer.pal(12, "Set3"))
myPal <- cols(length(unique(df$Spieltag)))

df %>%
  ggplot(aes(Punkte, as.factor(Spieltag))) + 
  geom_boxplot(aes(fill=Spieltag), color="orange") +
  stat_summary(fun=mean, geom="point", shape=23, size=2, color="white", fill="purple") +
  scale_x_continuous(minor_breaks = seq(-20 , 100, 5), breaks = seq(0, 100, 10)) +
  labs(title = "Managerliga Performance", subtitle = "Punkteverteilung innerhalb underer Liga pro Spieltag",
       caption = copyright) +
  xlab("Punkte") + 
  ylab("Spieltag") +
  theme_minimal() +
  theme(legend.position="none")

ggsave(paste0("plots/Speiltagsdurchschnitt_",str_sub(file,15,20),"_",as.character(matchday),".jpeg"), device="jpeg")


##### Plotly Linegraph per game day and for all players ####

df_avg <- df %>%
  group_by(Spieltag) %>%
  summarise(Punkte = mean(Punkte))

df_avg$Spieler <-"Schnitt"
  
df_avg<-rbind(arrange(df, Spieler), df_avg)


plot<-df_avg %>%
  ggplot(aes(x=Spieltag, y=Punkte, color=Spieler)) + 
  geom_line(size=1.5, linetype=1) +
  scale_color_manual(values=farbe_schnitt) +
  scale_x_continuous(breaks=seq(1, matchday, 1)) +
  labs(title = "Punkte pro Spieler pro Spieltag", subtitle = "Spieltagschnitt in rot",
       caption = copyright) +
  theme_minimal()
  
plot

ggplotly(plot) %>%
  layout(hovermode="x")

##### Radar ####

df_avg_radar<-pivot_wider(df_avg, names_from = "Spieltag",
            values_from = "Punkte")
colnames(df_avg_radar)[1]<-"group"
df_avg_radar[df_avg_radar<0]<-0


radar<-ggradar(df_avg_radar,
        base.size=10,
        values.radar = c("0","50","86"),
        grid.min = 0, grid.mid = 50, grid.max = 86,
        label.gridline.min = FALSE,
        group.line.width = 1,
        group.point.size = 2,
        plot.title = "Radar Graph fuer alle spieltage",
        group.colours = c("#fec44f",    #Adam
                                 "#a1dab4",    #Jorg
                                 "#41b6c4",    #Noah
                                 "#2c7fb8",    #Rainer
                                 "#f03b20",    #Schnitt
                                 "#253494"))  #Volker

radar

ggplotly(radar)

##### Spieltagsanteil ####

df_no_neg<-df
df_no_neg[df_no_neg<0]<-0

df_no_neg %>%
  ggplot(aes(Spieltag, Punkte, fill=Spieler)) +
  geom_col() +
  scale_x_continuous(breaks=seq(1, 34, 1)) +
  scale_fill_manual(values=farbe) +
  labs(title = "Punkteverteilung pro Spieltag", subtitle = "Anteile an der von der Liga erspielten Punkte",
       caption = copyright) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=90, vjust=0.5))


ggsave(paste0("plots/Anteil_absolut",str_sub(file,15,20),"_",as.character(matchday),".jpeg"), device="jpeg")


df_no_neg %>%
  ggplot(aes(Spieltag, Punkte, fill=Spieler)) +
  geom_col(position="fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks=seq(1, 34, 1)) +
  scale_fill_manual(values=farbe) +
  labs(title = "Punkteanteil pro Spieltag", subtitle = "prozentualer Anteil jedes Managers am Ergebnis der Liga",
       caption = copyright) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=90, vjust=0.5))

ggsave(paste0("plots/Anteil_prozentual",str_sub(file,15,20),"_",as.character(matchday),".jpeg"), device="jpeg")







