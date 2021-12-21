#Load libraries
#install.packages("rstudioapi", dependencies = TRUE)
#install.packages("readxl", dependencies = TRUE)
#install.packages("ggplot2", dependencies = TRUE)
#install.packages("dplyr", dependencies = TRUE)
#install.packages("tidyr", dependencies = TRUE)
#install.packages("directlabels", dependencies = TRUE)
#install.packages("ggimage", dependencies = TRUE)

# install.packages("rgdal", dependencies = TRUE)
#install.packages("corrplot", dependencies = TRUE)

library(rstudioapi)
library(readxl)
library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)
library(directlabels)
library(data.table)
library(reshape2)
library(ggimage)
library('grid')
library(RColorBrewer)
library(maptools)
library(ggplot2)
library(rgdal)
library(corrplot)

theme_var =   theme(plot.title = element_text(hjust = 0.5), 
             panel.background = element_rect(fill = '#f1f1f1', colour = 'white'),
             legend.position = c(.90,.85),
             panel.grid.minor = element_blank(),
             axis.text.y=element_blank(),
             axis.ticks.y=element_blank(),
             axis.text=element_text(size=14))

theme_var_leg_out =   theme(plot.title = element_text(hjust = 0.5), 
                   panel.background = element_rect(fill = '#f1f1f1', colour = 'white'),
                   panel.grid.minor = element_blank(),
                   axis.text.y=element_blank(),
                   axis.ticks.y=element_blank(),
                   axis.text=element_text(size=14))

# Set current directory
currentDir <- dirname(getActiveDocumentContext()$path)
setwd(currentDir)

# Get europe boarders for map charts
# http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip

my_spdf <- readOGR( 
  dsn= ("data/TM_WORLD_BORDERS_SIMPL-0.3.shp") , 
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)
europe <- my_spdf[my_spdf@data$REGION==150 , ]
data <- fortify(europe)
europe@data$id=rownames(europe@data)


get_eurostat_table <- function(path,sheet,columns_numeric,columns,skip=0) {
  df <- read_excel(path,sheet=sheet, skip = skip,col_types = "text")
  df <- pivot_longer(df, cols = starts_with("2"))
  colnames(df) <- columns
  df[columns_numeric] <- sapply(df[columns_numeric],function(x) as.numeric(as.character(x)))
  return(df)
}

get_who_table<- function(path,columns_numeric) {
  df <- read.csv(path,header =TRUE)
  df[columns_numeric] <- sapply(df[columns_numeric],function(x) as.numeric(as.character(x)))
  return(df)
}


# Country main table
df_country_metadata <- read.csv("data/country_metadata.csv", header =TRUE)
colnames(df_country_metadata) <- c("country","country_name","iso_name","lat_center","long_center")



# GDP pps
df_gdp_pps = get_eurostat_table(path="data/eurostat_gdp_pps.xlsx",
                                           sheet='Sheet 1',
                                           columns_numeric = c("year", "gdp_pps"),
                                           columns=c("country", "year", "gdp_pps"),
                                           skip=8)

# Unemployment
df_unemployment = get_eurostat_table(path="data/eurostat_unemployment.xlsx",
                                           sheet='Sheet 3',
                                           columns_numeric = c("year", "unemployment"),
                                           columns=c("country", "year", "unemployment"),
                                           skip=8)


# sucide_rate_total
df_suicide_rate_total = get_eurostat_table(path="data/eurostat_suicide_rate_by_sex.xlsx",
                                     sheet='Sheet 1',
                                     columns_numeric = c("year", "sucide_rate_total"),
                                     columns=c("country", "year", "sucide_rate_total"),
                                     skip=10)


# Sucide rates absolutes - detailed
df_suicide_rate_detailed = get_who_table(path='data/who_suicide_statistics.csv',columns_numeric=c("suicides_no","population","year"))


# Combine datasets - select only specific counties in EU in years 2011 - 2015
dfs = list(df_gdp_pps,df_unemployment,df_suicide_rate_total)
dfs_new = join_all(dfs, by=c("country","year"), type='left')
df_final = join_all(list(df_country_metadata,dfs_new), by="country", type='left')
df_final = df_final[df_final$year %in% c("2011","2012","2013","2014","2015"),]


df_final$sucide_rate_total_<- with(df_final, ave(sucide_rate_total, iso_name, 
                FUN=function(x) c(NA, diff(x)/x[-length(x)]) ))*100

df_final$sucide_rate_total_<- round(df_final$sucide_rate_total_,1)


# Chart bar EU growth - Slide 3
ggplot(df_final[df_final$iso_name=="EU",], aes(x = year, y = sucide_rate_total_)) + 
  geom_bar(stat='identity', fill = "#324EAD") +
  geom_text(aes(label = sucide_rate_total_),color="white",size=8, 
            vjust = ifelse(df_final[df_final$iso_name=="EU",]$sucide_rate_total_ >= 0, 3, -2)) +
  theme_var + 
  labs( title = "Suicide Rate change % of European Union, 2011-2015") +
  ylab("change %") +
  xlab("year")


# Chart 1 Line EU trend (with theme) - Slide 3
ggplot(df_final[df_final$iso_name=="EU",], aes(x = year, y = sucide_rate_total)) + 
  coord_cartesian(ylim = c(11, 13)) +
  geom_text(aes(label = sucide_rate_total),vjust = -3, hjust = 0.5,color="#ffee93",size=5) +
  geom_line(color = "#ffee93", size =1, linetype = "dashed") +
  geom_point(color = "#ffee93", size = 1)+
  geom_image(aes(image="https://storage.googleapis.com/give_me_random_datasets/start.png"), size=.05, asp = 1.5) +
  theme(plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill = '#324EAD', colour = 'transparent'),
        legend.position = c(.90,.85),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text=element_text(size=14)) +
  labs( title = "Suicide Rate of European Union, 2011-2015") +
  ylab("sucide rate (per 100k population)") +
  xlab("year")


# Chart 1 Line Greece trend - Slide 4
ggplot(df_final[df_final$iso_name=="GRC",], aes(x = year, y = sucide_rate_total)) + 
  geom_line(stat = "identity") +
  coord_cartesian(ylim = c(4, 5.3)) +
    geom_text(aes(label = sucide_rate_total),vjust = -2, hjust = 0.5,color="#324EAD",size=5) +
  geom_line(color = "#324EAD", size = 2) +
  geom_point(color = "#324EAD", size = 7)+
  theme_var +
  labs( title = "Suicide Rate, Greece 2011-2015") +
  ylab("sucide rate (per 100k population)") +
  xlab("year")

# Chart bar Greece growth - Slide 4
ggplot(df_final[df_final$iso_name=="GRC",], aes(x = year, y = sucide_rate_total_)) + 
  geom_bar(stat='identity', fill = "#324EAD") +
  geom_text(aes(label = sucide_rate_total_),color="white",size=8, 
            vjust = ifelse(df_final[df_final$iso_name=="GRC",]$sucide_rate_total_ >= 0, 3, -2)) +
  theme_var + 
  labs( title = "Suicide Rate change % of European Union, 2011-2015") +
  ylab("change %") +
  xlab("year")



# Chart all countries lines - No Slide
ggplot(df_final[!df_final$iso_name=="LTU",], aes(x = year, y = sucide_rate_total, group = country_name, colour = country_name)) + 
  geom_line() +
  scale_colour_discrete(guide = 'none') +
  scale_x_continuous(expand=c(0, 1)) +
  theme_var +
  geom_dl(aes(label = country_name), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) +
  labs( title = "Suicide Rates by Country 2011-2015") +
    ylab("sucide rate (per 100k population)") +
  xlab("year")


# Chart all countries trend charts - Slide 5
ggplot(df_final, aes(x = year, y = sucide_rate_total)) + 
  geom_line(color = "#000000", size = 0.5) + 
  geom_smooth(method = "lm",color="darkgrey", size=0.8, linetype= "dashed", se = FALSE) +
  facet_wrap(~ country_name , scales = "free") +
  xlab("year") + 
  ylab("sucide rate (per 100k population)") +
  labs( title = "Suicide Rates by Country, 2011-2015") + 
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.background = element_rect(colour="#CCCCC4",fill="#CCCCC4"),
        strip.text = element_text(colour = 'black'))



# Chart all countries max min scale range - Slide 6
df_final_max <- aggregate( sucide_rate_total  ~  country_name, data = df_final, max)
df_final_min <- aggregate( sucide_rate_total  ~  country_name, data = df_final, min)
diff = df_final_max$sucide_rate_total - df_final_min$sucide_rate_total
newdf <- data.frame(cbind(diff, df_final_min$country_name))
colnames(newdf) <- c("y_scale_range","country_name")
newdf <- newdf[order(newdf$y_scale_range,decreasing = FALSE),]
newdf$seq = seq(1, nrow(newdf))
newdf$y_scale_range <- as.numeric(newdf$y_scale_range)
newdf$trend <- ifelse(newdf$country_name %in% c("Greece", "Cyprus", "Iceland", "Netherlands", "Portugal", "Spain", "Luxembourg", 
                                                "Turkey", "UK"),"upward","downward/ neutral") 


ggplot(newdf, aes(x=seq, y=y_scale_range, fill=trend))  +
  scale_fill_manual(values=c("#4d4d52","#B91646")) +
  geom_bar(stat = "identity") +
  scale_y_continuous() +
  coord_flip() +   
  geom_text(aes(label = country_name),vjust = 0.3, hjust = 1.1,color="#ffffff") +
  labs( title = ("Suicide Rates Max-Min range by Country, 2011-2015")) +
  xlab("") + 
  ylab("max-min range") +
  theme(plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill = '#f1f1f1', 
                                        colour = 'white'),legend.position = c(.90,.85),
        
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())


# Chart all countries trend charts with color scale  - Slide 7
df_final$bins = cut(df_final$sucide_rate_total,c(0,5,10,13,15,18,30))
ggplot(data=df_final[!df_final$country_name=='Lithuania',], aes(y=country_name, x=year, fill=sucide_rate_total)) + 
  geom_tile(aes(fill = bins)) +
  scale_fill_manual(values=c("#4CA1A3","#93c6c7","#b3d7d7","#e6aebf","#dc8ca4","#B91646", "#d0f4de")) +
  xlab("Year") + 
  ylab("Country") +
  labs( title = "Suicide Rates by Country, 2011-2015" ,fill = "Suicide rate bins,\nusing EU threshold") 


# Chart all countries ordered vertical barplot single year - Slide 8
chart_all_barplot_single_year <- function(df_final,selected_year) {
  df_final_year <- df_final[df_final$year %in% c(selected_year),]
  eu_threshold_year <- df_final_year$sucide_rate_total[df_final_year$iso_name=="EU"]
  df_final_year <- df_final_year[!df_final_year$iso_name=='EU',]
  df_final_year <- df_final_year[order(df_final_year$sucide_rate_total,decreasing = TRUE),]
  df_final_year$threshold <- ifelse(df_final_year$sucide_rate_total>eu_threshold_year, "more", "less")
  df_final_year$is_greece <- ifelse(df_final_year$iso_name=='GRC', 1, 0)
  df_final_year$seq = seq(1, nrow(df_final_year))
  
  ggplot(df_final_year, aes(x=seq, y=sucide_rate_total,fill=threshold))  +
    scale_fill_manual(values=c("#4CA1A3","#B91646")) +
    geom_bar(stat = "identity") +
    coord_flip() +   
    geom_text(aes(label = country_name),vjust = 0.3, hjust = 1.1,color="#ffffff") +
    labs( title = paste0("Suicide Rates by Country, ",selected_year),
          fill = "EU average \nsuicide rate",
    ) +
    scale_y_continuous(breaks = round(seq(round(min(df_final_year$sucide_rate_total))-1, 
                                round(max(df_final_year$sucide_rate_total))+1, by =2),1)) +
    xlab("") + 
    ylab("sucide rate (per 100k population)") +
    theme(plot.title = element_text(hjust = 0.5), 
          panel.background = element_rect(fill = '#f1f1f1', 
          colour = 'white'),legend.position = c(.90,.85),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank())
}

par(mfrow=c(2,2))
chart_all_barplot_single_year(df_final,2011)
chart_all_barplot_single_year(df_final,2012)
chart_all_barplot_single_year(df_final,2013)
chart_all_barplot_single_year(df_final,2014)
chart_all_barplot_single_year(df_final,2015)

  
  
# Map all countries trend charts color single year - Slide 9
chart_all_map_single_year <- function(df_final,polygon,selected_year) {
  df_final_year = df_final[df_final$year %in% c(selected_year),]
  dfs_new = left_join(df_final_year,polygon@data, by=c("iso_name"="ISO3"), type='left')
  data <- left_join(data, dfs_new, by=c('id'))
  data$bins = cut(data$sucide_rate_total,c(0,10,13,18,30))
  labels = data[!duplicated(data[,c("country_name")]),]
  
  p = ggplot() +  
  geom_polygon(data=data, aes(long, lat, group = group, fill = bins), colour = "black", size = 0.7) +
  geom_label(data=labels,mapping = aes(x = long_center,y = lat_center, label = country_name),vjust = 0.3, hjust = 1.1,alpha=0.5) +
  xlim(-10,50) + 
  ylim(30,70) +
  scale_fill_manual(values = c("#4CA1A3","#93c6c7","#dc8ca4","#B91646", "#d0f4de"))+
  labs( title = paste0("Suicide Rates by Country, ", selected_year),
          fill = "Suicide rate bins,\nusing EU threshold"
  ) +
    xlab("") + 
    ylab("") +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          strip.background = element_rect(colour="#9E7777",fill="#9E7777"),
          strip.text = element_text(colour = 'white'))
  return(p)
}

chart_all_map_single_year(df_final,polygon=europe,selected_year=2011)
chart_all_map_single_year(df_final,polygon=europe,selected_year=2012)
chart_all_map_single_year(df_final,polygon=europe,selected_year=2013)
chart_all_map_single_year(df_final,polygon=europe,selected_year=2014)
chart_all_map_single_year(df_final,polygon=europe,selected_year=2015)

# Scatterplot to compare years - Slide 10
y1 <- df_final[df_final$year==2011,]$sucide_rate_total
y2 <- df_final[df_final$year==2014,]$sucide_rate_total
names <- df_final[df_final$year==2014,]$country_name
data = data.frame(y1,y2,names)
data$stability_line = as.factor(ifelse(data$y2>data$y1,0,1))

ggplot(data = data,aes(x=y1, y=y2)) +
  geom_point(aes(color=stability_line)) + 
  geom_text(aes(x = y1, y = y2, label = names,color=stability_line),
            
            vjust = -1, size = 3) +
  scale_color_manual(labels = c("increased", "decreased"), values = c("#B91646","#7ea4b3")) +
  geom_abline(slope=1, intercept=0,color = "#4CA1A3", size=0.5) +
  geom_vline(xintercept=12.2, linetype="dashed", 
             color = "black", size=0.05) +
  geom_hline(yintercept=11.9, linetype="dashed", 
             color = "black", size=0.05) +
  xlim(1, 25) +
  ylim(1, 20) +
  theme(plot.title = element_text(hjust = 0.5), 
                      panel.background = element_rect(fill = '#f1f1f1', colour = 'white'),
                      legend.position = c(.90,.15),
                      axis.text=element_text(size=10)) +
  xlab("sucide rate (per 100k population), 2011") + 
  ylab("sucide rate (per 100k population), 2014") +
  labs( title = "Suicide Rates by Country, 2011 vs 2014", 
        fill = "Suicide rate bins,\nusing EU threshold", color = "sucide rate \n2011 vs 2014") 



# Greece total - No Slide
df_final_this <- df_final[df_final$iso_name=="GRC",]
df_final_this_1 <- df_final_this[c("sucide_rate_total","sucide_rate_total_","year")]


# Greece GDP PPS - No Slide
df_final_this_1 <- df_final_this[c("gdp_pps","year")]

ggplot(df_final_this_1, aes(x = year, y = gdp_pps)) + 
  geom_line(stat = "identity") +
  geom_text(aes(label = gdp_pps),vjust = -2, hjust = 0.5,color="#324EAD",size=5) +
  geom_line(color = "#324EAD", size = 2) +
  geom_point(color = "#324EAD", size = 7)+
  theme_var +
  labs( title = "GDP per capita in PPS, 2011-2015") +
  ylab("GDP PPS") +
  xlab("year")


# Detailed total - Slide 11
gr_raw <- df_suicide_rate_detailed[df_suicide_rate_detailed$country=="Greece" & df_suicide_rate_detailed$year > 2010,]
gr_raw_gs  <- plyr::ddply(gr_raw, c("year"), summarise, suicides_no=sum(suicides_no))


ggplot(gr_raw_gs, aes(x = year, y = suicides_no)) + 
  geom_line(stat = "identity") +
  geom_text(aes(label = suicides_no),vjust = -2, hjust = 0.5,color="#324EAD",size=5) +
  geom_line(color = "#324EAD", size = 2) +
  geom_point(color = "#324EAD", size = 7)+
  theme_var +
  ylim(450, 600) +
  labs( title = "Total Suicides, Greece 2011-2015") +
  ylab("sucides #") +
  xlab("year")


# Detailed by sex - Slide 11
gr_raw <- df_suicide_rate_detailed[df_suicide_rate_detailed$country=="Greece" & df_suicide_rate_detailed$year > 2010,]
gr_raw_gs  <- plyr::ddply(gr_raw, c("year","sex"), summarise, suicides_no=sum(suicides_no))

ggplot(gr_raw_gs, aes( x=year, y=suicides_no, group=sex, fill=sex )) + 
  geom_bar(stat="identity", width=.5, position = "dodge")   +
  scale_fill_manual(values=c("female"="#BEAEE2","male"="#79B4B7")) +
  geom_text(aes(label = suicides_no),vjust = -1, hjust = 0.5 ,size=5, position = position_dodge(width = .5)) +
    theme_var_leg_out +
  xlab("year") + 
  ylab("sucides #") +
  labs( title = "Sucides per sex, Greece 2011-2015") 

# Detailed by age - Slide 11
gr_raw <- df_suicide_rate_detailed[df_suicide_rate_detailed$country=="Greece" & df_suicide_rate_detailed$year > 2010,]
gr_raw_gsa <- plyr::ddply(gr_raw, c("year","age"), summarise, suicides_no=sum(suicides_no))
ggplot(gr_raw_gsa, aes(x = year, y = suicides_no, group = age, colour = age)) + 
  geom_line(size=1) +
  scale_colour_discrete(guide = 'none') +
  scale_x_continuous(expand=c(0, 1)) +
  geom_dl(aes(label = age), method = list(dl.trans(x = x + 0.2), "last.points", cex = 1.2)) +
  theme_var_leg_out +
  xlab("year") + 
  ylab("sucides #") +
  labs( title = "Sucides per age group, Greece 2011-2015") 


# Suicide rate vs unemployment in Greece - Slide 12
df_final_this <- df_final[df_final$iso_name=="GRC",]
df_final_this_0 <- df_final_this[c("unemployment","sucide_rate_total","gdp_pps","year")]
M = cor(df_final_this_0[c("sucide_rate_total","unemployment","gdp_pps")])
corrplot(M, type="upper", method="number", tl.pos="lt", tl.col="black",  tl.offset=1, tl.srt=0)

# Suicide rate vs unemployment in Greece - Slide 13
df_final_this <- df_final[df_final$iso_name=="GRC",]
df_final_this_0 <- df_final_this[c("unemployment","sucide_rate_total","year")]
colnames(df_final_this_0) <- c("x","y","year")
linear.model <-lm(y ~ x, df_final_this_0)
exp.model <-lm(y ~ exp(x), df_final_this_0)
linear.model.df <- data.frame(x = df_final_this_0$x, y = fitted(linear.model))

ggplot(data = df_final_this_0,aes(x=x, y=y)) +
  geom_point() + 
  geom_smooth(method="lm", aes(color="Exp Model"), formula= (y ~ exp(x)), se=FALSE, linetype = 1) +
  geom_line(data = linear.model.df, aes(x, y, color = "Linear Model"), size = 1, linetype = 2) + 
  xlim(16,30) +
  ylim(4, 5) +
  geom_text(aes(x = x, y = y, label = year, vjust = -1, hjust = 0.5))+
  theme_var +
  xlab("sucide rate (per 100k population)") + 
  ylab("unemployment") +
  labs( title = "Sucide rate vs Unemployment, Greece 2011-2015", fill = "Lines") 


