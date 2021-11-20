# Necessary libraries for data manipulation and plotting: we use the Tidyverse for this
if (!require("tidyverse")) install.packages("tidyverse"); library("tidyverse")
if (!require("readxl"))    install.packages("readxl");    library("readxl")
if (!require("broom"))     install.packages("broom");     library("broom")

# Mapping tools
if (!require("rgdal"))    install.packages("rgdal");    library("rgdal")
if (!require("maptools")) install.packages("maptools"); library("maptools")

# Set to working directory
# setwd("N://Users/321244/AER_Figure_1/")

# Regions to inset: Local Authorities for each inset
LondonPlus <- c("barkinganddagenham","barnet","bexley","brent","bromley","buckinghamshire","camden",
                "cityoflondon","cityofwestminster","croydon","ealing","enfield","essex","greenwich",
                "hackney","hammersmithandfulham","haringey","harrow","havering","hillingdon","hounslow",
                "islington","kensingtonandchelsea","kent","kingstonuponthames","lambeth","lewisham",
                "merton","newham","redbridge","richmonduponthames","southwark","surrey","sutton",
                "towerhamlets","walthamforest","wandsworth","hertfordshire","thurrock")
NW         <- c("blackburnwithdarwen","bolton","bury","calderdale","cheshireeast","cheshirewestandchester",
                "derbyshire","halton","kirklees","knowsley","lancashire","liverpool","manchester","oldham",
                "rochdale","salford","sefton","sthelens","stockport","tameside","trafford","warrington",
                "wigan","wirral")
Birmingham <- c("birmingham","coventry","derbyshire","dudley","herefordshire","sandwell","shropshire",
                "solihull","staffordshire","leicestershire","walsall","warwickshire","wolverhampton",
                "worcestershire","wrekin")

# Read spatial data
gla  <- readOGR(dsn="Local_Authority_Districts_Dec_2015_Generalised_Clipped", 
                layer="Local_Authority_Districts_December_2015_Generalised_Clipped_Boundaries_in_Great_Britain")
# Region names used later to rearrange Land Registry data
dd   <- select(gla@data, code=lad15cd, name=lad15nm) %>% 
  mutate(code=as.character(code))

# Price data by Land Registry region
map_codes <- read_excel("house_price_data_figure_1.xls")

# long format
gg <- map_codes %>% 
  select("land_reg_region", starts_with("e_"), starts_with("av_")) %>% 
  gather(name, code, -land_reg_region, -starts_with("av_")) %>% 
  drop_na() %>%
  select("land_reg_region", "code", starts_with("av_")) 

# Join polygons defined by Land Registry regions
mcodes <- left_join(dd, gg, by=c("code"))
glalr  <- gla %>% 
  unionSpatialPolygons(mcodes$land_reg_region) 

glalrt <- tidy(glalr)

# Remove some of the Scottish islands
noislands <- glalrt %>% 
  filter(id != "orkneyislands") %>%
  filter(id != "shetlandislands") %>%
  filter(lat < 1000000)

# Join map data with inflation by region
noislands_growth <- left_join(noislands, 
                              select(map_codes, id="land_reg_region", starts_with("av_")), 
                              by=c("id")) %>% 
  rename_at(vars(starts_with("av_hp")), ~ c("Growth 02-07", "Growth 07-09", "Growth 09-14"))

ttl  <- "Average house price inflation" # Title 
wh   <- c("2002-2007", "2007-2009", "2009-2014") # Subtitle labels
hlim <- data.frame(l = c(5, -15, -2.5), h = c(20, -2.5, 12.5)) # Limits to trim outliers

ltyp <- 1
grp  <- c(1,1,1,1) # Dummy data for groups used in outlines

# Plot one era of inflation at a time
for (i in 1:3) {
  
  subttl     <- wh[i]
  sel_nois_g <- noislands_growth %>% 
    select(one_of(colnames(noislands_growth)[c(1:7,7+i)]))
  colnames(sel_nois_g)[8] <- "Av HP inflation"
  
  # Trim outliers by replacing with limit
  sel_nois_g <- sel_nois_g %>%
     mutate(`Av HP inflation` = if_else(`Av HP inflation` < hlim$l[i], hlim$l[i], `Av HP inflation`)) %>% 
     mutate(`Av HP inflation` = if_else(`Av HP inflation` > hlim$h[i], hlim$h[i], `Av HP inflation`))

  low <- floor(min(sel_nois_g$`Av HP inflation`, na.rm = TRUE))
  hig <- ceiling(max(sel_nois_g$`Av HP inflation`, na.rm = TRUE))

  # Plot whole UK (minus the islands removed above)
  gglr <- sel_nois_g %>% 
    ggplot(aes(x=long, y=lat, group=group)) + 
    geom_polygon(aes(fill=`Av HP inflation`), size=0.025, colour="grey88", show.legend=TRUE) +
    theme_void() +
    theme(legend.direction = "horizontal", 
          legend.position  = c(0.75,0.05),
          legend.title     = element_blank(), 
          plot.title       = element_text(hjust=1, vjust=-15), 
          plot.subtitle    = element_text(hjust=1, vjust=-17)) +
    coord_equal() + 
    scale_fill_gradient(low=grey(0.9), high=grey(0.05), limits=c(low, hig)) +
    labs(title=ttl, subtitle=subttl) +
    expand_limits(x = 740000)

  # London inset
  justl <- filter(sel_nois_g, id %in% LondonPlus)
  xl    <- c(505000, 555000)
  yl    <- c(152500, 202500)
  xminl <- xl[1] +  65000
  yminl <- yl[1] + 207500
  sxl   <- 4*(xl[2] - xl[1])
  syl   <- 4*(yl[2] - yl[1])
  long.lon    <- c(xl[1], xl[2], xl[2], xl[1])
  lat.lon     <- c(yl[1], yl[1], yl[2], yl[2])
  latlong.lon <- data.frame(long.lon, lat.lon, group=grp)
  
  # London grob
  lon   <- ggplot(data=justl, aes(x=long, y=lat, group=group, text=id)) + 
    geom_polygon(aes(fill=`Av HP inflation`), size=0.025, colour="grey88", show.legend=FALSE) +
    theme_void() +
    coord_cartesian(xlim=xl, ylim=yl, expand=FALSE) + 
    scale_fill_gradient(low=grey(0.9), high=grey(0.05), limits=c(low, hig)) +
    theme(panel.border = element_rect(linetype=ltyp, colour="grey22", fill=NA))
  longrob <- ggplotGrob(lon)

  # Birmingham inset
  justb <- filter(sel_nois_g, id %in% Birmingham) 
  xb    <- c(370000, 430000)
  yb    <- c(260000, 320000)
  xminb <- xb[1] - 345000
  yminb <- yb[1]
  sxb   <- 3*(xb[2] - xb[1])
  syb   <- 3*(yb[2] - yb[1])
  long.bir    <- c(xb[1], xb[2], xb[2], xb[1])
  lat.bir     <- c(yb[1], yb[1], yb[2], yb[2])
  latlong.bir <- data.frame(long.bir, lat.bir, group=grp)
  
  # Birmingham grob
  bir <- ggplot(data=justb, aes(x=long, y=lat, group=group)) + 
    geom_polygon(aes(fill=`Av HP inflation`), size=0.025, colour="grey88", show.legend=FALSE) +
    theme_void() +
    coord_cartesian(xlim=xb, ylim=yb, expand=FALSE) + 
    scale_fill_gradient(low = grey(0.9), high = grey(0.05), limits = c(low, hig)) +
    theme(panel.border = element_rect(linetype=ltyp, colour="grey22", fill = NA))
  birgrob <- ggplotGrob(bir)

  # NW inset
  justnw <- filter(sel_nois_g, id %in% NW)
  xnw    <- c(330000, 410000)
  ynw    <- c(375000, 420000)
  xminnw <- xnw[1] + 115000
  yminnw <- ynw[1] + 260000
  sxnw   <- 3*(xnw[2] - xnw[1])
  synw   <- 3*(ynw[2] - ynw[1])
  long.nw    <- c(xnw[1], xnw[2], xnw[2], xnw[1])
  lat.nw     <- c(ynw[1], ynw[1], ynw[2], ynw[2])
  latlong.nw <- data.frame(long.nw, lat.nw, group=grp)
  
  # North west grob
  nw     <- ggplot(data=justnw, aes(x=long, y=lat, group=group)) + 
    geom_polygon(aes(fill=`Av HP inflation`), size=0.025, colour="grey88", show.legend=FALSE) +
    theme_void() +
    coord_cartesian(xlim = xnw, ylim = ynw, expand=FALSE) + 
    scale_fill_gradient(low = grey(0.9), high = grey(0.05), limits = c(low, hig)) +
    theme(panel.border = element_rect(linetype=ltyp, colour="grey22", fill = NA))
  nwgrob <- ggplotGrob(nw)

  # PDF name 
  pdf(paste0("MAP0", i,".pdf"), width=7.5, height=9)
  # Add inset grobs
  gglr.ins <- gglr + 
    ## Lon
    annotation_custom(grob=longrob, xmin=xminl, xmax=xminl+sxl, ymin=yminl, ymax=yminl+syl) +
    geom_polygon(data=latlong.lon, aes(long.lon, lat.lon, group=group), 
                 fill=NA, color="grey22", linetype=ltyp, size=0.3, alpha=0.8) +
    annotate("segment", x = xl[1] + (xl[2]-xl[1])/2, y = yl[2], xend = xminl + sxl/2, yend = yminl, 
             colour = "grey22", size=0.3, alpha=0.8) +
    annotate("text", x = xminl, y= yminl + syl, label = "Greater London",
             colour = "grey22", size=3.5, alpha=0.8, hjust=0, vjust=-0.7) +
    ## Bir
    annotation_custom(grob=birgrob, xmin=xminb, xmax=xminb+sxb, ymin=yminb, ymax=yminb+syb) +
    geom_polygon(data=latlong.bir, aes(long.bir, lat.bir), 
                 fill=NA, color="grey22", linetype=ltyp, size=0.3, alpha=0.8) +
    annotate("segment", x=xb[1], y=yb[1]+(yb[2]-yb[1])/2, xend=xminb+sxb, yend=yminb+syb/2, 
             colour = "grey22", size=0.3, alpha=0.8) + 
    annotate("text", x=xminb, y=yminb+syb,label="Greater Birmingham",
             colour = "grey22", size=3.5, alpha=0.8, hjust=0, vjust=-0.7) +
    ## NW
    annotation_custom(grob=nwgrob, xmin=xminnw, xmax=xminnw+sxnw, ymin=yminnw, ymax=yminnw+synw) + 
    geom_polygon(data=latlong.nw, aes(long.nw, lat.nw), 
                 fill=NA, color="grey22", linetype=ltyp, size=0.3, alpha=0.8) +
    annotate("segment", x=xnw[1]+(xnw[2]-xnw[1])/2, y=ynw[2], xend=xminnw+sxnw/2, yend=yminnw, 
             colour = "grey22", size=0.3, alpha=0.8) +
    annotate("text", x=xminnw, y=yminnw + synw, label = "North West Urban",
             colour = "grey22", size=3.5, alpha=0.8, hjust=0, vjust=-0.7) 
  
  print(gglr.ins)
  dev.off()
}

gglr + 
  annotation_custom(grob=longrob, xmin=xminl, xmax=xminl+sxl, ymin=yminl, ymax=yminl+syl) +
  geom_polygon(data=latlong.lon, aes(long.lon, lat.lon, group=grp), 
               fill=NA, color="grey22", linetype=ltyp, size=0.25, alpha=0.8) # +
  #annotate("segment", x = xl[1] + (xl[2]-xl[1])/2, y = yl[2], xend = xminl + sxl/2, yend = yminl, 
  #         colour = "grey22", size=0.3, alpha=0.8) #+
  #annotate("text", x = xminl, y= yminl + syl, label = "Greater London",
  #         colour = "grey22", size=3.5, alpha=0.8, hjust=0, vjust=-0.7) 

xl    <- c(505000, 555000)
yl    <- c(152500, 202500)
justl <- filter(sel_nois_g, long <= xl[2] & long >= xl[1] & lat <= yl[2] & lat >= yl[1])
justl <- filter(sel_nois_g, id %in% unique(justl$id))

xminl <- xl[1] +  65000
yminl <- yl[1] + 207500
sxl   <- 4*(xl[2] - xl[1])
syl   <- 4*(yl[2] - yl[1])
lon   <- ggplot(data=justl, aes(x=long, y=lat, group=group)) + 
  geom_polygon(aes(fill=`Av HP inflation`), size=0.025, colour="grey88", show.legend=FALSE) +
  theme_void() +
  coord_cartesian(xlim=xl, ylim=yl, expand=FALSE) + 
  scale_fill_gradient(low=grey(0.9), high=grey(0.05), limits=c(low, hig)) +
  theme(panel.border = element_rect(linetype=ltyp, colour="grey22", fill=NA))
