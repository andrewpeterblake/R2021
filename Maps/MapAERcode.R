library("tidyverse")
library("readxl")
library("broom")

# Mapping tools
library("rgdal")
library("rgeos")
library("maptools")

gla  <- readOGR(dsn="Local_Authority_Districts_(December_2015)_Boundaries", 
                layer="Local_Authority_Districts_(December_2015)_Boundaries",
                verbose=FALSE)

plot(gla, bg="grey77", col="lightblue4", border=NA)
summary(gla)

glat <- tidy(gla, region="lad15nm")
head(glat)

glat %>% 
  ggplot(aes(x=long, y=lat, group=group)) + 
    geom_polygon(fill="lightblue2", colour=NA, show.legend=FALSE) +
    theme_void() +
    coord_equal()

glat %>% 
  filter(id != "Orkney Islands", id != "Shetland Islands") %>%
  ggplot(aes(x=long, y=lat, group=group)) + 
    geom_polygon(fill="lightblue2", colour=NA, show.legend=FALSE) +
    theme_void() +
    coord_equal()

glat %>% 
  filter(id != "Orkney Islands", id != "Shetland Islands") %>%
  ggplot(aes(x=long, y=lat, group=group)) + 
    geom_polygon(aes(fill=id), colour="grey22", size=0.005, show.legend=FALSE) +
    theme_void() +
    coord_equal()

### Inflation data and regions

# Price data by Land Registry region, converted to long format

gg <- read_excel("house_price_data_figure_1.xls")  %>% 
  select("land_reg_region", starts_with("e_"), starts_with("av_")) %>% 
  pivot_longer(names_to = "name", 
               values_to = "code", 
               cols= c(-land_reg_region, -starts_with("av_"))) %>% 
  drop_na() %>%
  select("land_reg_region", "code", starts_with("av_"))
head(gg)

filter(gg, land_reg_region == "buckinghamshire")

# Region names used to rearrange Land Registry data
dd <- select(gla@data, code=lad15cd, name=lad15nm)
head(dd)

# Join polygons defined by Land Registry regions
mcodes <- left_join(dd, gg, by=c("code"))

filter(mcodes, land_reg_region == "buckinghamshire")

glalrt <- gla %>% 
  unionSpatialPolygons(mcodes$land_reg_region) %>% 
  tidy() %>% 
  filter(id != "orkneyislands", id != "shetlandislands") 

glalrt %>%
  ggplot(aes(x=long, y=lat, group=group)) + 
    geom_polygon(aes(fill=id), colour="grey22", size=0.005, show.legend=FALSE) +
    theme_void() +
    coord_equal()


## Inflation in grayscale

glalrta <- glalrt %>% 
  left_join(select(mcodes, id="land_reg_region", starts_with("av_")), by=c("id")) %>% 
  rename_at(vars(starts_with("av_hp")), ~ c("Growth 02-07", "Growth 07-09", "Growth 09-14"))

low <- floor(min(glalrta$`Growth 02-07`, na.rm = TRUE))
hig <- ceiling(max(glalrta$`Growth 02-07`, na.rm = TRUE))

# Plot whole GB (minus the islands removed above)
plt <- glalrta %>% 
  ggplot(aes(x=long, y=lat, group=group)) + 
  geom_polygon(aes(fill=`Growth 02-07`), size=0.005, colour="grey88", show.legend=TRUE) +
  theme_void() +
  coord_equal() + 
  scale_fill_gradient(low=grey(0.9), high=grey(0.05), limits=c(low, hig)) +
  theme(legend.direction = "horizontal", 
        legend.position  = c(0.75,0.05),
        legend.title     = element_blank()) +
    expand_limits(x = 740000)
plot(plt)

## Adding detail

xl    <- c(505000, 555000)
yl    <- c(152500, 202500)
justl <- filter(glalrta, long <= xl[2] & long >= xl[1] & lat <= yl[2] & lat >= yl[1])
justl <- filter(glalrta, id %in% unique(justl$id))

lon   <- ggplot(data=justl, aes(x=long, y=lat, group=group)) + 
  geom_polygon(aes(fill=`Growth 02-07`), size=0.005, colour="grey88", show.legend=FALSE) +
  theme_void() +
  scale_fill_gradient(low=grey(0.9), high=grey(0.05), limits=c(low, hig))
lon

lon   <- ggplot(data=justl, aes(x=long, y=lat, group=group)) + 
  geom_polygon(aes(fill=`Growth 02-07`), size=0.005, colour="grey88", show.legend=FALSE) +
  theme_void() +
  scale_fill_gradient(low=grey(0.9), high=grey(0.05), limits=c(low, hig)) +
  coord_cartesian(xlim=xl, ylim=yl, expand=FALSE) + 
  theme(panel.border = element_rect(linetype=1, colour="grey22", fill=NA))
lon

longrob <- ggplotGrob(lon)

xminl <- xl[1] +  65000
yminl <- yl[1] + 207500
sxl   <- 4*(xl[2] - xl[1])
syl   <- 4*(yl[2] - yl[1])

long.lon    <- c(xl[1], xl[2], xl[2], xl[1])
lat.lon     <- c(yl[1], yl[1], yl[2], yl[2])
latlong.lon <- data.frame(long.lon, lat.lon, grp=c(1,1,1,1)) # Dummy group data

plt2 <- plt + 
  annotation_custom(grob=longrob, xmin=xminl, xmax=xminl+sxl, ymin=yminl, ymax=yminl+syl) +
  geom_polygon(data=latlong.lon, aes(long.lon, lat.lon, group=grp), 
               fill=NA, color="grey22", linetype=1, size=0.025, alpha=0.8) +
  annotate("segment", x = xl[1] + (xl[2]-xl[1])/2, y = yl[2], xend = xminl + sxl/2, yend = yminl, 
           colour = "grey22", size=0.3, alpha=0.8) +
  annotate("text", x = xminl, y= yminl + syl, label = "Greater London",
           colour = "grey22", size=2.5, alpha=0.8, hjust=0, vjust=-0.7) 
plt2

# Birmingham inset
xb    <- c(370000, 430000)
yb    <- c(260000, 320000)
  
justb <- filter(glalrta, long <= xb[2] & long >= xb[1] & lat <= yb[2] & lat >= yb[1])
justb <- filter(glalrta, id %in% unique(justb$id))

xminb <- xb[1] - 345000
yminb <- yb[1]
sxb   <- 3*(xb[2] - xb[1])
syb   <- 3*(yb[2] - yb[1])
long.bir    <- c(xb[1], xb[2], xb[2], xb[1])
lat.bir     <- c(yb[1], yb[1], yb[2], yb[2])
latlong.bir <- data.frame(long.bir, lat.bir, grp=c(1,1,1,1))
  
# Birmingham grob
bir <- ggplot(data=justb, aes(x=long, y=lat, group=group)) + 
  geom_polygon(aes(fill=`Growth 02-07`), size=0.025, colour="grey88", show.legend=FALSE) +
  theme_void() +
  coord_cartesian(xlim=xb, ylim=yb, expand=FALSE) + 
  scale_fill_gradient(low = grey(0.9), high = grey(0.05), limits = c(low, hig)) +
  theme(panel.border = element_rect(linetype=1, colour="grey22", fill = NA))
birgrob <- ggplotGrob(bir)

# NW inset
xnw    <- c(330000, 410000)
ynw    <- c(375000, 420000)

justnw <- filter(glalrta, long <= xnw[2] & long >= xnw[1] & lat <= ynw[2] & lat >= ynw[1])
justnw <- filter(glalrta, id %in% unique(justnw$id))

xminnw <- xnw[1] + 115000
yminnw <- ynw[1] + 260000
sxnw   <- 3*(xnw[2] - xnw[1])
synw   <- 3*(ynw[2] - ynw[1])
long.nw    <- c(xnw[1], xnw[2], xnw[2], xnw[1])
lat.nw     <- c(ynw[1], ynw[1], ynw[2], ynw[2])
latlong.nw <- data.frame(long.nw, lat.nw, grp=c(1,1,1,1))
  
# North west grob
nw     <- ggplot(data=justnw, aes(x=long, y=lat, group=group)) + 
  geom_polygon(aes(fill=`Growth 02-07`), size=0.025, colour="grey88", show.legend=FALSE) +
  theme_void() +
  coord_cartesian(xlim = xnw, ylim = ynw, expand=FALSE) + 
  scale_fill_gradient(low = grey(0.9), high = grey(0.05), limits = c(low, hig)) +
  theme(panel.border = element_rect(linetype=1, colour="grey22", fill = NA))
nwgrob <- ggplotGrob(nw)

library(patchwork)
nw/(bir+lon)

## All together

plt3 <- plt2 + 
    ## Birmingham
    annotation_custom(grob=birgrob, xmin=xminb, xmax=xminb+sxb, ymin=yminb, ymax=yminb+syb) +
    geom_polygon(data=latlong.bir, aes(long.bir, lat.bir, group=grp), 
                 fill=NA, color="grey22", linetype=1, size=0.3, alpha=0.8) +
    annotate("segment", x=xb[1], y=yb[1]+(yb[2]-yb[1])/2, xend=xminb+sxb, yend=yminb+syb/2, 
             colour = "grey22", size=0.3, alpha=0.8) + 
    annotate("text", x=xminb, y=yminb+syb,label="Greater Birmingham",
             colour = "grey22", size=2.5, alpha=0.8, hjust=0, vjust=-0.7) +
    ## NW
    annotation_custom(grob=nwgrob, xmin=xminnw, xmax=xminnw+sxnw, ymin=yminnw, ymax=yminnw+synw) + 
    geom_polygon(data=latlong.nw, aes(long.nw, lat.nw, group=grp), 
                 fill=NA, color="grey22", linetype=1, size=0.3, alpha=0.8) +
    annotate("segment", x=xnw[1]+(xnw[2]-xnw[1])/2, y=ynw[2], xend=xminnw+sxnw/2, yend=yminnw, 
             colour = "grey22", size=0.3, alpha=0.8) +
    annotate("text", x=xminnw, y=yminnw + synw, label = "North West Urban",
             colour = "grey22", size=2.5, alpha=0.8, hjust=0, vjust=-0.7) 

plt3
