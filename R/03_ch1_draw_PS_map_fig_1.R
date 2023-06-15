#### ch1_3_draw_PS_map_fig_1.R ####

## load packages

library(here)
library(rgdal)
library(broom)
library(ggplot2)
library(cowplot)
library(ggspatial)
library(grid)
library(patchwork)
library(png)

## figure save dir
fig_dir <- here("figures")

## load habitat pictures for multiplot
clam <- readPNG(here("data", "clean_data", "map_files","clam_net.png"),
                native = TRUE)
eelgrass <- readPNG(here("data", "clean_data", "map_files","eelgrass.png"),
                    native = TRUE)
flipbag <- readPNG(here("data", "clean_data", "map_files","flipbag.png"),
                   native = TRUE)
mud_flat <- readPNG(here("data", "clean_data", "map_files","mud_flat.png"),
                    native = TRUE)
oyster_on_bot <- readPNG(here("data", "clean_data", "map_files","oyster_on_bot.png"),
                         native = TRUE)



#### base map of Puget Sound ####

## load USA shape file
usa_spdf <- readOGR(dsn = here("data", "clean_data", "map_files", "USA_adm0.shp"))
## convert to df(ish)
usa_spdf_fort <- tidy(usa_spdf)


## draw puget sound map and add annotations etc
puget_sound<-ggplot(usa_spdf_fort, aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "gray50", fill = "#d9d9d9") +
  ylab("Latitude") +
  xlab("Longitude") +
  coord_fixed(xlim = c(-123.35, -121.92), ylim = c(46.95, 49.0), ratio = 1.3) +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill="#c6dbef", color = "black"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  scale_x_continuous(breaks = c(-123, -122.5),
                     expand = c(0, 0),
                     labels=c(expression(paste(123*degree,"W")),
                              expression(paste(122.5*degree,"W")))) +
  scale_y_continuous(breaks = seq(47.5, 48.5, 0.5),
                     expand = c(0, 0),
                     labels=c(expression(paste(47.5*degree,"N")),
                              expression(paste(48*degree,"N")),
                              expression(paste(48.5*degree,"N")))) +
  annotate("point", 
           x = -122.77, 
           y = 48.98,
           size = 1.5,
           color = "#e41a1c")+ 
  annotate("text", 
           label = "FB, EG", 
           x = -122.63, 
           y = 48.97,
           size = 3,
           color = "black")+
  annotate("point", 
           x = -122.5, 
           y = 48.59,
           size = 1.5,
           color = "#e41a1c")+ 
  annotate("point", 
           x = -122.45, 
           y = 48.61,
           size = 1.5,
           color = "#e41a1c")+ 
  annotate("text", 
           label = "FB, CL, EG, MD", 
           x = -122.22, 
           y = 48.62,
           size = 3,
           color = "black")+
  annotate("text", 
           label = "FB, CL, OB, EG, MD", 
           x = -122.19, 
           y = 48.56,
           size = 3,
           color = "black")+
  annotate("point", 
           x = -122.95, 
           y = 47.11,
           size = 1.5,
           color = "#e41a1c")+ 
  annotate("text", 
           label = "FB, CL, MD", 
           x = -123.14, 
           y = 47.18,
           size = 3,
           color = "black")+
  annotate("point", 
           x = -122.95, 
           y = 47.14,
           size = 1.5,
           color = "#e41a1c")+ 
  annotate("text", 
           label = "FB, MD", 
           x = -123.12, 
           y = 47.07,
           size = 3,
           color = "black")+
  annotate("point", 
           x = -123.02, 
           y = 47.55,
           size = 1.5,
           color = "#e41a1c")+ 
  annotate("point", 
           x = -122.88, 
           y = 47.7,
           size = 1.5,
           color = "#e41a1c")+ 
  annotate("text", 
           label = "CL, EG", 
           x = -123.0, 
           y = 47.7,
           size = 3,
           color = "black")+
  annotate("text", 
           label = "     FB, CL, OB 
      EG, MD", 
           x = -123.2, 
           y = 47.55,
           size = 3,
           color = "black")+
  annotate("text", 
         label = "Seattle",
         x = -122.14, 
         y = 47.6,
         size = 5,
         color = "black")+ 
  annotate("text", 
           label = "North Puget Sound",
           x = -122.3, 
           y = 48.83,
           size = 4,
           color = "black")+ 
  annotate("text", 
           label = "South Puget Sound",
           x = -122.708, 
           y = 47,
           size = 4,
           color = "black")+ 
  annotate("text", 
           label = "Hood Canal",
           x = -123.078546, 
           y = 47.78,
           size = 4,
           color = "black")+ 
  annotate("point", 
           x = -122.32, 
           y = 47.6,
           size = 2,
           color = "black")+ 
annotation_north_arrow(
    location = "br", which_north = "grid",
    pad_x = unit(0.1, "in"), pad_y = unit(0.4, "in"),
    style = north_arrow_orienteering(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book"))
  
## combine maps
## combine all into one plot (figure 1)
  patch<-wrap_elements(puget_sound)+
    (wrap_elements(eelgrass)/
       wrap_elements(mud_flat)/
       wrap_elements(flipbag)/
       wrap_elements(clam)/
       wrap_elements(oyster_on_bot))+ 
    plot_layout(tag_level = 'new') +
    plot_annotation(tag_levels = list(c(' ','EG','MD',"FB","CL","OB"))) & 
    theme(plot.tag.position = c(0.15, 0.5) ,
          plot.tag = element_text(size = 14, hjust = 0, vjust = -1))
  
  
  
  
## save the combined file as figure 1
  ggsave(filename = file.path(fig_dir, "fig_01_PS_map.png"), 
         plot = patch,
         width = 9, 
         height = 8,
         dpi = 300)
  
  