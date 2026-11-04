
library(sf)
library(lwgeom)
library(dplyr)
library(ggplot2)
library(ggview)
library(readr)
library(lubridate)
library(cowplot)
source("~/theme.R")

party_cols <- c(
  NAT = "#00529F",
  LAB   = "#E31B23",
  ACT      = "#FFD200",
  GRN    = "#00A651",
  TPM         = "#B22222",
  Placeholder = "#555555",
  `Purple Placeholder` = "purple"
)

# https://datafinder.stats.govt.nz/layer/122741-general-electorates-2025/
general_electorate <- st_read("general-electorates-2025/general-electorates-2025.gpkg")

# https://datafinder.stats.govt.nz/layer/122742-maori-electorates-2025/
maori_electorate <- st_read("maori-electorates-2025/maori-electorates-2025.gpkg")

# https://data.linz.govt.nz/layer/51153-nz-coastlines-and-islands-polygons-topo-150k/
coast <- st_read("../kx-nz-coastlines-and-islands-polygons-topo-150k-GPKG/nz-coastlines-and-islands-polygons-topo-150k.gpkg")

# defensive "just in case" not valid
general_electorate <- st_make_valid(general_electorate)
maori_electorate <- st_make_valid(maori_electorate)
coast <- st_make_valid(coast)

land <- coast |>
  st_union()

general_electorate_land <- st_intersection(
  general_electorate,
  land
)

maori_electorate_land <- st_intersection(
  maori_electorate,
  land
)

## Getting geographic file details, checking CRS, structure etc
## Not part of core script, used in development

summarise_sf <- function(x) {
  tibble(
    features = nrow(x),
    geometry_types = paste(unique(st_geometry_type(x)), collapse = ", "),
    crs = st_crs(x)$input,
    xmin = st_bbox(x)["xmin"],
    ymin = st_bbox(x)["ymin"],
    xmax = st_bbox(x)["xmax"],
    ymax = st_bbox(x)["ymax"]
  )
}

glimpse(general_electorate)
summarise_sf(general_electorate)

glimpse(maori_electorate)
summarise_sf(maori_electorate)

glimpse(coast)
summarise_sf(coast)

####
# Party results for each electorate
# as this may get opened in Excel, using simple ASCII names

genelc <- read_csv("gen.csv")
maoelc <- read_csv("maori.csv")

general_plot <- general_electorate_land |> 
  inner_join(genelc)
maori_plot <- maori_electorate_land |> 
  inner_join(maoelc)
  
#########
# subareas

# Auckland setup
ak_x <- c(174.122818, 176.428392)
ak_y <- c(-37.8, -36.425142)

ak_box <- st_as_sfc(
  st_bbox(
    c(
      xmin = ak_x[1],
      xmax = ak_x[2],
      ymin = ak_y[1],
      ymax = ak_y[2]
    ),
    crs = 4326
  )
)

# Christchurch setup
ch_x <- c(172.05, 172.9191)
ch_y <- c(-44, -43.3651)

ch_box <- st_as_sfc(
  st_bbox(
    c(
      xmin = ch_x[1],
      xmax = ch_x[2],
      ymin = ch_y[1],
      ymax = ch_y[2]
    ),
    crs = 4326
  )
)

# Wellington Setup
wl_x <- c(174.5700, 175.7922)
wl_y <- c(-41.6750, -40.6160)

wl_box <- st_as_sfc(
  st_bbox(
    c(
      xmin = wl_x[1],
      xmax = wl_x[2],
      ymin = wl_y[1],
      ymax = wl_y[2]
    ),
    crs = 4326
  )
)

# Tāmaki Makaurau 
tm_x <- c(174.122818, 175.428392)
tm_y <- c(-37.1, -36.425142)

tm_box <- st_as_sfc(
  st_bbox(
    c(
      xmin = tm_x[1],
      xmax = tm_x[2],
      ymin = tm_y[1],
      ymax = tm_y[2]
    ),
    crs = 4326
  )
)

# Make maps and subsection them
gen_countrywide <- ggplot() +
  geom_sf(data = general_plot, aes(fill = Party), colour = "white", linewidth=.05)  +
  geom_sf(
    data = ak_box,
    fill = NA,
    colour = six_cols[1],
    linewidth = 0.3
  )  +
  geom_sf(
    data = wl_box,
    fill = NA,
    colour = six_cols[2],
    linewidth = 0.3
  )  +
  geom_sf(
    data = ch_box,
    fill = NA,
    colour = six_cols[3],
    linewidth = 0.3
  ) +
  theme_void() +
  geom_text(
    aes(x = 165, y = -41.3),
    label = "General\nElectorates",
    size = 4
  ) +
  coord_sf(
    xlim = c(150, 195),
    ylim = c(-48, -33.5),
    crs = 4326,
    expand = FALSE
  ) +
  scale_fill_manual(values = party_cols)

inset_ak <- gen_countrywide +
  coord_sf(
    xlim = ak_x,
    ylim = ak_y,
    crs = 4326,
    expand = FALSE
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(
      colour = six_cols[1],
      fill = NA,
      linewidth = 0.8
    ),
    legend.position ="None"
  )

# Central detail
inset_wl <- gen_countrywide +
  coord_sf(
    xlim = wl_x,
    ylim = wl_y,
    crs = 4326,
    expand = FALSE
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(
      colour = six_cols[2],
      fill = NA,
      linewidth = 0.8
    ),
    legend.position ="None"
  )

# Canterbury detail
inset_ch <- gen_countrywide +
  coord_sf(
    xlim = ch_x,
    ylim = ch_y,
    crs = 4326,
    expand = FALSE
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(
      colour = six_cols[3],
      fill = NA,
      linewidth = 0.8
    ),
    legend.position ="None"
  )

###########
maoribase <- ggplot() +
  geom_sf(data = maori_plot, aes(fill = Party), colour = "white", linewidth=.1)  +
  geom_sf(
    data = tm_box,
    fill = NA,
    colour = six_cols[4],
    linewidth = 0.3
  )  +
  theme_void() +
  theme(legend.position="None") +
  geom_text(
    aes(x = 174, y = -46),
    label = "Māori\nElectorates",
    size = 4
  ) +
  coord_sf(
    xlim = c(165, 180),
    ylim = c(-49, -30),
    crs = 4326,
    expand = FALSE
  )+
  scale_fill_manual(values = party_cols)

inset_tm <- maoribase +
  coord_sf(
    xlim = tm_x,
    ylim = tm_y,
    crs = 4326,
    expand = FALSE
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(
      colour = six_cols[4],
      fill = NA,
      linewidth = 0.8
    ),
    legend.position ="None"
  )

maorimap <-
  ggdraw() +
  draw_plot(maoribase) +
  draw_plot(
    inset_tm,
    x = 0.62,
    y = 0.62,
    width = 0.37,
    height = 0.37
  )

final_map <-
  ggdraw() +
  draw_plot(gen_countrywide) +
  draw_plot(
    inset_ak,
    x = 0.01,
    y = 0.59,
    width = 0.29,
    height = 0.40
  ) +
  draw_plot(
    inset_wl,
    x = 0.01,
    y = 0.37,
    width = 0.2,
    height = 0.20
  ) +
  draw_plot(
    inset_ch,
    x = 0.01,
    y = 0.14,
    width = 0.2,
    height = 0.20
  ) +
  draw_plot(
    maorimap,
    x = 0.45,
    y = 0.15,
    width = 0.3,
    height = .8
  ) +
  canvas(
    width=2016,
    height=1134,
    units = "px",
    dpi = 300,
    scale = 1,
    bg = "white"
  )


final_map

