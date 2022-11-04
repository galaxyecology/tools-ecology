#' Statically map indicators using ggplot
#'
#' @param grid spatial features, e.g. hexagons, to plot; requires a geometry
#'   spatial column
#' @param column column name with indicator; default="shannon"
#' @param label label to show on legend
#' @param crs coordinate reference system; see `sf::st_crs()`
#' @param trans For continuous scales, the name of a transformation object or
#'   the object itself. Built-in transformations include "asn", "atanh",
#'   "boxcox", "date", "exp", "hms", "identity" (default), "log", "log10", "log1p",
#'   "log2", "logit", "modulus", "probability", "probit", "pseudo_log",
#'   "reciprocal", "reverse", "sqrt" and "time". See `ggplot2::continuous_scale`
#'
#' @return ggplot2 plot
#' @concept visualize
#' @export
#' @import rnaturalearth viridis ggplot2
#'
#' @examples
gmap_indicator <- function(
    grid, column = "shannon", label = "Shannon index", trans = "identity",
    crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") {

  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  bb <- sf::st_bbox(
    sf::st_transform(grid, crs))

  ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = grid, ggplot2::aes_string(
        fill = column, geometry = "geometry"), lwd = 0) +
    viridis::scale_color_viridis(
      option = "inferno", na.value = "white",
      name = label, trans = trans) +
    viridis::scale_fill_viridis(
      option = "inferno", na.value = "white",
      name = label, trans = trans) +
    ggplot2::geom_sf(
      data = world, fill = "#dddddd", color = NA) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank()) +
    ggplot2::xlab("") + ggplot2::ylab("") +
    ggplot2::coord_sf(
      crs  = crs,
      xlim = bb[c("xmin", "xmax")],
      ylim = bb[c("ymin", "ymax")])
}
