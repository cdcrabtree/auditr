#--- Get surname probabilities ---#
#'@title Classify surnames based on race and county demographics
#'@description Returns an object that classifies inputted surnames according to race and county demographics.
#'@author Charles Crabtree \email{ccrabtr@umich.edu}
#'@param family A vector of family names (i.e. surnames or last names).
#'@param key A Census API Key. This is typically a long string of mixed-case letters and numbers.
#'@return An object that classifies inputted surnames according to race and county demographics.
#'@examples
#' \dontrun{
#' family <- c("Chykina", "Crabtree", "Duck")
#' key <- "45b2kjsskd2335435kkmfdksmfkko"
#' names_probabilities(family, key)
#' }
#'@importFrom utils data
#'@export

names_probabilities <- function(family, key){
  data(sysdata, envir=environment())

  test_names <- merge(last_names, geo)

  colnames(test_names) <- c("surname", "state", "county")

  space_name_pred <- wru::predict_race(voter.file = test_names, census.geo = "county", census.key = key, census.surname = T)

  plot.whi <- ggplot2::ggplot(space_name_pred, ggplot2::aes(x = pred.whi, y = surname, group = surname, fill = surname)) +
    ggridges::geom_density_ridges(scale = 3, size = 0.20, rel_min_height = 0.03,
                                  alpha = .8, color = "white") +
    ggplot2::scale_fill_manual(values=rep(c('#fc8d62'), length(unique(space_name_pred$surname)))) +
    ggridges::theme_ridges() +
    ggplot2::theme(axis.title.y = element_blank(), legend.position='none') +
    ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0.03, 0)) +
    ggplot2::xlab("P(White | Surname, Location)")

  plot.bla <- ggplot(space_name_pred, ggplot2::aes(x = pred.bla, y = surname, group = surname, fill = surname)) +
    ggridges::geom_density_ridges(scale = 3, size = 0.20, rel_min_height = 0.03,
                                  alpha = .8, color = "white") +
    ggplot2::scale_fill_manual(values=rep(c('#8da0cb'), length(unique(space_name_pred$surname)))) +
    ggridges::theme_ridges() +
    ggplot2::theme(axis.title.y = element_blank(), legend.position='none') +
    ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0.03, 0)) +
    ggplot2::xlab("P(Black | Surname, Location)")

  plot.his <- ggplot(space_name_pred, ggplot2::aes(x = pred.his, y = surname, group = surname, fill = surname)) +
    ggridges::geom_density_ridges(scale = 3, size = 0.20, rel_min_height = 0.03,
                                           alpha = .8, color = "white") +
    ggplot2::scale_fill_manual(values=rep(c('#66c2a5'), length(unique(space_name_pred$surname)))) +
    ggridges::theme_ridges() +
    ggplot2::theme(axis.title.y = element_blank(), legend.position='none') +
    ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0.03, 0)) +
    ggplot2::xlab("P(Hispanic | Surname, Location)")

  plot.asi <- ggplot(space_name_pred, ggplot2::aes(x = pred.asi, y = surname, group = surname, fill = surname)) +
    ggridges::geom_density_ridges(scale = 3, size = 0.20, rel_min_height = 0.03,
                                  alpha = .8, color = "white") +
    ggplot2::scale_fill_manual(values=rep(c('#e78ac3'), length(unique(space_name_pred$surname)))) +
    ggridges::theme_ridges() +
    ggplot2::theme(axis.title.y = element_blank(), legend.position='none') +
    ggplot2::xlab("P(Asian | Surname, Location)")

  gridExtra::grid.arrange(plot.whi, plot.bla, plot.his, plot.asi, ncol=2)
}
