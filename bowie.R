library(ggplot2)
library(plotly)
library(purrr)
library(spotifyr)
source('spotify.R')
library(compmus)
library(tidyr)

# Colors

bg_color <- 'grey95' # Equals '#F2F2F2'
plot_panel_color <- '#28BB9C'
text_color <- 'grey40' # Equals '#666666'
important_points_color <- 'black'
other_points_color <- 'white'

# Spotify data

bowie <- spotifyr::get_playlist_audio_features('joostvdm', '4L34OFuKhWwCB9akiVC2Ak')

# Remove remaster appendices from album titles, such as '(2015 Remaster)'

label_cleaner <- function(value, max_len) {
  stripped <- gsub(' \\(19.*', '', value)
  stripped <- gsub(' \\(20.*', '', stripped)
  stripped <- gsub(' \\[20.*', '', stripped)
  substr(stripped, 1, max_len)
}
max_number_of_characters <- 100
bowie$album <- label_cleaner(bowie$track.album.name, max_number_of_characters)

# Clean-up text data

text_cleaner <- function(text, first_chars_of_to_be_removed_part, max_len) {
  stripped <- sub(paste0(first_chars_of_to_be_removed_part, '.*'), '', text)
  substr(stripped, 1, max_len)
}
bowie$year <- text_cleaner(bowie$track.album.release_date, '-', 4)
bowie$track <- text_cleaner(bowie$track.name, ' -', 100)

# Annotate Berlin Trilogy

trilogy_vec <-  purrr::map_chr(bowie$album,
                               ~ ifelse((. == '\"Heroes\"' | . == 'Low' | . == 'Lodger'),
                                        'Berlin Trilogy', 'Other'))
bowie <- bowie %>%
  mutate(trilogy = trilogy_vec,
         tri_within = (bowie$track == 'Sense of Doubt' | bowie$track == 'Moss Garden' |
                         bowie$track == 'Neuköln')) %>%
  mutate(trilogy = factor(trilogy, levels = c('Berlin Trilogy','Other'), ordered = TRUE))

# Save processed data

saveRDS(bowie, file = 'bowie.Rds')

# Scatterplot

bowie_scatterplot <- function(x_aes, y_aes, xlims) {
  xlims <- c(0, 1)
  ylims <- c(0, 1)
  p <- bowie %>%
    ggplot(aes(x = eval(as.symbol(x_aes)),
               y = eval(as.symbol(y_aes)),
               colour = trilogy,
               alpha = trilogy,
               text = paste0(track, '\n',
                             album, ' ', year, '\n',
                             format(eval(as.symbol(x_aes)),
                                    scientific = FALSE, drop0trailing = TRUE),
                             ' ', x_aes, '\n',
                             format(eval(as.symbol(y_aes)),
                                    scientific = FALSE, drop0trailing = TRUE),
                             ' ', y_aes, '\n'))) +
    # Limits
    xlim(xlims[1], xlims[2]) +
    ylim(ylims[1], ylims[2]) +
    # Points
    scale_alpha_ordinal(range = c(1, 0.6)) +
    scale_colour_manual(values = c(important_points_color, other_points_color)) +
    geom_point(data = bowie,
               fill = FALSE,
               position = 'jitter',
               size = 1) +
    # Triangle
    geom_path(data = rbind(bowie[bowie$tri_within,],
                           head(bowie[bowie$tri_within,], 1)),
              inherit.aes = FALSE,
              aes(x = eval(as.symbol(x_aes)),
                  y = eval(as.symbol(y_aes))),
              colour = important_points_color,
              size = 0.2) +
    # Re-render black points on top of white points
    geom_point(data = bowie[bowie$trilogy == 'Berlin Trilogy', , drop = FALSE],
               size = 1) +
    # Hairlines along borders to show marginal distributions
    geom_rug(data = bowie[bowie$trilogy == 'Berlin Trilogy', , drop = FALSE],
             inherit.aes = FALSE,
             aes(x = eval(as.symbol(x_aes)),
                 y = eval(as.symbol(y_aes))),
             colour = important_points_color,
             sides = 'lb',
             size = 0.1) +
    geom_rug(data = bowie[bowie$trilogy == 'Other', , drop = FALSE],
             inherit.aes = FALSE,
             aes(x = eval(as.symbol(x_aes)),
                 y = eval(as.symbol(y_aes))),
             colour = other_points_color,
             sides = 'tr',
             size = 0.1) +
    # Axis labels
    labs(x = x_aes, y = y_aes) +
    # Theme
    theme(axis.title = element_text(family = 'monospace', colour = text_color),
          axis.ticks = element_line(colour = text_color),
          axis.text = element_text(colour = text_color),
          plot.background = element_rect(fill = bg_color),
          legend.background = element_rect(fill = plot_panel_color),
          panel.background = element_rect(fill = plot_panel_color),
          legend.text = element_text(colour = 'black'),
          panel.grid = element_blank()) +
    # Legend
    guides(alpha = 'none',
           colour = guide_legend(title = NULL))
}