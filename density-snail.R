library(gganimate)
library(data.table)

# Download and unzip the current data from here:
# https://www.owid.de/plus/cowidplus2020/data/unigram-results.Rds.zip

res <- readRDS("~/unigram-results.Rds")

# This is the tricky part: I am realizing the moving window as follows:
# - Iterate through the rows of the dataframe and
# - (if we have still 30 rows left to select) select the following 30 rows
# - Get the 'begin' date of this window
# - (from.to is not necessary)

# The result is dataframe that holds a LOT OF rows multiple times
# because the transition_time function can only select one 'begin' value
# from the dataframe.

max.rows <- nrow(res) - 30
slw.df <- lapply(1:nrow(res), FUN = function (di) {
  if (di < max.rows) {
    ret <- res[di:(di+30),]
    ret$begin <- as.Date(paste(min(ret$date)))
    ret$from.to <- paste(min(ret$date), "to", max(ret$date))
    ret
  }
})
slw.df <- rbindlist(slw.df)

# Plotting...

gg <- ggplot(slw.df, aes(x = top100share, fill = factor(begin))) +
  geom_density(alpha = .5) +
  transition_time(begin) +
  ease_aes('cubic-in-out') +
  labs(title='30 days since {frame_time}', x = "", y = "Kernel density top100 frequency share") +
  shadow_mark(alpha = alpha/4, color = alpha("grey", .25)) +
  guides(fill = F) +
  theme(title = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))

out.dir <- "~"

# Rendering...

# For a quick preview to see if everything works as it should
animate(gg, fps = 5, duration = 10, width = 500*2, height = 350*2, rewind = F,
        renderer = av_renderer(file = paste0(out.dir, "/test.mp4")))

# For a 30 sec movie
animate(gg, fps = 25, duration = 30, width = 500*2, height = 350*2, rewind = F,
        renderer = av_renderer(file = paste0(out.dir, "/long.mp4")))

# For a 20 sec movie
animate(gg, fps = 25, duration = 20, width = 500*2, height = 350*2, rewind = F,
        renderer = av_renderer(file = paste0(out.dir, "/short.mp4")))