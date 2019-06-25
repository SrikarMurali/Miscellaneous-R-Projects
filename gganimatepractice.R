library(dplyr)
library(gganimate)
library(ggplot2)
library(readr)
library(gapminder)
theme_set(theme_bw())
gapminder <- read_tsv("gapminderDataFiveYear.tsv")
glimpse(gapminder)
gapminder_plot <- ggplot(gapminder) +
  aes(x = gdpPercap, y = lifeExp, colour = continent, size = pop,
      frame = year) +
  geom_point(alpha = 0.4) +
  scale_x_log10()
gapminder_plot
install.ImageMagick(URL = "http://www.imagemagick.org/script/binary-releases.php")
Sys.setenv(PATH = paste("C:/Program Files/ImageMagick/bin",
                        Sys.getenv("PATH"), sep = ";"))
#ani.options(convert = shQuote('C:/ProgramData/Microsoft/Windows/Start Menu/Programs/imagemagick/magick.exe'))
gganimate(gapminder_plot)

