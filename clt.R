library(dplyr)
library(tidyr)
library(ggplot2)
library(gganimate)
library(knitr)

# set seed
set.seed(123)

# knitr chunk options
opts_chunk$set(echo = TRUE,
               eval = TRUE,
               include = TRUE,
               warning = FALSE,
               message = FALSE,
               cache = TRUE,
               cache.extra = rand_seed,
               fig.width = 7,
               fig.height = 7,
               tidy = FALSE)
# Generate formal distribution of mean = 0, sd = 1
norm.df <- rnorm(200000, mean = 0, sd = 1) %>%
  data_frame(values = .)

# Calculate density from norm.df
norm.dens <- density(norm.df$values)
norm.dens <- data_frame(x = norm.dens$x, y = norm.dens$y)

# Calculate quantiles from norm.df
q2.5 <- quantile(norm.df$values, 0.025)
q97.5 <- quantile(norm.df$values, 0.975)
q16 <- quantile(norm.df$values, 0.16)
q84 <- quantile(norm.df$values, 0.84)

# Use quantiles to filter data for geom_ribbon
dens_95 <- norm.dens %>%
  filter(x > q2.5 & x < q97.5) %>%
  mutate(quant = rep('2SD', length(x)))
dens_68 <- norm.dens %>%
  filter(x > q16 & x < q84) %>%
  mutate(quant = rep('1SD', length(x)))
ribbon <- do.call(rbind, list(dens_95, dens_68))

# Plots
norm.plot <- ggplot() +
  geom_density(data = norm.df, aes(x = values),
               fill = '#FFFFFF') +
  geom_ribbon(data = ribbon[ribbon$quant == '2SD', ],
              aes(x = x, ymax = y, ymin = 0), 
              fill = '#00517f') +
  geom_ribbon(data = ribbon[ribbon$quant == '1SD', ],
              aes(x = x, ymax = y, ymin = 0),
              fill = '#0072B2') +
  geom_density(data = norm.df, aes(x = values),
               size = 1) +
  geom_segment(aes(x = -1, xend = 1,
                   y = 0.22, yend = 0.22), 
               colour = "#FFFFFF", size = 1.5) +
  geom_label(aes(label = '1 SD: 68%', x = 0, y = 0.22), 
             colour = 'gray10', fill = '#FFFFFF') +
  geom_segment(aes(x = -1.96, xend = 1.96,
                   y = 0.05, yend = 0.05), 
               colour = "#FFFFFF", size = 1.5) +
  geom_label(aes(label = '2 SD: 95%', x = 0, y = 0.05), 
             colour = 'gray10', fill = '#FFFFFF') +
  scale_x_continuous(breaks = seq(from = -4, to = 4, by = 1)) +
  labs(x = '\nStandard deviation (SD)', y = 'Density\n') +
  theme(plot.title = element_text(size = 17), 
        axis.text = element_text(size = 17),
        axis.title = element_text(size = 17),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = 'gray80', size = 0.3),
        panel.background = element_blank())
# Save plot
ggsave('normal-distr.png', norm.plot, width = 7, height = 5)


prec_data <- rnorm(200000, mean = 0, sd = 1)
foo <- list() # empty list
bar <- seq(from = 10, to = 1000, by = 20) # sample sizes
for(i in 1:50) {
  foo[[i]] <- sample(prec_data, bar[i])
  foo <- lapply(foo, data.frame)
  df.name <- seq(from = 10, to = 1000, by = 20)
  names(foo[[i]]) <- df.name[i]
  foo
}

baz <- foo %>%
  bind_rows() %>%
  gather(key, value) %>%
  filter(!is.na(value)) %>%
  group_by(key) %>%
  summarise(n = length(value),
            sem = sd(value) / sqrt(sum(1:length(key)))) %>%
  select(n, sem) %>%
  arrange(n)
# Plot data
prec.plot <- ggplot(data = baz, aes(y = sem, x = n, 
                                    frame = n)) +
  geom_line(aes(cumulative = TRUE), 
            colour = '#0072B2', size = 1) +
  geom_hline(yintercept = 0, 
             colour = "#E69F00", size = 1) +
  labs(title = 'Standard error of the mean as a function of sample size\nfor samples drawn from a normal distribution\n(N = 200,000, mean = 0, sd = 1) n =',
       x = '\nSample size', y = 'Standard error of the mean (SEM)\n') +
  theme(plot.title = element_text(size = 15), 
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = 'gray80', size = 0.3),
        panel.background = element_blank())
prec.plot
gganimate(prec.plot, interval = 0.2, 'sem.gif')


# Create empty objects, counter, and right-skewed data
foo <- rexp(200000)
bar <- list()
cnt <- 1:5000

# Take 5000 samples of n = 200 from 'data' and make a list of sample means.
for(i in cnt){
  bar[[i]] <- mean(sample(foo, size = 200))
}

#######################
# Central limit theorem

# Unlist and round to reduce the number of bins for CLT
baz <- round(unlist(bar), 2)

# cross-tabulate the data
boo <- xtabs(~baz)

# Extract the names ('bins')
nam <- names(boo)

# Make data.frame for plotting
bee <- data_frame(sample = c(1:length(boo)),
                  mean = as.numeric(nam),
                  freq = as.numeric(boo))
######################
# Law of large numbers 

# Unlist for LLN
baz_2 <- unlist(bar)

# Calculate cummulative mean for plotting
zoot <- baz_2 %>%
  data_frame(mean = .) %>%
  mutate(cmean = round(cummean(mean),4),
         count = 1:length(mean)) %>%
  select(count, mean, cmean)
# Create df from original data for plotting
distr <- data_frame(data = foo)

# Plot original distribution
distr.plot <- ggplot(distr, aes(data)) +
  geom_density(fill = '#0072B2', colour = '#0072B2') +
  geom_vline(xintercept = mean(foo), colour = "#E69F00", size = 1) +
  scale_x_continuous(limits = c(0, 14),
                     breaks = c(0, 2, 4, 6, 8, 10, 12, 14)) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  labs(x = '\nValue', y = 'Probability\n') +
  theme(plot.title = element_text(size = 17), 
        axis.text = element_text(size = 17),
        axis.title = element_text(size = 17),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = 'gray80', size = 0.3),
        panel.background = element_blank())

distr.plot
# Save plot
ggsave('original-distr.png', distr.plot)

# Plot CLT data
clt <- ggplot(bee, aes(y = freq, x = mean, frame = sample)) +
  geom_bar(aes(cumulative = TRUE), stat = 'identity',
           fill = '#0072B2', colour = '#0072B2') +
  geom_vline(xintercept = mean(foo), colour = "#E69F00", size = 1) +
  scale_x_continuous(limits = c(0.7, 1.3),
                     breaks = seq(from = 0.7, to = 1.3, by = 0.1)) +
  labs(title = 'bin counter =',
       x = '\nSample mean', y = 'Frequency\n') +
  theme(plot.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = 'gray80', size = 0.3),
        panel.background = element_blank())
gganimate(clt, interval = 0.2, 'central-limit.gif')

# Trim the data to roughly equal the number of frames in the CLT plot
bee_length <- dim(bee)[1]
sample_freq <- length(zoot$count)/bee_length
filter_foo <- seq(from = 1, to = length(cnt), by = sample_freq)
zoot_short <- zoot[filter_foo, ]


# Plot LLN data
lln <- ggplot(zoot_short, aes(y = cmean, x = count)) + 
  geom_line(aes(frame = count, cumulative = TRUE), 
            colour = '#0072B2') +
  geom_hline(yintercept = mean(foo), colour = "#E69F00", size = 1) +
  labs(title = 'Sample counter =',
       x = '\nSample number', y = 'Cumulative mean\n') +
  theme(plot.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = 'gray80', size = 0.3),
        panel.background = element_blank())

gganimate(lln, interval = 0.2, 'law-large-numbers.gif')
