library(ggplot2)
library(dplyr)

# Create test data
set.seed(555)
test_data <- data.frame('group' = rep('A',52),
                        'week_of' = seq.Date(as.Date('2017-01-01'),
                                             as.Date('2017-12-30'),
                                             by = 'week')) %>% 
  rbind(data.frame('group' = rep('B',52),
                   'week_of' = seq.Date(as.Date('2017-01-01'),
                 as.Date('2017-12-30'),
                 by = 'week'))) %>% 
  mutate('num_obs' = rpois(length(week_of),100)) %>% 
  mutate('num_pos' = rbinom(length(week_of),num_obs,0.5)) 

# Add out of control points
test_data[c(10,35,50),'num_pos'] <- test_data[c(10,35,50),'num_pos'] + 15


## Create ggproto Stat for the pChart
Pchart <- ggproto(
  'Pchart',
  Stat,
  compute_group = function(data,scales, n = NULL, which.line = 'prop') {
      if (is.null(n)) {
        warning('p Chart Error - requires area of opportunity "n"')
        return(NULL)
      }
      prop = data$y/n
      mean = rep(mean(prop),times = length(data$x))
      sigma = sqrt(prop*(1-prop)/n)
      ucl = mean + 3*sigma
      lcl = mean - 3*sigma
      
      returnData <- data.frame(x = data$x, sigma, ucl, lcl)
      
      returnData$y <- 
        switch(which.line,
        'prop' = prop,
        'mean' = mean,
        'ucl' = ucl,
        'lcl' = lcl)
      View(returnData)
      returnData
  },
  required_aes = c('x','y')
)

## Create layer function for pChart stat
stat_pChart <- function(mapping = NULL, data = NULL, inherit.aes = TRUE, geom = 'line',
                        position = 'identity', show.legend = FALSE,
                        which.line = which.line,...) {
  propLine <- layer(stat = Pchart, data = data, mapping = mapping, geom = geom,
                    position = position, inherit.aes = inherit.aes,
                    show.legend = show.legend, params = list(which.line = 'prop',...))
  meanLine <- layer(stat = Pchart, data = data, mapping = mapping, geom = geom,
                    position = position, inherit.aes = inherit.aes,
                    show.legend = show.legend, params = list(which.line = 'mean',...))
  uclLine <- layer(stat = Pchart, data = data, mapping = mapping, geom = 'step',
                    position = position, inherit.aes = inherit.aes,
                    show.legend = show.legend, params = list(which.line = 'ucl',...))
  lclLine <- layer(stat = Pchart, data = data, mapping = mapping, geom = 'step',
                    position = position, inherit.aes = inherit.aes,
                    show.legend = show.legend, params = list(which.line = 'lcl',...))
  points <- layer(stat = Pchart, data = data, 
                  mapping = aes(color = (..y.. > ..ucl..) | (..y.. < ..lcl..)), 
                  geom = 'point',
                  position = position, inherit.aes = inherit.aes,
                  show.legend = show.legend, params = list(which.line = 'prop',
                                                                      ...))
  list(propLine,meanLine,uclLine,lclLine,points)
}

# test new function
ggplot(test_data, aes(x = week_of, y = num_pos)) + 
  stat_pChart(n = test_data$num_obs) + 
  scale_color_manual(values =c('black','red'))

