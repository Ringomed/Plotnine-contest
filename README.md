# Plotnine Contest 2024

<p align="center">
<img src="https://github.com/user-attachments/assets/a1a4a976-225d-4852-9315-797ae3d44fea">
</p>

Inspired by ggplot2, the plotnine library is also based on the concept of grammar of graphics, allowing for creation of graphs by stacking multiple layers on top of one another. This powerful concept lets us create essentially any visualization, as long as we know how to code it. I’ll be using it to construct an advanced version of a spider chart from scratch. The chart will present a comparison of Titanic passenger across the three passenger classes.

I'll be using R for data preparation and then plotnine for the plotting (the actual code is run within a Quarto notebook which is available in the repository).

<p align="center">
  <img src="https://github.com/user-attachments/assets/3cdbfa30-3b83-4d2e-8bd4-5538ffa1737f">
  <em>The philosophy behind chart construction using the grammar of graphics approach.</em>
</p>

## Data preparation and tidying

```{r}
library(titanic)
library(dplyr)
library(tidyr)
library(tibble)
library(purrr)
library(scales)
library(ggplot2)


# Prepare the Titanic data
Titanic <- titanic_train

Titanic_gr1 <-
  Titanic %>%
  select(Survived:Fare) %>%
  group_by(Pclass) %>%
  summarise(across(c(Age, Fare), mean, na.rm = TRUE))

Titanic_gr2 <-
  Titanic %>%
  select(Survived:Fare) %>%
  group_by(Pclass, Survived) %>%
  summarise(N = n()) %>%
  pivot_wider(names_from = Survived, values_from = N) %>%
  mutate("Survived (%)" = `1`/(`0` + `1`)) %>%
  select(1,4)

Titanic_gr3 <-
  Titanic %>%
  select(Survived:Fare) %>%
  group_by(Pclass, Sex) %>%
  summarise(N = n()) %>%
  pivot_wider(names_from = Sex, values_from = N)

Titanic_gr <- reduce(list(Titanic_gr1, 
                          Titanic_gr2, 
                          Titanic_gr3), left_join) %>%
  rename(Male = "male", Female = "female")

p_data <- Titanic_gr %>% 
  rename(group = "Pclass") %>%
  mutate(group = as.factor(case_when(group == 1 ~ "1st Class",
                                     group == 2 ~ "2nd Class",
                                     group == 3 ~ "3rd Class")),
         `Survived (%)` = 100*`Survived (%)`)

# Calculate the coordinates of polygon tips
circle_coords <- function(r, n_axis = ncol(p_data) - 1){
  fi <- seq(0, 2*pi, (1/n_axis)*2*pi) + pi/2
  x <- r*cos(fi)
  y <- r*sin(fi)
  
  tibble(x, y, r)
}

central_distance <- 0.2
axis_name_offset <- 0.2

circle_df <- map_df(seq(0, 1, 0.25) + central_distance, circle_coords)

# Calculate the coordinates for the axis lines
axis_coords <- function(n_axis){
  fi <- seq(0, (1 - 1/n_axis)*2*pi, (1/n_axis)*2*pi) + pi/2
  x1 <- central_distance*cos(fi)
  y1 <- central_distance*sin(fi)
  x2 <- (1 + central_distance)*cos(fi)
  y2 <- (1 + central_distance)*sin(fi)
  
  tibble(x = c(x1, x2), y = c(y1, y2), id = rep(1:n_axis, 2))
}

# Coordinates for the axis titles
text_data <- p_data %>%
  select(-group) %>%
  map_df(~ min(.) + (max(.) - min(.)) * seq(0, 1, 0.25)) %>%
  mutate(r = seq(0, 1, 0.25)) %>%
  pivot_longer(-r, names_to = "parameter", values_to = "value")

text_coords <- function(r, n_axis = ncol(p_data) - 1){
  fi <- seq(0, (1 - 1/n_axis)*2*pi, (1/n_axis)*2*pi) + pi/2 + 0.01*2*pi/r
  x <- r*cos(fi)
  y <- r*sin(fi)
  
  tibble(x, y, r = r - central_distance)
}

# Coordinates for the axis labels
labels_data <- map_df(seq(0, 1, 0.25) + central_distance, text_coords) %>%
  bind_cols(text_data %>% select(-r)) %>%
  group_by(parameter) %>%
  mutate(value = signif(value, 2) %>% as.character)


rescaled_coords <- function(r, n_axis){
  fi <- seq(0, 2*pi, (1/n_axis)*2*pi) + pi/2
  tibble(r, fi) %>% mutate(x = r*cos(fi), y = r*sin(fi)) %>% select(-fi)
}

# Coordinates for the car feature value points
rescaled_data <- p_data %>% 
  mutate(across(-group, rescale)) %>%
  mutate(copy = pull(., 2)) %>%
  pivot_longer(-group, names_to = "parameter", values_to = "value") %>%
  group_by(group) %>%
  mutate(coords = rescaled_coords(value + central_distance, ncol(p_data) - 1)) %>%
  unnest
```

## Making the chart

Now for the Python and plotnine part! Let's first set up the libraries and fonts.

```{python}
import pandas as pd
import numpy as np
from plotnine import *
from scipy.stats import zscore
import matplotlib
import matplotlib.pyplot as plt
from matplotlib import font_manager
import requests
import os
import zipfile

#I'll be using the Google's Roboto Condensed font for the plot
# Path to the directory containing the fonts
font_dir = "roboto_condensed_fonts"

# Register the font files
for font_file in os.listdir(font_dir):
    if font_file.endswith(".ttf"):
        font_path = os.path.join(font_dir, font_file)
        font_manager.fontManager.addfont(font_path)
        
# List of font family names in your directory
font_family = 'Roboto Condensed'

# Use the font in the plot
plt.rcParams['font.family'] = font_family
         

central_distance = 0.2
axis_name_offset = 0.2
```

We are finally ready to start making the plot. The layered approach calls for separate construction of different aspects of the graph. First we will create the chart outline.

```{python}
step_1 = (ggplot(r.circle_df, aes('x', 'y')) +
        geom_polygon(data=r.circle_coords(1 + central_distance, p_data.shape[1] - 1), alpha=1, fill='beige') +
        geom_path(aes(group='r'), linetype='dashed', alpha=0.5) +
        theme_void() +
        theme(legend_title=element_blank(),
        legend_direction='horizontal',
        legend_position='bottom',
        legend_box_spacing=0)
        )
```
![image](https://github.com/user-attachments/assets/b04d54c7-44ad-4e6f-9dfc-69bfa3e9e4b0)

Next, we add the axes to the chart...

```{python}
step_2 = (step_1 +
geom_line(data=r.axis_coords(p_data.shape[1] - 1), mapping=aes(x='x', y='y', group='id'), alpha=0.3)
)
```

![image](https://github.com/user-attachments/assets/d99c2ca5-383b-48b7-9c78-f0b6b4721318)

...and overlay the data points.

```{python}
step_3 = (step_2 +
geom_point(data=r.rescaled_data, mapping=aes(x='x', y='y', group='group', color='group'), size=3) +
    geom_path(data=r.rescaled_data, mapping=aes(x='x', y='y', group='group', color='group'), size=1) +
    geom_polygon(data=r.rescaled_data, mapping=aes('x', 'y', group = 'group', color = 'group', fill = 'group'), size = 1, alpha = 0.05, show_legend = False)
    )
```

![image](https://github.com/user-attachments/assets/5ffd4d88-aae6-43dd-86be-c0344f671a0b)

The only thing left is to add the textual labels and names of the axes.

```{python}
step_4 = (step_3 +
geom_text(data=r.labels_data, mapping=aes(x='x', y='y', label='value'), alpha=0.65, size=8, 
         color='#303030') +
        geom_text(data=r.text_coords(1 + central_distance + 0.25, p_data.shape[1] - 1),
         mapping=aes(x='x', y='y'), 
         label=[param for param in p_data.columns[1:]],
         size=9)
    )
```

![image](https://github.com/user-attachments/assets/a5ffbfc8-3a9d-460b-a696-82dae007eb07)


Putting all the steps together, using the Roboto font and pimping the plot a little bit more, we get:

```{python}
plot = (ggplot(r.circle_df, aes('x', 'y')) +
        geom_polygon(data=r.circle_coords(1 + central_distance, p_data.shape[1] - 1), alpha=1, fill='beige') +
        geom_path(aes(group='r'), linetype='dashed', alpha=0.5) +
        theme_void() +
        theme(legend_title=element_blank(),
        legend_direction='horizontal',
        legend_position='bottom',
        legend_box_spacing=0) +
        geom_line(data=r.axis_coords(p_data.shape[1] - 1), mapping=aes(x='x', y='y', group='id'), alpha=0.3) +
        geom_point(data=r.rescaled_data, mapping=aes(x='x', y='y', group='group', color='group'), size=3) +
        geom_path(data=r.rescaled_data, mapping=aes(x='x', y='y', group='group', color='group'), size=1) +
        geom_polygon(data=r.rescaled_data, mapping=aes('x', 'y', group = 'group', color = 'group', fill = 'group'), size = 1, alpha = 0.05, show_legend = False) +
        geom_text(data=r.labels_data, mapping=aes(x='x', y='y', label='value'), alpha=0.65, size=9, 
         color='#303030') +
        geom_text(data=r.text_coords(1 + central_distance + 0.15, p_data.shape[1] - 1),
         mapping=aes(x='x', y='y'), 
         label=[param for param in p_data.columns[1:]],
         size=9) +
         labs(color='', title = 'Comparison of Titanic passengers') +
         theme(legend_position='bottom',
               legend_text=element_text(size=10, face='bold'),
               legend_title=element_blank(),
                text=element_text(family="Roboto Condensed"),
               legend_box_margin=0,
               legend_margin=-30,
               plot_title=element_text(size=12, margin={'b': -40}, face='bold'),
               axis_title=element_blank(),
               axis_text=element_blank()) +
         lims(x=(-1.75, 1.75), y=(-1.5, 1.8)))
```        

<p align="center">
<img src="https://github.com/user-attachments/assets/a1a4a976-225d-4852-9315-797ae3d44fea">
</p>

Ta-daa, our work here is done. Let’s just take a moment more to comment on the numbers displayed. The 1st class passengers were the oldest and the wealthiest of the three. The 3rd class passengers had the highest number of both male and female passengers and were the youngest group — probably mostly young people and families in search for better life abroad. However, the 1st class passengers had the highest survival rate, and the 3rd the lowest. This is probably partly due to the 1st class quarters being closer to the boat deck and partly due to the higher proportion of women in that class (since woman and children were rescued first).

P.S. The code necessary to produce the above plots is also available in the spider_titanic.qmd Quarto notebook in the repository.

## Bonus: mtcars plot

<p align="center">
<img src="https://github.com/user-attachments/assets/ea56447a-ba73-4fbd-aeb5-52a45f91fdc1">
</p>

For the mtcars version, please consult the spider_mtcars.qmd notebook in the repository.

