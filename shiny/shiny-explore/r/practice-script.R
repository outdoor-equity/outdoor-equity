# practice code
librarian::shelf(palmerpenguins, tidyverse, DT, htmltools)

# filter for body masses
body_mass_df <- penguins %>% 
  filter(body_mass_g %in% 3000:4000)

# scatterplot
ggplot(na.omit(body_mass_df), aes(x = flipper_length_mm, y = bill_length_mm,
                              color = species, shape = species)) +
  geom_point(size = 4) +
  scale_color_manual(values = c("#FEA346", "#B251F1", "#4BA4A4")) +
  labs(x = "Flipper length (mm)", 
       y = "Bill length (mm)",
       color = "Penguin species",
       shape = "Penguin species")

# data table
colnames = c("Species", "Island", "Bill Length (mm)", "Bill Depth (mm)", "Flipper Length (mm)", "Body Mass (g)", "Sex", "Year")
DT::datatable(data = penguins,
              caption = htmltools::tags$caption(
                style = "caption-side: top; text-align: left",
                htmltools::em("Table 1: Palmer Penguins characteristics")),
              options = list(pageLength = 5, lengthMenu = c(5, 10, 20)),
              class = "cell-border stripe",
              colnames = colnames)

# reactive histogram

# flipper length histogram ----
  ggplot(na.omit(penguins), aes(x = flipper_length_mm, fill = species)) +
    geom_histogram(alpha = 0.6) +
    scale_fill_manual(values = c("Adelie" = "#FEA346", 
                                 "Chinstrap" = "#B251F1", 
                                 "Gentoo" = "#4BA4A4")) +
    labs(x = "Flipper length (mm)", y = "Frequency", 
         fill = "Penguin species") +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.background = element_rect(color = "white"))
