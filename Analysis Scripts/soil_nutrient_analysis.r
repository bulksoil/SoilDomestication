library(datapasta)
nutes <- tibble::tribble(
                       ~Site, ~NO3, ~P, ~K1, ~K2, ~Na1, ~Na2, ~Ca, ~Mg, ~CEC, ~OM,  ~pH,
               "Arbuckle_ag",   5.82,     11.5,  199, 0.51,   195,  0.85, 19.49,  15.7,            36.5,     3.95, 6.43,
              "Arbuckle_ag2",   5.67,     11.7,  197,  0.5,   196,  0.85, 19.81,  15.9,            37.1,     3.91, 6.44,
           "Arbuckle_non_ag",  38.17,     37.6,  225, 0.58,    45,   0.2, 19.67,  14.6,              35,     2.88,  6.3,
                  "Biggs_ag",   0.96,     13.9,   94, 0.24,    66,  0.29, 15.02,  10.6,            26.1,     2.88,  5.1,
              "Biggs_non_ag",    0.8,      5.5,  114, 0.29,    33,  0.14, 18.95,  11.5,            30.9,     1.75,  6.2,
                     "Davis",   4.57,     12.1,  363, 0.93,    39,  0.17,   8.2,  19.7,              29,     2.52, 6.59,
                "Sacramento",   5.37,       30,  345, 0.88,    43,  0.19, 11.57,  18.4,              31,     2.91, 7.13,
               "Sacramento2",   5.42,     29.8,  353,  0.9,    44,  0.19, 11.98,  18.7,            31.7,     2.63, 7.13,
                 "Arkansas1",   9.25,     23.4,   46, 0.12,    35,  0.15,  6.38,  2.58,            9.22,     1.91,  5.7,
               "Arkansas1_2",   10.1,     23.8,   46, 0.12,    34,  0.15,  6.26,  2.57,             9.1,      1.9, 5.71,
                 "Arkansas2",  12.05,     16.8,   40,  0.1,    37,  0.16,  6.53,  2.71,            9.51,     2.14, 5.68,
               "California1",    8.9,      6.6,  186, 0.48,   126,  0.55, 19.68, 15.64,           36.34,     4.98, 6.32,
               "California2",   8.45,      6.3,  203, 0.52,   123,  0.54, 26.18, 15.97,            43.2,     4.84, 6.43,
                     "Biggs",   53.5,       52,  227, 0.58,    77,  0.34, 12.82,  9.56,            23.3,     3.69, 4.64,
                    "Davis1",  10.55,     20.4,  346, 0.89,    39,  0.17,  8.36, 18.27,           27.69,     2.79, 6.73,
                    "Davis2",   10.4,     21.4,  354,  0.9,    38,  0.17,  8.53, 18.56,           28.16,     2.72, 6.73
           )
nutes <- as.data.frame(nutes, row.names = nutes[,1])
j <- prcomp(nutes[-c(2,8, 9:16),-c(1, 4, 6)], scale. = T)
biplot(j)

points <- j$x %>% 
  as.data.frame() %>% 
  rownames_to_column("Site") %>% 
  ggplot(aes(PC1, PC2, color = Site)) +
  geom_point(size = 3, color = 'black') +
  geom_point(size = 2) +
  scale_color_manual(values = soil.pal) +
  theme_minimal()
arrows <- j$rotation %>% 
  as.data.frame() %>% 
  rownames_to_column("Nutrient") %>% 
  mutate(Nutrient = gsub("2", "", Nutrient)) %>% 
  ggplot(aes(x = 0, y = 0, xend = PC1, yend = PC2, label = Nutrient)) +
  geom_segment(arrow = arrow(length = unit(0.2, "cm"))) +
  geom_text(aes(PC1, PC2), hjust = 1, size = 4) +
  theme_minimal() +
  lims(x = c(-0.7, 0.25)) +
  labs(x = "PC1", y = "PC2")
cowplot::plot_grid(points + theme(legend.position = "none"), arrows)
               