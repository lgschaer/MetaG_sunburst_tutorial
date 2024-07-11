#load packages
library(tidyverse)
#install.packages("sunburstR")
library(sunburstR)

#load data
table <- read_csv("/Users/lgschaer/Desktop/Permafrost_180/06282024_Results/summary_table.csv") %>%
  filter(dRep == "winner") %>%
  mutate_at(vars(Domain, Phylum, Class, Order, Family, Genus, Species), funs(gsub("-", "_", .))) %>%
  mutate(
    Species = ifelse(is.na(Species), "Unclassified", Species),
    Genus = ifelse(is.na(Genus), "Unclassified", Genus),
    Path = paste(Domain, Phylum, Class, Order, Family, Genus, Species, sep = "-"),
    V2 = 1
  ) %>%
  select(Domain, Phylum, Class, Order, Family, Genus, Species, Path, V2) %>%
  arrange(Path)
head(table)

greys <- colorRampPalette(c("grey50", "grey22"))(2)
my_colors <- colorRampPalette(c("cyan2", "cadetblue4", "olivedrab4", "yellow3", "orange3", "firebrick", "maroon4", "purple4", "dodgerblue4"))(24) 
my_colors


colors <- c(#domain
            "white",   "#7F7F7F", "#383838", 
            #phylum
            "#00EEEE", "#1CC9CB", "#39A5A9", "#538686", "#5B8761", "#63893D", "#71901F", "#94A713", "#B7BE07",
            "#CDC300", "#CDAA00", "#CD9100", "#C87305", "#BE5111", "#B52E1D", "#A9202F", "#9B1E46", "#8E1C5C",
            "#7C1B6C", "#6A1A7A", "#571A89", "#40298B", "#273B8B", "#104E8B",
            #class
            "#00EEEE", "#1CC9CB", "#39A5A9", "#39A5A9", "#39A5A9", "#39A5A9", "#538686", "#538686", "#538686",
            "#538686", "#5B8761", "#63893D", "#71901F", "#94A713", "#94A713", "#B7BE07", "#CDC300", "#CDAA00",
            "#CDAA00", "#CDAA00", "#CD9100", "#C87305", "#BE5111", "#B52E1D", "#B52E1D", "#A9202F", "#8E1C5C",
            "#7C1B6C", "#7C1B6C", "#7C1B6C", "#6A1A7A", "#6A1A7A", "#6A1A7A", "#571A89", "#40298B", "#40298B",
            "#273B8B", "#104E8B", "#104E8B",
            #order
            "#00EEEE", "#1CC9CB", "#39A5A9", "#39A5A9", "#39A5A9", "#39A5A9", "#39A5A9", "#39A5A9", "#538686",
            "#538686", "#538686", "#538686", "#538686", "#538686", "#538686", "#538686", "#538686", "#538686",
            "#538686", "#538686", "#538686", "#538686", "#5B8761", "#5B8761", "#5B8761", "#63893D", "#63893D",
            "#63893D", "#71901F", "#94A713", "#B7BE07", "#CDC300", "#CDAA00", "#CDAA00", "#CDAA00", "#CD9100",
            "#C87305", "#B52E1D", "#B52E1D", "#A9202F", "#9B1E46", "#8E1C5C", "#7C1B6C", "#7C1B6C", "#7C1B6C",
            "#6A1A7A", "#6A1A7A", "#6A1A7A", "#571A89", "#571A89", "#40298B", "#40298B", "#40298B", "#40298B",
            "#40298B", "#40298B", "#40298B", "#40298B", "#273B8B", "#273B8B", "#273B8B", "#104E8B", "#104E8B",
            "#104E8B", 
            #family
            "#00EEEE", "#1CC9CB", "#39A5A9", "#39A5A9", "#39A5A9", "#39A5A9" ,"#39A5A9", "#538686", "#538686", 
            "#538686", "#538686", "#538686", "#538686", "#538686", "#538686", "#538686", "#538686", "#538686",
            "#538686", "#538686", "#538686", "#538686", "#538686", "#538686", "#538686" ,"#5B8761" ,"#5B8761",
            "#5B8761", "#63893D", "#71901F" ,"#94A713" ,"#94A713" ,"#94A713", "#94A713", "#94A713", "#94A713",
            "#B7BE07", "#CDC300", "#CDAA00", "#CDAA00", "#CDAA00" ,"#CD9100" ,"#C87305", "#B52E1D", "#B52E1D",
            "#B52E1D", "#8E1C5C", "#7C1B6C", "#7C1B6C", "#7C1B6C", "#6A1A7A", "#6A1A7A" ,"#6A1A7A", "#571A89",
            "#40298B", "#40298B", "#40298B", "#40298B", "#40298B", "#40298B", "#40298B", "#40298B", "#40298B",
            "#40298B", "#40298B", "#40298B", "#40298B", "#273B8B", "#273B8B", "#104E8B", "#104E8B", "#104E8B",
            #genus
            "white",   "#1CC9CB", "#39A5A9", "#39A5A9", "#39A5A9", "#39A5A9", "#39A5A9", "#39A5A9", "#39A5A9",
            "#39A5A9", "#39A5A9", "#39A5A9", "#538686", "#538686", "#538686", "#538686", "#538686", "#538686",
            "#538686", "#538686", "#538686", "#538686", "#538686", "#538686", "#538686", "#538686", "#538686",
            "#538686", "#538686", "#538686" ,"#538686" ,"#538686", "#538686", "#538686", "#538686", "#538686",
            "#538686", "#538686", "#538686", "#538686", "#538686", "#5B8761", "#5B8761", "#5B8761", "#5B8761",
            "#5B8761", "#63893D", "#63893D", "#63893D", "#63893D", "#94A713", "#94A713", "#94A713", "#94A713",
            "#94A713", "#94A713", "#94A713", "#94A713", "#94A713", "#B7BE07", "#CDC300", "#CDAA00", "#CDAA00",
            "#CDAA00" ,"#CDAA00", "#CDAA00", "#C87305", "#C87305", "#A9202F", "#A9202F", "#7C1B6C", "#7C1B6C",
            "#7C1B6C", "#7C1B6C", "#6A1A7A", "#6A1A7A", "#571A89", "#40298B", "#40298B", "#40298B", "#40298B",
            "#40298B", "#40298B", "#40298B", "#40298B", "#40298B", "#40298B", "#40298B", "#40298B", "#40298B", 
            "#40298B", "#40298B", "#40298B", "#40298B", "#40298B", "#40298B", "#40298B", "#273B8B", "#273B8B",
            "#104E8B", "#104E8B", "#104E8B", "#104E8B", "#104E8B", 
            #species
            "#39A5A9", "#39A5A9", "#538686", "#538686", "#63893D", "#94A713", "#94A713" ,"#94A713", "#94A713",
            "#94A713", "#94A713", "#CDAA00", "#CD9100", "#A9202F", "#7C1B6C", "#273B8B", "#104E8B")

#colors <- c("white", "grey2", "grey80", phylum$my_colors, phylum$my_colors, phylum$my_colors, phylum$my_colors, phylum$my_colors)
sunburst(data = data.frame(xtabs(V2~Path, table)), colors = colors, legend = FALSE)


########
# Using ggplot

table_raw <- read_csv("/Users/lgschaer/Desktop/Permafrost_180/06282024_Results/summary_table.csv") 

table <- table_raw %>%
  filter(dRep == "winner") %>%
  mutate_at(vars(Domain, Phylum, Class, Order, Family, Genus, Species), funs(gsub("-", "_", .))) %>%
  mutate_at(vars(Domain, Phylum, Class, Order, Family, Genus, Species), funs(gsub(" ", "_", .))) %>%
  mutate(
    Species = ifelse(is.na(Species), "Unclassified", Species),
    Genus = ifelse(is.na(Genus), "Unclassified", Genus),
    Path = paste(Domain, Phylum, Class, Order, Family, Genus, Species, sep = "-"),
    V2 = 1
  ) %>%
  arrange(Path) %>%
  mutate(row = row_number())
head(table)
dim(table)


# Make lists of all taxonomic levels within each phylum
Thermoplasmatota <- filter(table, Phylum == "Thermoplasmatota")
Thermoplasmatota <- c(unique(Thermoplasmatota$Class), unique(Thermoplasmatota$Order), unique(Thermoplasmatota$Family), unique(Thermoplasmatota$Genus), unique(Thermoplasmatota$Species))
Thermoproteota <- filter(table, Phylum == "Thermoproteota")
Thermoproteota <- c(unique(Thermoproteota$Class), unique(Thermoproteota$Order), unique(Thermoproteota$Family), unique(Thermoproteota$Genus), unique(Thermoproteota$Species))
Acidobacteriota <- filter(table, Phylum == "Acidobacteriota")
Acidobacteriota <- c(unique(Acidobacteriota$Class), unique(Acidobacteriota$Order), unique(Acidobacteriota$Family), unique(Acidobacteriota$Genus), unique(Acidobacteriota$Species))
Actinomycetota <- filter(table, Phylum == "Actinomycetota")
Actinomycetota <- c(unique(Actinomycetota$Class), unique(Actinomycetota$Order), unique(Actinomycetota$Family), unique(Actinomycetota$Genus), unique(Actinomycetota$Species))
Bacillota_A <- filter(table, Phylum == "Bacillota_A")
Bacillota_A <- c(unique(Bacillota_A$Class), unique(Bacillota_A$Order), unique(Bacillota_A$Family), unique(Bacillota_A$Genus), unique(Bacillota_A$Species))
Bacillota_C <- filter(table, Phylum == "Bacillota_C")
Bacillota_C <- c(unique(Bacillota_C$Class), unique(Bacillota_C$Order), unique(Bacillota_C$Family), unique(Bacillota_C$Genus), unique(Bacillota_C$Species))
Bacillota_I <- filter(table, Phylum == "Bacillota_I")
Bacillota_I <- c(unique(Bacillota_I$Class), unique(Bacillota_I$Order), unique(Bacillota_I$Family), unique(Bacillota_I$Genus), unique(Bacillota_I$Species))
Bacteroidota <- filter(table, Phylum == "Bacteroidota")
Bacteroidota <- c(unique(Bacteroidota$Class), unique(Bacteroidota$Order), unique(Bacteroidota$Family), unique(Bacteroidota$Genus), unique(Bacteroidota$Species))
Caldisericota <- filter(table, Phylum == "Caldisericota")
Caldisericota <- c(unique(Caldisericota$Class), unique(Caldisericota$Order), unique(Caldisericota$Family), unique(Caldisericota$Genus), unique(Caldisericota$Species))
Campylobacterota <- filter(table, Phylum == "Campylobacterota")
Campylobacterota <- c(unique(Campylobacterota$Class), unique(Campylobacterota$Order), unique(Campylobacterota$Family), unique(Campylobacterota$Genus), unique(Campylobacterota$Species))
Chloroflexota <- filter(table, Phylum == "Chloroflexota")
Chloroflexota <- c(unique(Chloroflexota$Class), unique(Chloroflexota$Order), unique(Chloroflexota$Family), unique(Chloroflexota$Genus), unique(Chloroflexota$Species))
Desulfobacterota <- filter(table, Phylum == "Desulfobacterota")
Desulfobacterota <- c(unique(Desulfobacterota$Class), unique(Desulfobacterota$Order), unique(Desulfobacterota$Family), unique(Desulfobacterota$Genus), unique(Desulfobacterota$Species))
Eremiobacterota <- filter(table, Phylum == "Eremiobacterota")
Eremiobacterota <- c(unique(Eremiobacterota$Class), unique(Eremiobacterota$Order), unique(Eremiobacterota$Family), unique(Eremiobacterota$Genus), unique(Eremiobacterota$Species))
FCPU426 <- filter(table, Phylum == "FCPU426")
FCPU426 <- c(unique(FCPU426$Class), unique(FCPU426$Order), unique(FCPU426$Family), unique(FCPU426$Genus), unique(FCPU426$Species))
Fibrobacterota <- filter(table, Phylum == "Fibrobacterota")
Fibrobacterota <- c(unique(Fibrobacterota$Class), unique(Fibrobacterota$Order), unique(Fibrobacterota$Family), unique(Fibrobacterota$Genus), unique(Fibrobacterota$Species))
Gemmatimonadota <- filter(table, Phylum == "Gemmatimonadota")
Gemmatimonadota <- c(unique(Gemmatimonadota$Class), unique(Gemmatimonadota$Order), unique(Gemmatimonadota$Family), unique(Gemmatimonadota$Genus), unique(Gemmatimonadota$Species))
JAJYCY01 <- filter(table, Phylum == "JAJYCY01")
JAJYCY01 <- c(unique(JAJYCY01$Class), unique(JAJYCY01$Order), unique(JAJYCY01$Family), unique(JAJYCY01$Genus), unique(JAJYCY01$Species))
Methylomirabilota <- filter(table, Phylum == "Methylomirabilota")
Methylomirabilota <- c(unique(Methylomirabilota$Class), unique(Methylomirabilota$Order), unique(Methylomirabilota$Family), unique(Methylomirabilota$Genus), unique(Methylomirabilota$Species))
Myxococcota <- filter(table, Phylum == "Myxococcota")
Myxococcota <- c(unique(Myxococcota$Class), unique(Myxococcota$Order), unique(Myxococcota$Family), unique(Myxococcota$Genus), unique(Myxococcota$Species))
Patescibacteria <- filter(table, Phylum == "Patescibacteria")
Patescibacteria <- c(unique(Patescibacteria$Class), unique(Patescibacteria$Order), unique(Patescibacteria$Family), unique(Patescibacteria$Genus), unique(Patescibacteria$Species))
Planctomycetota <- filter(table, Phylum == "Planctomycetota")
Planctomycetota <- c(unique(Planctomycetota$Class), unique(Planctomycetota$Order), unique(Planctomycetota$Family), unique(Planctomycetota$Genus), unique(Planctomycetota$Species))
Pseudomonadota <- filter(table, Phylum == "Pseudomonadota")
Pseudomonadota <- c(unique(Pseudomonadota$Class), unique(Pseudomonadota$Order), unique(Pseudomonadota$Family), unique(Pseudomonadota$Genus), unique(Pseudomonadota$Species))
Spirochaetota <- filter(table, Phylum == "Spirochaetota")
Spirochaetota <- c(unique(Spirochaetota$Class), unique(Spirochaetota$Order), unique(Spirochaetota$Family), unique(Spirochaetota$Genus), unique(Spirochaetota$Species))
Verrucomicrobiota <- filter(table, Phylum == "Verrucomicrobiota")
Verrucomicrobiota <- c(unique(Verrucomicrobiota$Class), unique(Verrucomicrobiota$Order), unique(Verrucomicrobiota$Family), unique(Verrucomicrobiota$Genus), unique(Verrucomicrobiota$Species))


taxList <- c(unique(table$Domain), unique(table$Phylum), unique(table$Class), unique(table$Order), unique(table$Family), unique(table$Genus), unique(table$Species))
head(taxList)
length(taxList)

dim(table)

numbers <- c(seq(0, length(table$genome_name), by = 20), length(table$genome_name))

colorsHopefully <- table %>%
  select(Domain, Phylum, Class, Order, Family, Genus, Species, V2,assembly_quality) %>%
  arrange(Domain, Phylum, Class, Order, Family, Genus, Species) %>%
  mutate(Path = paste(Family, Genus, Species, sep = "_"), 
         Quality = assembly_quality,
         Mag_Number = n():1) %>%
  pivot_longer(cols = c("Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species", "Quality"), values_to = "Tax_Name", names_to = "Tax_Level") %>%
  mutate(#Domain = value, Phylum = value, Class = value, Order = value, Family = value, Genus = value, Species = value,
    my_colors = case_when(
      Tax_Name == "Bacteria" ~ "#7F7F7F", 
      Tax_Name == "Archaea" ~ "#383838", 
      Tax_Name == "Thermoplasmatota" | Tax_Name %in% Thermoplasmatota ~ "#00EEEE", 
      Tax_Name == "Thermoproteota" | Tax_Name %in% Thermoproteota ~ "paleturquoise",   
      Tax_Name == "Acidobacteriota" | Tax_Name %in% Acidobacteriota ~ "#39A5A9", 
      Tax_Name == "Actinomycetota" | Tax_Name %in% Actinomycetota ~ "darkslategrey", 
      Tax_Name == "Bacillota_A" | Tax_Name %in% Bacillota_A ~ "aquamarine", 
      Tax_Name == "Bacillota_C" | Tax_Name %in% Bacillota_C ~ "#63893D", 
      Tax_Name == "Bacillota_I" | Tax_Name %in% Bacillota_I ~ "darkgreen",    
      Tax_Name == "Bacteroidota" | Tax_Name %in% Bacteroidota ~ "#94A713", 
      Tax_Name == "Caldisericota" | Tax_Name %in% Caldisericota ~ "greenyellow", 
      Tax_Name == "Campylobacterota" | Tax_Name %in% Campylobacterota ~ "#CDC300", 
      Tax_Name == "Chloroflexota" | Tax_Name %in% Chloroflexota ~ "lemonchiffon", 
      Tax_Name == "Desulfobacterota" | Tax_Name %in% Desulfobacterota ~ "goldenrod4", 
      Tax_Name == "Eremiobacterota" | Tax_Name %in% Eremiobacterota ~ "gold", 
      Tax_Name == "FCPU426" | Tax_Name %in% FCPU426 ~ "#BE5111",          
      Tax_Name == "Fibrobacterota" | Tax_Name %in% Fibrobacterota ~ "cornsilk3", 
      Tax_Name == "Gemmatimonadota" | Tax_Name %in% Gemmatimonadota ~ "darkred", 
      Tax_Name == "JAJYCY01" | Tax_Name %in% JAJYCY01 ~ "rosybrown", 
      Tax_Name == "Methylomirabilota" | Tax_Name %in% Methylomirabilota ~ "#8E1C5C",
      Tax_Name == "Myxococcota" | Tax_Name %in% Myxococcota ~ "thistle3", 
      Tax_Name == "Patescibacteria" | Tax_Name %in% Patescibacteria ~ "#6A1A7A", 
      Tax_Name == "Planctomycetota" | Tax_Name %in% Planctomycetota ~ "lavender",  
      Tax_Name == "Pseudomonadota" | Tax_Name %in% Pseudomonadota ~ "#40298B", 
      Tax_Name == "Spirochaetota" | Tax_Name %in% Spirochaetota ~ "lightblue", 
      Tax_Name == "Verrucomicrobiota" | Tax_Name %in% Verrucomicrobiota ~ "#104E8B",
      Tax_Level == "Quality" ~ NA),
    my_colors = ifelse(Tax_Name == "Unclassified", NA, my_colors),
    col_color = ifelse(Tax_Level != "Quality" & Tax_Name != "Unclassified", "black", NA),
    line_colors = ifelse(Mag_Number %in% numbers, "grey70", NA),
    point_fill = case_when(
      Tax_Level == "Quality" & Tax_Name == "Medium" ~ "orange3",
      Tax_Level == "Quality" & Tax_Name == "High" ~ "seagreen4"),
    point_color = case_when(
      Tax_Level == "Quality" ~ "black"),
    Tax_Name = ifelse(Tax_Name == "Unclassified", Path, Tax_Name),
    Tax_Name = ifelse(Tax_Level == "Quality", paste(Tax_Name, Mag_Number, sep = "_"), Tax_Name)) %>%
  mutate(row = row_number(),
    Tax_Level = factor(Tax_Level, levels = c("Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species", "Quality"))) %>%
  group_by(Tax_Level, Tax_Name, my_colors, point_fill, point_color, col_color) %>%
  reframe(
    V2 = sum(V2),
    row = max(row),
    Mag_Number = median(Mag_Number),
    line_colors = median(line_colors)
  )
  
head(colorsHopefully)
#view(colorsHopefully)
#dim(quality)
#dim(colorsHopefully)


ggplot(colorsHopefully, aes(x = Tax_Level, y = V2, fill = my_colors, group = row))+
  geom_hline(aes(yintercept = Mag_Number, color = line_colors))+
  geom_col(aes(color = col_color), size = 0.3, width = 1)+
  geom_point(aes(x = Tax_Level, y = Mag_Number, fill = point_fill, group = row, color = point_color), shape = 21, size = 2, position = position_nudge(x = 0, y = -.5))+
  #geom_text(aes(label = Mag_Number, y = Mag_Number))+
  scale_fill_identity()+
  scale_color_identity()+
  coord_radial(theta = 'y', expand = FALSE, inner.radius = 0.18, r_axis_inside = TRUE, start = 0 * pi, end = 1.65 * pi)+
  labs(x = NULL, y = NULL)+
  theme_minimal()+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"),
        #axis.line.theta = element_blank(),
        axis.text.y = element_text(size = 18, color = "black"),
        axis.text.x = element_blank())

#test <- filter(colorsHopefully, Tax_Level=="Phylum")
#unique(test$Tax_Name)

legend <- colorsHopefully %>%
  select(Tax_Level, Tax_Name, my_colors) %>%
  filter(Tax_Level == "Phylum") %>%
  mutate(Tax_Name = factor(Tax_Name, levels = c("Thermoplasmatota", "Thermoproteota",  "Acidobacteriota",   "Actinomycetota",    "Bacillota_A",       "Bacillota_C",       "Bacillota_I",      
                                 "Bacteroidota",      "Caldisericota",     "Campylobacterota",  "Chloroflexota",     "Desulfobacterota", 
                                 "Eremiobacterota",   "FCPU426",           "Fibrobacterota",    "Gemmatimonadota",   "JAJYCY01",         
                                 "Methylomirabilota", "Myxococcota",       "Patescibacteria",   "Planctomycetota",   "Pseudomonadota",   
                                 "Spirochaetota",  "Verrucomicrobiota"))) %>%
  arrange(Tax_Name) %>%
  mutate(column = rep(1:4, each = 6),
         row = rep(1:6, times = 4))
head(legend)
#View(legend)
#unique(legend$Tax_Name)

ggplot(legend, aes(y = row, x = column, fill = my_colors))+
  geom_point(shape = 22, size = 10)+
  geom_text(aes(label = Tax_Name), size = 9, hjust = 0, position = position_nudge(x = 0.1))+
  theme_void()+
  xlim(1,5)+
  ylim(6.1, 0.9)+
  scale_fill_identity()


legend2 <- data.frame(
  row = 1, 
  column = 1:2,
  Quality = c("High", "Medium"),
  colors = c("seagreen4", "orange3"))
head(legend2)
#View(legend)
#unique(legend$Tax_Name)

ggplot(legend2, aes(x = row, y = column, fill = colors))+
  geom_point(shape = 21, size = 10)+
  geom_text(aes(label = Quality), size = 9, hjust = 0, position = position_nudge(x = 0.1))+
  theme_void()+
  ylim(0,6)+
  xlim(1,2)+
  scale_fill_identity()
