library(tidyverse)
library(htmlwidgets)
library(crosstalk)
library(plotly)
library(highcharter)
library(dygraphs)
library(d3heatmap)
library(leaflet)
library(r2d3)
library(palmerpenguins)
library(networkD3)

                                        # r2d3 examples ---------

r2d3(data =  read_csv('data/flare.csv'), d3_version = 4, script = 'bubbles.js')
r2d3(data = jsonlite::read_json('data/miserables.json'), d3_version = 4, script = 'forcegraph.js')


lemis <- jsonlite::fromJSON('data/miserables.json')
lemis$links <- lemis$links %>%
    mutate(ID = 1:n()) %>% 
    gather(location, nm, source, target) %>%
    mutate(nm = as.integer(as.factor(nm))-1) %>%
    spread(location, nm)

forceNetwork(Links = lemis$links, Nodes = lemis$nodes,
             Source='source', Target='target',
             Value = 'value',
             NodeID = 'id', Group = 'group', opacity=0.7,
             zoom = TRUE, fontSize = 20)

                                        # plotly examples -------
p <- ggplot(data = diamonds, aes(x = cut, fill = clarity))+
    geom_bar(position = 'dodge')
ggplotly(p)

set.seed(20498)
d <- slice_sample(diamonds, n = 500)
plot_ly(d, x = d$carat, y = d$price,
       text = paste("Clarity: ", d$clarity),
        mode = "markers", color = d$carat, size = d$carat)


                       aes(x = bill_length_mm,
                           y = body_mass_g,
                           color = species))+
    geom_point(size = 2) +
    theme_minimal()
ggplotly(plt_penguins, tooltip = c('species'))

                                        # linked scatterplot brushing

d <- highlight_key(penguins)
plt1 <- ggplot(d, aes(x = bill_length_mm, y = body_mass_g))+geom_point()
plt2 <- ggplot(d, aes(x = bill_length_mm, y = flipper_length_mm))+geom_point()
subplot(plt1, plt2) %>% 
    layout(title = "Click and drag to select points") %>%
    highlight("plotly_selected")

 highlight_key(iris) %>%
       GGally::ggpairs(aes(colour = Species), columns = 1:4) %>%
       ggplotly(tooltip = c("x", "y", "colour")) %>%
       highlight("plotly_selected")

                                        # dygraph examples ------
dygraph(nhtemp, main = "New Haven temperatures") %>%
    dyRangeSelector(dateWindow = c("1920-01-01","1960-01-01"))

dygraph(ldeaths, main = "All", group = "lung-deaths")
dygraph(mdeaths, main = "Male", group = "lung-deaths")
dygraph(fdeaths, main = "Female", group = "lung-deaths")

dygraph(presidents, main = "Quarterly Presidential Approval Ratings") %>%
  dyAxis("y", valueRange = c(0, 100)) %>%
  dyEvent("1950-6-30", "Korea", labelLoc = "bottom") %>%
  dyEvent("1965-2-09", "Vietnam", labelLoc = "bottom")

                                        # highcharter example -----
hchart(penguins, 'scatter', hcaes(x = bill_length_mm, y = flipper_length_mm, z = body_mass_g/100, color = species)) %>%
    hc_title(title = "Scatter plot with size and color")

                                        # d3heatmap ------
library(Biobase)
exdat <- readRDS('data/exprset.rds')
library(limma)
design1 <- model.matrix(~type, data=pData(exdat))
lm1 <- lmFit(exprs(exdat), design1)
lm1 <- eBayes(lm1) # compute linear model for each probeset
geneID <- rownames(topTable(lm1, coef = 2, number = 100, 
                            adjust.method = 'none',
                            p.value = 0.05))
exdat2 <- exdat[geneID,] # Keep features with p-values < 0.05

d3heatmap(exprs(exdat2), colors = 'RdYlGn', scale='row')

                                        # ggraph ------

library(ggraph)
library(tidygraph)
library(igraph)
lemis <- jsonlite::fromJSON('data/miserables.json')
lemis_graph <- graph_from_data_frame(lemis$links, vertices = lemis$nodes)
lemis_graph_layout <- create_layout(lemis_graph, 'igraph', algorithm='fr')

ggraph(lemis_graph_layout)+
    geom_edge_link()+
    geom_node_point(size=3)+
    geom_node_text(aes(label=name))+
    theme_void()

ggraph(lemis_graph, 'igraph',algorithm='fr')+
    geom_edge_link()+
    geom_node_point(size=3)+
    geom_node_text(aes(label=name))+
    theme_void()

    

library(ggnetwork)
library(network)


ggplot(ggnetwork(lemis_graph), 
       aes(x =x, y=y, xend=xend, yend=yend))+
    geom_edges()+
    geom_nodes()+
    geom_nodetext_repel(aes(label=name))+
    theme_void()
