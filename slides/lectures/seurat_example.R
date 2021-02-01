library(tidyverse)
library(Seurat)

pbmc.data <- Read10X(data.dir='~/Downloads/filtered_gene_bc_matrices/hg19/')
pbmc <- CreateSeuratObject(counts = pbmc.data, project = "pbmc3k", min.cells = 3, min.features = 200)
pbmc

pbmc[["percent.mt"]] <- PercentageFeatureSet(object = pbmc, pattern = "^MT-")
plt <- VlnPlot(object = pbmc, features = c('nFeature_RNA','nCount_RNA','percent.mt'))

plot_data <- pbmc@meta.data %>%
  gather(variable, value, -orig.ident)
ggplot(plot_data, aes(orig.ident, value)) +
  geom_violin(fill = 'red') +
  geom_jitter(width=0.5, alpha = 0.1) +
  facet_wrap(~variable, nrow = 1, scales = 'free_y') +
  labs(x = 'Identity',y = 'Expression Level') +
  theme_classic()

plot1 <- FeatureScatter(object = pbmc, feature1 = "nCount_RNA", feature2 = "percent.mt")
plot2 <- FeatureScatter(object = pbmc, feature1 = "nCount_RNA", feature2 = "nFeature_RNA")
CombinePlots(plots = list(plot1, plot2))

cormatrix <- cor(pbmc@meta.data %>% select(-orig.ident))
plt1 <-
  ggplot(pbmc@meta.data, aes(x = nCount_RNA, y = percent.mt, group = orig.ident, color = orig.ident)) +
  geom_point() +
    theme_classic() +
    labs(color = 'Identity',
         title=as.character(round(cormatrix['nCount_RNA','percent.mt'],2)))+
  theme(plot.title = element_text(face = 'bold', hjust = 0.5))

plt2 <-
  ggplot(pbmc@meta.data, aes(x = nCount_RNA, y = nFeature_RNA, group = orig.ident, color = orig.ident)) +
  geom_point() +
  theme_classic() +
  labs(color = 'Identity',
       title=as.character(round(cormatrix['nCount_RNA','nFeature_RNA'],2)))+
  theme(plot.title = element_text(face = 'bold', hjust = 0.5))
ggpubr::ggarrange(plt1, plt2, nrow = 1, ncol=2)

pbmc <- subset(x = pbmc, subset = nFeature_RNA > 200 & nFeature_RNA < 2500 & percent.mt < 5)
pbmc <- NormalizeData(object = pbmc, normalization.method = "LogNormalize", scale.factor = 10000)
# This is stored in pbmc[['RNA']]@meta.features

pbmc <- FindVariableFeatures(object = pbmc, selection.method = "vst", nfeatures = 2000)


# Identify the 10 most highly variable genes
top10 <- head(x = VariableFeatures(object = pbmc), 10)

# plot variable features with and without labels
plot1 <- VariableFeaturePlot(object = pbmc)

plt_data <- pbmc[['RNA']]@meta.features %>% rownames_to_column(var='id')
topvars <- pbmc[['RNA']]@var.features
plt_data <- plt_data %>% mutate(indic = ifelse(id %in% topvars, 'Variable count','Non-variable count'))
bl <- plt_data %>% count(indic) %>% glue::glue_data("{indic}: {n}")
plt_data <- plt_data %>%
  mutate(indic = bl[indic])
plt11 <- ggplot(plt_data, aes(x = mean, y = variance.standardized, color = indic)) +
  geom_point() +
  scale_x_log10() +
  scale_color_manual(values = c('black','red')) +
  labs(x = 'Average Expression', y = 'Standardized Variance', color = '')+
  theme_classic()


plot2 <- LabelPoints(plot = plot1, points = top10, repel = TRUE)
plt12 <- plt11 + ggrepel::geom_text_repel(data = plt_data %>% filter(id %in% top10),
                                          aes(label = id),
                                          color = 'black')
CombinePlots(plots = list(plot1, plot2))
ggpubr::ggarrange(plt11, plt12, nrow=1, ncol=2)

all.genes <- rownames(x = pbmc)
pbmc <- ScaleData(object = pbmc, features = all.genes)
#pbmc <- ScaleData(object = pbmc)

pbmc <- RunPCA(object = pbmc, features = VariableFeatures(object = pbmc))
# results in pbmc@reductions

VizDimLoadings(object = pbmc, dims = 1, reduction = "pca")

plt_data <- pbmc@reductions[['pca']]@feature.loadings %>%
  as_tibble(rownames=NA) %>% rownames_to_column(var='id')

plt_dat1 <- plt_data %>%
  select(id, PC_1) %>% top_n(30, abs(PC_1))
ggplot(plt_dat1, aes(x = PC_1, y = fct_reorder(id, PC_1)))+
  geom_point(color='blue') +
  labs(y = '')+
  theme_classic()

ggplot(plt_data, aes(x = PC_1, y = PC_2))+geom_point()

DimPlot(object = pbmc, reduction = "pca")

plt_data <- Embeddings(pbmc, reduction='pca') %>%
  as.data.frame() %>%
  rownames_to_column(var='id') %>%
  left_join(as.data.frame(pbmc@meta.data) %>%
              rownames_to_column(var='id'),
            by ='id')
ggplot(plt_data, aes(x = PC_1, y = PC_2, color = orig.ident))+geom_point()


pbmc <- FindNeighbors(object = pbmc, dims = 1:10)
pbmc <- FindClusters(object = pbmc, resolution = 0.5)
pbmc <- RunUMAP(object = pbmc, dims = 1:10)

plt_data <- as.data.frame(pbmc@reductions[['umap']]@cell.embeddings) %>%
  rownames_to_column(var='id') %>%
  mutate(groups = pbmc@active.ident[id])

ggplot(plt_data, aes(x = UMAP_1, y = UMAP_2, color = groups))+geom_point()
