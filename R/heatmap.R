#' heatmap plotting with pheatmap package
#'
#' @param df  a dataframe or matrix for heat map, row samples column factors
#' @param limitMax limit of the max value of the matrix, avoid too large value
#' @param limitMin limit of the max value of the matrix, avoid too small value
#' @param scale  if scaleing the matrix, can also use log before heatmap plotting, default F
#' @param show.rowName if show row Name, default T
#' @param show.colName if show column Name, default T
#' @param row.cluster if clust row, default F
#' @param col.cluster if clust column, default F
#' @param cellwidth cell width, default 50
#' @param cellheight cell height, default 20
#' @param pal color pal for heatmap  a vector of three color values, default  c("navy", "white", "firebrick3")
#' @param color_levelnum levels setting for color change, default 50
#' @param border.color border line color, default NA, without border line color;
#' @param col.angle angle of column name ,can be 270、0、45、90、315, default 90
#' @param rowtext.size rowname text size , default 14
#' @param coltext.size colname text size , default 18
#' @param adding.legend if adding the legend figure for heatmap, default T
#' @param legend.breaks a vectors of numeric to divide plot values
#' @param legend.labels a vectors of strings to label legend.breaks, must be consistant with break cut values
#' @param ac_df annotation dataframe for colunms
#' @param ann_colors a color list for annotation dataframes , could be more than one palette
#'                 e.g., ann_colors = list(group1= c("white", "tomato4"),  group2 = c(X1 = "slateblue3", X2 = "red2"), group3 = c(phylum1 = "#7D26CD", phylum2 = "#E7298A", phylum3 = "#66A61E"))
#' @param add.rowgaps gaps split the row, a numeric vector with the beginning row numbers to split
#' @param add.colgaps gaps split the column, a numeric vector with the beginning colnumn numbers to split
#'
#' @return  a grid object of heatmap plot
#' @export
#'
#' @examples myplotting::pheatmap_plotting(df = n, cellheight = 2, show.rowName = F, coltext.size = 10, ac_df = ac, add.rowgaps = c(50, 80), add.colgaps = c(3))

# R 数据可视化 —— 聚类热图 pheatmap https://zhuanlan.zhihu.com/p/370477638
# R语言pheatmap包绘制热图进阶教程 https://mp.weixin.qq.com/s/rPDgHIF0JysbS6oYvrAyRQ
# pheatmap画热图 参数解析 https://mp.weixin.qq.com/s/dk7UeN_j5oA3AbYuEgeg2Q
pheatmap_plotting <- function(df, limitMax,  limitMin, scale=F,
                      show.rowName=T,
                      show.colName=T,
                      row.cluster = F, col.cluster = F,
                      cellwidth = 50, cellheight =20,
                      pal = c("navy", "white", "firebrick3"), color_levelnum=50, border.color = NA,
                      col.angle = 90, rowtext.size =14, coltext.size=18,
                      adding.legend = T, legend.breaks = NA, legend.labels = NA,
                      ac_df = NA, ann_colors = NA,
                      add.rowgaps = c(0), add.colgaps = c(0) ){
    # data format
    if( !(is.data.frame(df) | is.matrix(df)) ){
        stop('df must be dataframe or matrix!!')
    }

    if(missing(ac_df)){
        warning('ac_df(annotation colunm) missing, must be dataframe !!')
    }


    cat(paste0('In this dataframe/matrix, The max vaule is ', round(max(df), 3), 'the min value is ', round(min(df), 3) , '\n' ))


    # preparing heatmap data
    df_matrix = as.matrix(df)
    rownames(df_matrix) = rownames(df)
    colnames(df_matrix) = colnames(df)

    # setting max and min value
    if(!missing(limitMax) & !missing(limitMin) & scale==F){

        df_matrix[df_matrix > limitMax] = limitMax
        df_matrix[df_matrix < limitMin] = limitMin

    }else if(missing(limitMax) & missing(limitMin) & scale==T){

        df_matrix=scale(df_matrix)

    }else if(missing(limitMax) & missing(limitMin) & scale==F){

        df_matrix = df_matrix

    }else{
        cat('can not give max or min limit value and scaling the matrix at the same time! \n')
    }


    # if adding legend labs
    if( sum(is.na(legend.labels)) == 0  & sum(is.na(legend.breaks)) == 0 ){
        cat('no legend_labels and legend_breaks given, using default value! \n')
        }else{
            cat('legend_labels and legend_breaks must be in the same length, and consitent , labels can be characteristic, shorted as high or low, breaks must be numeric !! \n')
            stopifnot(length(legend.breaks)==length(legend.labels))
        }

    # if adding gap on row or columns (if cluster on row or column, counld not add gap)
    if( sum(add.rowgaps) != 0  | sum(add.colgaps) != 0){
        cat('if cluster on row or column, counld not add gaps! \n
            row/column gaps begein on the numbers in the vectors \n')
        stopifnot( row.cluster == F & col.cluster == F)
    }

    # plotting
    p = pheatmap::pheatmap(df_matrix,
                           show_colnames = show.colName,  show_rownames = show.rowName,
                           cluster_rows = row.cluster,  cluster_cols = col.cluster,
                           cellwidth = cellwidth, cellheight = cellheight,
                           color = colorRampPalette(pal)(color_levelnum),
                           border_color = border.color,
                           angle_col = col.angle,
                           fontsize_row = rowtext.size,
                           fontsize_col = coltext.size,
                           legend = adding.legend,
                           legend_breaks = legend.breaks,
                           legend_labels = legend.labels,
                           annotation_col = ac_df,
                           annotation_colors = ann_colors,
                           gaps_row = add.rowgaps,
                           gaps_col = add.colgaps )

    return(p)
}






# R语言画图 | ggplot2绘制热图及个性化修饰 https://mp.weixin.qq.com/s/bwOIZ_n31axAkUtJMrzoAw
# R语言画图 | ggplot2绘制离散值热图 https://mp.weixin.qq.com/s/rHOFlKSIRx7-jnujNFqWBg
# 跟着Nature学作图 | ggplot2包绘制突变基因及其临床特点的复杂组合图 https://mp.weixin.qq.com/s/UaRN8JEquDvOPMuxidY14A
# paletteer包：拥有2100多个调色板！ https://cloud.tencent.com/developer/article/1839444
#' Title: heatmap plotting with ggplot2, matrix could be discrete / continues values
#'
#' @param df a dataframe for heat map, row samples column factors
#' @param row.factor.levels row name orders ranked as row.factor.levels
#' @param col.factor.levels col name orders ranked as col.factor.levels
#' @param fill.factor.levels legend factor ranked as fill.factor.levels if filling for discrete values
#' @param pal.discrete discrete pal color pal for heatmap, default ggsci::pal_lancet(palette = c("lanonc"), alpha = 0.6)(9)
#' @param pal.continue continue pal color pal for heatmap, could be two or three, default c('#498EA4', 'white', '#E54924')
#' @param pal.continue.limit limits for continue pal color pal, default max and min value
#' @param pal.continue.break breaks for continue pal color pal, default a seq vector cutting the bar average
#' @param border.color border line color of the tile,  default grey
#' @param legend.title legend title of the heatmap bar,  default blank ''
#' @param legend.title.size text size of the legend title, default 18
#' @param legend.text.size text size of the legend lab, default 16
#' @param legend.position legend bar position ,default right, 'none' do not show the bar
#' @param show.rowNames  if show row names default T
#' @param row.text.size text size of the row names, default 16
#' @param row.text.angle text angle of the row names, default 0
#' @param show.colNames if show column names default T
#' @param col.text.size text size of the col names, default 16
#' @param col.text.angle  text angle of the column names, default 90
#'
#' @return a ggplot2 object of heatmap (the main plot)
#' @export
#'
#' @examples heatmap_ggplot2(df = df_wider, col.factor.levels = c('low', 'high'), pal.continue = c("#0074b3", "#982b2b"), legend.title = 'gene expression (TPM)',
#'           pal.continue.limit = c(0, 100), pal.continue.break = c( 20, 40, 60, 80))
#'
heatmap_ggplot2 <- function(df, row.factor.levels, col.factor.levels,
                            fill.factor.levels, pal.discrete = ggsci::pal_lancet(palette = c("lanonc"), alpha = 0.6)(9),
                            pal.continue =  c('#498EA4', 'white', '#E54924'), pal.continue.limit = NULL,  pal.continue.break = NULL,
                            border.color = "grey",
                            legend.title='', legend.title.size=18, legend.text.size=16, legend.position = "right",
                            show.rowNames = T, row.text.size=16, row.text.angle=0,
                            show.colNames = T, col.text.size=16, col.text.angle=90){
    # data format
    cat('Dataframe with wide form (such as row genes, column samples)，filling groups could be discrete levels (such as gene sample wild/mutation) or numeric values \n')

    stopifnot(is.data.frame(df))  # df must be a dataframe, with rowname as heatmap rownames

    # plot data
    plot_df = df %>% rownames_to_column('row_var_name') %>%
        tidyr::pivot_longer(-row_var_name)  # generate plot_df with new columns as  'name' for columns  ; 'value' for  spread values of the long dataframe (here value is characteristic such as yes/no for discrete)

    # row levels
    if(!missing(row.factor.levels)){
        plot_df$row_var_name <- factor(plot_df$row_var_name, levels = row.factor.levels )
    }

    # col levles
    if(!missing(col.factor.levels)){
        plot_df$name <- factor(plot_df$name, levels = col.factor.levels )
    }



    # if filling discrete or contiunue values
    if(sum(is.numeric(plot_df$value)) != 0){
        # numeric values
        cat(paste0('fill groups is numeric : max value:', max(plot_df$value), 'min value : ', min(plot_df$value), '\n'))

        if(missing(pal.continue.limit)){
            # limits
            pal.continue.limit = c(floor(min(plot_df$value)) , ceiling(max(plot_df$value)))
        }

        if(missing(pal.continue.break)){
            # break points
            break_seq = seq(floor(min(plot_df$value)), ceiling(max(plot_df$value)),  (ceiling(max(plot_df$value)) - floor(min(plot_df$value)))/3)
            pal.continue.break = round(break_seq, 0)
        }

        # plotting
        p = ggplot(data = plot_df, aes(x = name,y = row_var_name))+
            geom_tile(aes(fill = value), color = border.color)

        if(length(pal.continue) == 3){
            p = p +  scale_fill_gradient2(low = pal.continue[1], mid = pal.continue[2], high = pal.continue[3],
                                          limits = pal.continue.limit,
                                          breaks = pal.continue.break)

        }else if(length(pal.continue) == 2){
            p = p +  scale_fill_gradient(low = pal.continue[1], high = pal.continue[2],
                                          limits = pal.continue.limit,
                                          breaks = pal.continue.break)
        }else{
            stop('pal.continue must be a vector of 3: c(low, mid, high ) or 2: c(low, high) \n')
        }


    }else{
        # discrete values (factor levels setting)
        cat(paste0('fill groups including level numbers :', length(unique(plot_df$value)), '\n'))

        if(!missing(fill.factor.levels)){
            plot_df$value <- factor(plot_df$value, levels = fill.factor.levels )
        }

        # plotting
        p = ggplot(data = plot_df, aes(x = name,y = row_var_name))+
            geom_tile(aes(fill = value), color = border.color) +
            scale_fill_manual(values = pal.discrete)
    }


    # labs
    p = p + labs(fill = legend.title)


    # heatmap_theme
    # x lab setting / colunm names
    if(show.colNames == T){
        axis_x_text_setting = element_text(colour = 'black', angle = col.text.angle, size = col.text.size, hjust = 0.5, vjust = 0.5)
    }else{
        axis_x_text_setting = element_blank()
    }

    # y lab setting / row names
    if(show.rowNames == T){
        axis_y_text_setting = element_text(colour = 'black', angle = row.text.angle, size = row.text.size,  hjust = 1, vjust = 0.5)
    }else{
        axis_y_text_setting = element_blank()
    }

    # adding theme
    p = p + theme_minimal() +
        theme(panel.grid = element_blank(),
              legend.position = legend.position,
              legend.text = element_text(size = legend.text.size),
              legend.title = element_text(size = legend.title.size),
              axis.ticks.y = element_blank(),
              axis.title = element_blank(),
              axis.text.x = axis_x_text_setting,
              axis.text.y = axis_y_text_setting
              )

    return(p)
}




#' plotting only one annotation column for heatmap with ggplot2, ac could be discrete / continues values
#'
#' @param ac a dataframe ac for annotation the column for heatmap, row names must be consistant with the heatmap columns; colunms can be  one or more, but only one column used for annotaition setting
#' @param group.name the colunm name in ac data frame, picked for annotation
#' @param fill_levels if the filling is discrete values, can define the factors orders using levels
#' @param pal.continuous if the filling is continue values, using continues pal, a two value vector: c('low color', 'high color'), default c("white", "red")
#' @param pal.discrete if the filling is discrete values, using discrete pal, if denfined the length of the pal must be consistant with the length of fill factor level,
#'                    default: ggsci::pal_lancet(palette = c("lanonc"), alpha = 0.6)(9)
#' @param legend.title.size text size of the ac bar legend title, default 18
#' @param legend.text.size text size of the ac bar legend lab, default 16
#' @param legend.title title of the ac legend bar, if not given, using the column name of the ac data frame
#'
#' @return  a ggplot2 object of one annotation column of a heatmap
#' @export
#'
#' @examples ac_heatmap_ggplot2(ac = ac, group.name = 'group_list')
ac_heatmap_ggplot2 <- function(ac, group.name, fill_levels,
                               pal.continuous = c("white", "red"),
                               pal.discrete = ggsci::pal_lancet(palette = c("lanonc"), alpha = 0.6)(9),
                               legend.title, legend.title.size=18, legend.text.size=16){

    # ac data frame
    stopifnot(is.data.frame(ac))
    cat('make sure rownames of the anotation dataframe consistant with the column names of the heatmap !\n
        identical(rownames(ac), colnames(df)) == T')

    # as sample information dataframe, rownames as sample names(indential with columns of the heatmap), colums with different sample vars
    # only one group name can be given, than using aplot package to patch the heatmap
    stopifnot(length(group.name) == 1)

    ac_plot_df <- data.frame(Sample_id = rownames(ac),
                             Yaxis = "Annotation",
                             group = ac[[group.name]])

    # if define the ac title
    if(missing(legend.title)){
        legend.title = group.name
    }


    # factor levels
    if(!missing(fill_levels) & !is.numeric(ac[[group.name]])){
        ac_plot_df$group = factor(ac_plot_df$group, levels = fill_levels)
    }else if(is.numeric(ac[[group.name]])){
        cat('numeric values did not need levels !')
    }else{
        cat('no fill levels given !')
    }


    # adding annotation columns
    ac_theme <- theme(panel.grid = element_blank(),
                      legend.position = "right",
                      legend.text = element_text(size = legend.text.size),
                      legend.title = element_text(size = legend.title.size),
                      axis.ticks.y = element_blank(),
                      axis.title = element_blank(),
                      axis.text.x =  element_blank(),
                      axis.text.y =  element_blank())

    # plotting
    p = ggplot(data = ac_plot_df, aes(Sample_id, Yaxis, fill = group)) +  # notice xvalue/Sample_id consistant with heatmap
        geom_tile()

    if(is.numeric(ac_plot_df$group)){
        p = p + scale_fill_continuous(low = pal.continuous[1], high = pal.continuous[2])
    }else{
        cat('pal.discrete level must be of the same number with the fill group levels \n ')
        # stopifnot(length(pal.discrete) == length(unique(ac_plot_df$group)))
        p = p + scale_fill_manual(values = pal.discrete)
    }


    # labs
    p = p + labs(fill = legend.title)

    # theme
    p = p + theme_void() + ac_theme


    return(p)
}




