#' A cor plot with margin plots analysed using x, y var names
#'
#' @param data dataframe with long form; row samples, columns different varnames, such as genes, data type numeric/continuous data
#' @param x_value varname of x ,such as gene1
#' @param y_value varname of y ,such as gene2
#' @param log.Data if log the data, can be none, log2, log10
#' @param point.size size of the points,default 2
#' @param point.color color of the points, default "#025259"
#' @param text.position text position of the cor/p value, can be topright, topleft, bottomright, bottomleft, none; default topright
#' @param text.size text size ,default 5
#' @param text.color text color ,default red
#' @param Marginal.type type of Marginal plot, can be histogram, density, boxplot, densigram, none; default histogram
#' @param Marginal.color color of Marginal plot lines, defualt black
#' @param Marginal.fill color of Marginal plot fillings, defualt '#d94f04'
#' @param title plot title
#' @param x_lab plot x lab
#' @param y_lab plot y lab
#' @param base.size base text size of the theme default 15
#' @param legend.position legend position default top
#' @param legend.size legend text size default 12
#' @param plot.theme plot.theme
#' @param cor.methods cor analysis methods: pearson, kendall, spearman, default spearman
#'
#' @return  ggplot plot object
#' @export
#'
#' @examples corrplot_Margin(data = dat, x_value = 'LATS2', y_value = 'ALKBH5', base.size = 20)
#'           corrplot_Margin(data = dat, x_value = 'LATS2', y_value = 'ALKBH5', log.Data = 'log10', text.position = 'bottomright', Marginal.type = 'densigram', title = 'corplot')
corrplot_Margin <-  function(data,  x_value, y_value, log.Data = 'none', cor.methods = 'spearman',
                             point.size = 2, point.color = "#025259",
                             text.position = 'topright' , text.size = 5, text.color = 'red',
                             Marginal.type = 'histogram', Marginal.color = 'black', Marginal.fill= '#d94f04',
                             title = NULL, x_lab, y_lab,
                             base.size=15, legend.position = 'top', legend.size =12,
                             plot.theme ){

    if(missing(x_lab)){ x_lab = x_value}
    if(missing(y_lab)){ y_lab = y_value}

    # plot data
    plot_data = data.frame(x = as.numeric(data[[x_value]]),
                           y = as.numeric(data[[y_value]]) )

    # if log data
    if(log.Data=='none'){
        plot_data = plot_data

    }else if(log.Data=='log2'){
        plot_data = log2(plot_data)

    }else if(log.Data=='log10'){
        plot_data = log10(plot_data)

    }else{
        stop('log.Data can only be none , log2, log10 !')

    }

    # get cor value
    if(cor.methods %in% c("pearson", "kendall", "spearman")){
        dd = cor.test(plot_data$x, plot_data$y,
                      method = cor.methods)
    }else{
        stop(" the cor.methods must be in pearson, kendall, spearman !")
    }


    cor = round(dd$estimate, 4)
    pv = round(dd$p.value, 4)

    label_anno = paste0("cor: ", cor, '\n', "pvalue: ", pv )

    cat(paste0(x_value, 'and', y_value,'  cor value:', cor,', p value:', pv))

    # plotting
    p = ggplot(plot_data, aes(x, y)) +
        geom_point(size = point.size, colour = point.color) +
        ggtitle(title) +  xlab(x_lab) +  ylab(y_lab) +
        stat_smooth(method = lm, level = 0.99)  # fit line


    # add cor , p value
    xmax = max(plot_data$x) ; xmin = min(plot_data$x)
    ymax = max(plot_data$y) ; ymin = min(plot_data$y)

    if(text.position=='topright'){
        p = p + annotate("text", x= xmax, y =1*ymax, label = label_anno,  col = text.color, size = text.size, hjust =1, vjust=1)  # vjust=1, text top up with ymax，hjust = 1 right with xmax

    }else if(text.position=='topleft'){
        p = p + annotate("text", x= xmin, y =1*ymax, label = label_anno,  col = text.color, size = text.size, hjust =0, vjust=1)  # vjust=1, text top up with ymax，hjust = 0 left with xmin

    }else if(text.position=='bottomright'){
        p = p + annotate("text", x= xmax, y =1*ymin, label = label_anno,  col = text.color, size = text.size, hjust =1, vjust=0)  # vjust=0, text bottom down with ymax，hjust = 1 right with xmax

    }else if(text.position=='bottomleft'){
        p = p + annotate("text", x= xmin, y =1*ymin, label = label_anno,  col = text.color, size = text.size, hjust =0, vjust=0)  # vjust=0, text bottom down with ymax，hjust = 0 left with xmin

    }else if(text.position=='none'){
        p = p

    }else{
        stop('the cor/p value position can only be topright, topleft, bottomright, bottomleft, none !')
    }


    # theme
    if(missing(plot.theme)){
        p = p + theme_bw(base_size = base.size) +
            theme(plot.background = element_rect(fill='white', color='white'),
                  plot.title = element_text(colour = "black", hjust = 0.5),
                  legend.position = legend.position,
                  legend.title = element_blank(),
                  legend.text= element_text(color="black", size = legend.size))
    }else{
        p = p + plot.theme
    }


    # Marginal type
    if(Marginal.type=='histogram'){

        p = ggExtra::ggMarginal(p, type = "histogram", color = Marginal.color, fill= Marginal.fill, alpha = 0.7)

    }else if(Marginal.type=='density'){

        p = ggExtra::ggMarginal(p, type = "density", color = Marginal.color, alpha = 0.7)

    }else if(Marginal.type=='boxplot'){

        p = ggExtra::ggMarginal(p, type = "boxplot", color = Marginal.color, fill= Marginal.fill, alpha = 0.7)

    }else if(Marginal.type=='densigram'){

        p = ggExtra::ggMarginal(p, type = "densigram", color = Marginal.color, fill= Marginal.fill, alpha = 0.7)

    }else if(Marginal.type=='none'){
        p = p

    }else{
        stop('the Marginal type can only be histogram, density, boxplot, densigram, none !')
    }


    return(p)
}







#' A cor plot with rug analysed using x, y var names
#'
#' @param data dataframe with long form; row samples, columns different varnames, such as genes, data type numeric/continuous data
#' @param x_value varname of x ,such as gene1
#' @param y_value varname of y ,such as gene2
#' @param log.Data if log the data, can be none, log2, log10
#' @param cor.methods cor analysis methods: pearson, kendall, spearman, default spearman
#' @param point.size size of the points,default 1
#' @param point.color color of the points, default "#984ea3"
#' @param smoothline.color color of the smoothline, default "#fdc086"
#' @param rug.color color of the "#7fc97f"
#' @param text.size size of the text(cor/p value), default 5
#' @param title plot title
#' @param x_lab plot x lab
#' @param y_lab plot y lab
#' @param base.size base text size of the theme default 15
#' @param legend.position legend position default top
#' @param legend.size legend text size default 12
#' @param plot.theme plot.theme
#'
#' @return ggplot plot object
#' @export
#'
#' @examples corrplot_Rug(data = sur_data, x_value = 'ASNS', y_value = 'ATF4', text.size = 5, base.size = 20)
corrplot_Rug <-  function(data,  x_value, y_value, log.Data = 'none', cor.methods = 'spearman',
                             point.size = 1, point.color = "#984ea3",
                             smoothline.color = "#fdc086",  rug.color = "#7fc97f", text.size=5,
                             title = NULL, x_lab, y_lab,
                             base.size=15, legend.position = 'top', legend.size =12,
                             plot.theme ){

    if(!cor.methods %in% c("pearson", "kendall", "spearman")){
        stop(" the cor.methods must be in pearson, kendall, spearman !")
    }

    if(missing(x_lab)){ x_lab = x_value}
    if(missing(y_lab)){ y_lab = y_value}

    # plot data
    plot_data = data.frame(x = as.numeric(data[[x_value]]),
                           y = as.numeric(data[[y_value]]) )

    # if log data
    if(log.Data=='none'){
        plot_data = plot_data

    }else if(log.Data=='log2'){
        plot_data = log2(plot_data)

    }else if(log.Data=='log10'){
        plot_data = log10(plot_data)

    }else{
        stop('log.Data can only be none , log2, log10 !')

    }

    # plotting
    p = ggplot(plot_data, aes(x, y)) +
        geom_point(size = point.size, colour = point.color) +
        geom_smooth(method=lm,  se=T, na.rm=T, fullrange=T, size=1, col=smoothline.color) +
        geom_rug(col = rug.color) +
        stat_cor(method = cor.methods, digits = 3, size = text.size) +
        ggtitle(title) +  xlab(x_lab) +  ylab(y_lab)


    # theme
    if(missing(plot.theme)){
        p = p + theme_bw(base_size = base.size) +
            theme(plot.background = element_rect(fill='white', color='white'),
                  plot.title = element_text(colour = "black", hjust = 0.5),
                  legend.position = legend.position,
                  legend.title = element_blank(),
                  legend.text= element_text(color="black", size = legend.size))
    }else{
        p = p + plot.theme
    }

    return(p)

}




#' show correlation plot for multiple variables (more than two vars, show by matrix, not scatter points)
#'
#' @param cor.matrix  data frame with different vars: row samples, columns different var names (all in cor analysis)
#' @param show  can only be mix or upper, default mix:with shapes (upper) and cor values (lower)
#' @param shape can be 'circle', 'square', 'ellipse', 'number', 'pie', 'shade' and 'color'; default ellipse
#' @param title plot title
#' @param lab.color color of labs
#' @param lab.size text size of labs
#' @param pal color bar for cor value
#' @param method cor analyse method can be "pearson", "spearman", "kendall", default spearman
#'
#' @return a cor plot for multi factors
#' @export
#'
#' @examples cor_matrix(cor.matrix = mtcars, show = 'upper', shape = 'pie')
cor_matrix <- function(cor.matrix, method='spearman', show="mix", shape = "ellipse", title='', lab.color='black', lab.size = 1,
                        pal = colorRampPalette(c("#2254aa","white","#f3261f"))(100)
                       ){

    cor_df = cor(cor.matrix, method = method)  # can be "pearson", "spearman", "kendall"

    if(show=='mix'){

        corrplot::corrplot.mixed(cor_df,
                                 upper = shape,  # upper shape
                                 lower = 'number',  # lower cor values
                                 # col = pal,
                                 title = title,
                                 order = 'AOE',
                                 tl.col = lab.color,
                                 tl.cex = lab.size)

    }else if(show == 'upper'){

        corrplot::corrplot(cor_df,
                           method = shape, # shape of cor, can be 'circle' (default), 'square', 'ellipse', 'number', 'pie', 'shade' and 'color'
                           # col = pal,
                           type = 'upper', # where to show the shapes，can be 'full'(defult),'lower','upper'
                           order = 'AOE',  # order by the cor value
                           title = title,
                           tl.col = lab.color,
                           tl.cex = lab.size,
                           addgrid.col = 'black' # color the the grid
        )

    }else{
        stop('show value can only be mix or upper!')
    }
}




