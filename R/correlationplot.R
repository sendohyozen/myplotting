
#' Convert numeric p-values into significance levels
#'
#' @param pval pval numeric vector; p-value(s) to be converted
#'
#' @return character vector;
#' @export
pLevel <- function(pval) {
    ifelse(pval < 0.001, "p < 0.001",
           ifelse(pval < 0.01, "p < 0.01",
                  ifelse(pval < 0.05, "p < 0.05",
                         ifelse(pval < 0.1, "p < 0.1", "n.s."))))
}




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
#' @param base.size base text size of the theme default 18
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
                             base.size=18, legend.position = 'top', legend.size =12,
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

    cat(paste0(x_value, 'and', y_value,'  cor value:', cor,', p value:', pv))
    label_anno = paste0("  cor ", cor,   '\n  ', pLevel(pv), '  ' )  # without space after log text might be covered



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
#' @param base.size base text size of the theme default 18
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
                             base.size=18, legend.position = 'top', legend.size =12,
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
#' @param cor.matrix  data frame with different vars: row samples, columns different var names (cor analysis in function)
#' @param show  can only be mix or consist, default mix:with shapes (upper) and cor values (lower)
#' @param shape can be 'circle', 'square', 'ellipse', 'number', 'pie', 'shade' and 'color'; default ellipse
#' @param title plot title
#' @param lab.color color of labs
#' @param lab.size text size of labs
#' @param pal color bar for cor value, show = mix can't be used .only in COL2 form
#' @param method cor analyse method can be "pearson", "spearman", "kendall", default spearman
#' @param tpye used in the consist model, can be full','lower','upper'(defult)
#' @param order the order to put factors,defualt AOE
#' @param addrect rect to add on the plot ,only order of "hclust" can be showed, defualt 2
#'
#' @return a cor plot for multi factors
#' @export
#'
#' @examples cor_matrix(cor.matrix = mtcars, show = 'upper', shape = 'pie')
cor_matrix <- function(cor.matrix, method='spearman', show="mix", tpye = 'upper', shape = "ellipse", title='', lab.color='black', lab.size = 1,
                       pal = corrplot::COL2('RdBu', 10),  # COL2(diverging = c("RdBu", "BrBG", "PiYG", "PRGn", "PuOr", "RdYlBu"), n = 200)
                       order = 'AOE', addrect = 2){

    cor_df = cor(cor.matrix, method = method)  # can be "pearson", "spearman", "kendall"

    if(show=='mix'){

        corrplot::corrplot.mixed(cor_df,
                                 upper = shape,  # upper shape
                                 lower = 'number',  # lower cor values
                                 # col = pal,
                                 title = title,
                                 order = order,
                                 tl.col = lab.color,
                                 tl.cex = lab.size)

    }else if(show == 'consist'){

        corrplot::corrplot(cor_df,
                           method = shape, # shape of cor, can be 'circle' (default), 'square', 'ellipse', 'number', 'pie', 'shade' and 'color'
                           col = pal,
                           type = tpye, # where to show the shapes，can be 'full'(defult),'lower','upper'
                           order = order,
                           addrect = addrect,
                           title = title,
                           tl.col = lab.color,
                           tl.cex = lab.size,
                           addgrid.col = 'black' # color the the grid
        )

    }else{
        stop('show value can only be mix or consist!')
    }
}




#'  show correlation plot for multiple variables (more than two vars, show by matrix, using ggcor package)
#'
#' @param cor.matrix  data frame with different vars: row samples, columns different var names (cor analysis in function)
#' @param shape can be 'circle', 'square'; default square
#' @param text.size text size default 2.5
#'
#' @return a cor plot for multi factors
#' @export
#'
#' @examples cor_matrix2(cor.matrix = df_expr)
cor_matrix2 <- function(cor.matrix, shape = 'square' , text.size=2.5){

    if(shape == 'square'){

        p = ggcor::quickcor(cor.matrix, cor.test = T) +
            geom_square(data = get_data(type = "lower", show.diag = F)) +
            geom_mark(data = get_data(type = "upper", show.diag = F), size = text.size) +
            geom_abline(slope = -1, intercept = ncol(cor.matrix)+1)

    }else if(shape == 'circle'){
        p =  ggcor::quickcor(cor.matrix, cor.test = T) +
            geom_circle2(data = get_data(type = "lower", show.diag = F)) +
            geom_mark(data = get_data(type = "upper", show.diag = F), size = text.size) +
            geom_abline(slope = -1, intercept = ncol(cor.matrix)+1)
    }else{
        cat('shape can only be square or circle!')
    }

    return(p)
}




#' calculate the 95% confidence interval of cor coefficient
#'
#' @param cor.estimate   cor coefficient analysed from cor.test function
#' @param total.sample  number of total samples used
#'
#' @return a vector of  95% lower and upper  confidence of cor coefficient
#' @export
#'
#' @examples cor_95CI(cor.estimate = dd$estimate, total.sample = nrow(data))
cor_95CI <- function(cor.estimate, total.sample){
    # R code to calculate the confidence interval of Pearson's correlation coefficient using Fisher Z-transformation
    # Assume we have some sample correlation coefficient r and sample size N
    r <- cor.estimate # Example correlation coefficient
    N <- total.sample # Example sample size

    # Apply Fisher Z-Transformation to r
    z <- 0.5 * log((1 + r) / (1 - r))

    # Compute Standard Error (SE) of z
    SE <- 1 / sqrt(N - 3)

    # Decide on the confidence level, typically 95%, which corresponds to Z value of 1.96
    Z <- qnorm(0.975) # Two-tailed for 95% confidence interval

    # Calculate the confidence interval for z
    CI_lower_z <- z - (Z * SE)
    CI_upper_z <- z + (Z * SE)

    # Transform the confidence intervals back to r
    CI_lower_r <- (exp(2 * CI_lower_z) - 1) / (exp(2 * CI_lower_z) + 1)
    CI_upper_r <- (exp(2 * CI_upper_z) - 1) / (exp(2 * CI_upper_z) + 1)

    # Return the confidence interval in terms of r
    CI <- c(CI_lower_r, CI_upper_r)

    return(CI)
}




#' Calculate the cor coefficient with 95%CI from two factors analysed using x, y var names
#'
#' @param data Raw data frame with row of samples, colunames of multiple factors
#' @param factor1 column names of x var name
#' @param factor2 column names of y var name
#' @param method analysis methods can be "pearson", "spearman", "kendall", default spearman
#'
#' @return a vector of factor1, factor2, method, cor coefficient, p value, cor 95%lower, cor 95%upper
#' @export
#'
#' @examples cor.analysis(data = dat, gene1 = "CD8A", gene2 = 'CXCL9', method = 'spearman')
cor.analysis = function(data, factor1, factor2, method= 'spearman'){

    dd = cor.test(x = data[[factor1]], y = data[[factor2]], method = method)

    cor  = dd$estimate
    pv = dd$p.value
    cor_lower = cor_95CI(cor.estimate = dd$estimate, total.sample = nrow(data))[1]
    cor_upper = cor_95CI(cor.estimate = dd$estimate, total.sample = nrow(data))[2]

    cor.all = c(factor1, factor2, method, cor, pv, cor_lower, cor_upper)
    vector.name = c('factor1', 'factor2', 'method', 'cor', 'pv', 'cor_lower', 'cor_upper')

    cor.res = setNames(cor.all, vector.name)

    return(cor.res)
}





#' corelation showing on two different matrix, not all the correlation of factors in one row
#'
#' @param df_matrix correlation show using matrix
#' @param df_link  correlation show using link lines on the factors of the matrix
#' @param text.size text size ,default 2.5
#'
#' @return a link line corplot
#' @export
#'
#' @examples cor_link_plot(df_matrix = df_expr, df_link = immucell_df)
cor_link_plot <- function(df_matrix, df_link, text.size=2.5){

    # link between the two matrix, show of link lines
    link_cor = ggcor::correlate(df_link , df_matrix, cor.test = T) %>%
        as_cor_tbl() %>%
        select(spec = .row.names, env = .col.names, r, p.value) %>%
        mutate(
            rd = cut(r, breaks = c(-Inf, 0.2, 0.4, Inf),
                     labels = c("< 0.2", "0.2 - 0.4", ">= 0.4")),

            pd = cut(p.value, breaks = c(-Inf, 0.01, 0.05, Inf),
                     labels = c("< 0.01", "0.01 - 0.05", ">= 0.05")))


    # link cor plot
    p = ggcor::quickcor(df_matrix, type = "upper", cor.test = T) +
        geom_square() +
        geom_mark(size = text.size) +
        ggcor::anno_link(data = link_cor, aes(color = pd, size = rd)) +
        scale_size_manual(values = c(0.5, 1, 2)) +
        scale_colour_manual(values = c("#56B4E9", "#E69F00", "#999999")) +
        scale_fill_gradient2(midpoint = 0.5,low = "#80B1D3",mid = "white",high = "#8214A0",space = "Lab")


    return(p)
}



#' corelation showing on two different matrix, not all the correlation of factors in one row, line show with mantel test
#'
#' @param df_matrix correlation show using matrix
#' @param df_link correlation show using link lines on the factors of the matrix
#' @param link_column_list a list object define link points on the df_line,using colnum numbers, can be multi-columns using :
#' @param cor.method cor analysis methods ,default spearman
#' @param text.size  text size ,default 2.5
#'
#' @return a link line corplot using mantel test
#' @export
#'
#' @examples cor_link_mantel(df_matrix= df_expr, df_link=immucell_df, link_column_list=list(B = 1, CD4T = 2, CD8T = 3))
cor_link_mantel <- function(df_matrix, df_link, link_column_list,
                            cor.method = 'spearman', text.size=2.5){

    # Mantel test
    mantel <- linkET::mantel_test(spec = df_link, env = df_matrix, spec_select = link_column_list, mantel_fun = 'mantel')

    # define link line color and width
    mantel <- mutate(mantel,
                     rd = cut(r, breaks = c(-Inf, 0.2, 0.4, Inf), labels = c('< 0.2', '0.2 - 0.4', '>= 0.4')),
                     pd = cut(p, breaks = c(-Inf, 0.01, 0.05, Inf), labels = c('< 0.01', '0.01 - 0.05', '>= 0.05'))
    )

    # link cor plot with mantel test results
    p = linkET::qcorrplot(correlate(df_matrix, method = cor.method), type = 'upper', diag = FALSE) +   # cor matrix on the upper
        geom_square() +   # square heatmap matrix
        geom_mark(sep = '\n', size = text.size, sig.thres = 0.05) +  # show cor Spearman results and p value
        geom_couple(aes(color = pd, size = rd), data = mantel, curvature = nice_curvature()) +  # metal results lines
        scale_fill_gradientn(colors = c('#053061', '#68A8CF', 'white', '#F7B394', '#67001F'), limits = c(-1, 1)) +  # heatmap color
        scale_size_manual(values = c(0.5, 1, 2)) +  # Mantel results define line width
        scale_color_manual(values = c('#D95F02', '#1B9E77', '#E0E0E0')) +  # mantel p value for line colors
        guides(color = guide_legend(title = "Mantel's p", order = 1),  # legend
               size = guide_legend(title = "Mantel's r", order = 2),
               fill = guide_colorbar(title = "Spearman's r", order = 3)) +
        theme(legend.key = element_blank())


    return(p)
}
