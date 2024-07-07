
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






#' correlation showing on two different matrix, not all the correlation of factors in one row, line show with spearman/pearson/mantel test
#'
#' @param df_matrix correlation show using matrix
#' @param df_link correlation show using link lines on the factors of the matrix
#' @param mantel  if using mantel test, default F
#' @param link_column_list if mantel test used; a list object define link points on the df_line,using colnum numbers, can be multi-columns using :
#' @param cor.method cor analysis methods ,default spearman
#' @param sig.adding if show significance labs on the matrix, default T
#' @param r.line.break a vector of break points (2 or 3 numbers) setting for abs value of r , default c(0.2, 0.6)
#' @param matrix.pal a color setting for the matrix, must be a vector of 5 colors;
#' @param line.pal a color setting for the line for postive or negative relationship, a vector of 2 colors
#' @param text.size  text size of the matrix, default 16
#' @param lab.size  lab text size of line link points default 5
#' @param legend.title.size text size of the legend title default 14
#' @param legend.text.size text size of the legend text default 12
#'
#' @return a link line corplot
#' @export
#'
#' @examples cor_link_plot(df_matrix= df_expr, df_link=immucell_df, mantel=T, link_column_list=list(B = 1, CD4T = 2, CD8T = 3))
#'  cor_link_plot(df_matrix = ko2 , df_link = ko1, r.line.break = c(0.3, 0.7))
#'
cor_link_plot <- function(df_matrix, df_link, mantel=F, link_column_list,
                          cor.method = 'spearman', sig.adding=T,
                          r.line.break = c(0.2, 0.6),
                          matrix.pal = c("#730922", "#e4795d", "#fbfbf7", "#4895c7", "#2e405f"), line.pal=c("#009f76" ,"#d95e27") ,
                          text.size=16, lab.size=5, legend.title.size=14, legend.text.size=12){

    # 计算组内（单矩阵内）间相关性（未进行P值矫正）：
    cor2 = linkET::correlate(df_matrix, method = cor.method)#所有因子都符合正态分布的可能性较小，这里采用Spearman相关性进行演示。如果数据都符合正态分布，将Spearman改成Pearson。下同。
    corr2 = cor2 %>% linkET::as_md_tbl()



    #计算组间（2个数据框间）相关性（未进行P值矫正）:
    if(mantel == T){
        # Mantel test
        cor3 <- linkET::mantel_test(spec = df_link, env = df_matrix, spec_select = link_column_list, mantel_fun = 'mantel')
        link.method = "|Mantel's r|"


    }else{
        # No mental spearman /pearson
        cor3 = linkET::correlate(df_link, df_matrix, method = cor.method)
        link.method = paste0("|",cor.method,"'s r|")
    }

    # 转换格式
    corr3 = cor3 %>% linkET::as_md_tbl()



    #组合网络热图绘制
    #生物因子和环境因子间相关性系数和P值分区（break点一般2个或3个，否则太细难区分）
    if(length(r.line.break) == 2){
        break_labs = c(paste0("<", r.line.break[1]),
                       paste0(r.line.break[1], "-", r.line.break[2]),
                       paste0(r.line.break[2], "-1"))

        break.line.sets  = setNames(c(0.1, 0.4, 0.8), break_labs)  # 后面连线粗细的设置关联

    }else if(length(r.line.break) == 3){
        break_labs = c(paste0("<", r.line.break[1]),
                       paste0(r.line.break[1], "-", r.line.break[2]),
                       paste0(r.line.break[2], "-", r.line.break[3]),
                       paste0(r.line.break[3], "-1"))

        break.line.sets  = setNames(c(0.1, 0.3, 0.5, 0.7), break_labs)  # 后面连线粗细的设置关联


    }else{
        stop('r.line.break  must be a vector of two or three numbers, not including the inital 0 and the end 1 !')
    }


    # 切割数据相关系数与p值
    r.p.data.plot = corr3 %>%
        mutate(r.sign = cut(r, breaks = c(-Inf, 0, Inf),
                            labels = c("Negative", "Positive")),
               p.sign = cut(p, breaks = c(0, 0.05, Inf),
                            labels = c("P<0.05", "P>=0.05"),
                            include.lowest = TRUE, # 较小值是否为闭区间,
                            right = FALSE),  # 较大值是否为闭区间
               r.abs = cut(abs(r), breaks = c(0, r.line.break, 1),
                           labels = break_labs,
                           include.lowest = TRUE,#较小值是否为闭区间,
                           right = FALSE),#较大值是否为闭区间
        )

    # print(r.p.data.plot$`r.abs`)


    # 绘制相关性热图-半角矩形:
    p = linkET::qcorrplot(cor2,
                          grid_col = "grey50", # 网格线颜色
                          grid_size = 0.2, # 网格线粗细
                          type = "upper", # 图形是上三角，想展示在下三角的话upper改lower
                          diag = F) + # 不展示对角线，想展示的话F改为T
        linkET::geom_square()

    # 添加显著性标签：
    if(sig.adding == T){
        p = p + linkET::geom_mark(size = 4, # * 符号大小
                                  only_mark = T,
                                  sig_level = c(0.05, 0.01, 0.001), # <0.05一颗*，<0.01两个**，<0.001三颗***。这个选项是将P值分为这三个level
                                  sig_thres = 0.05, # 显著性阈值
                                  colour = 'black') # *符号的颜色

    }


    # 在相关性热图上添加Spearman连线:
    p = p + linkET::geom_couple(data = r.p.data.plot,   # 添加网络图并映射数据
                                aes(colour = r.sign,  # 连线颜色由r.p.data.plot中的r.sign确定
                                    size = r.abs,  # 连线粗细由r.p.data.plot中的r.abs确定
                                    linetype = p.sign), # 线型由r.p.data.plot中的p.sign确定
                                nudge_x = 0.15, # 标签距离点的距离
                                curvature = 0.1, # 曲线弯曲程度
                                label.fontface = 1, # 字体，1是非粗体，2是粗体。
                                label.family = "sans", # serif表示是Times New Roman字体，想用Arial的话serif改sans
                                label.size = lab.size) # 字体大小


    #继续美化连线:
    p = p + scale_size_manual(values = break.line.sets) +  # 为每个分类设置连线粗细, 在前面break点时已设置好setName
        scale_colour_manual(values = c("Negative" =  line.pal[1],
                                       "Positive" =  line.pal[2] )) +  # 为每个分类设置颜色
        scale_linetype_manual(values = c("P<0.05" = "solid",
                                         "P>=0.05" = "dashed")) +
        scale_fill_gradientn(colours = rev(matrix.pal), # 自定义颜色,和上面p4颜色略不同，提供了另一种自定义颜色的示例。
                             breaks = seq(-1, 1, 0.5),   # 5 个阶段，需要5个颜色
                             limits = c(-1, 1)) + #设置图例范围
        # geom_diag_label()+
        guides(
            fill = guide_colorbar(title = paste0(cor.method, "'s r"),
                                  barwidth = 1,barheight = 8, order = 1),
            linetype = guide_legend(title = NULL,override.aes = list(size = 6,linewidth = 0.6, order = 3)),
            colour = guide_legend(title = NULL,override.aes = list(size = 1,linewidth = 0.6, order = 4)),
            size = guide_legend(title = link.method,
                                override.aes = list(colour = "black",size = 1),
                                order = 2)  #此处均为标题设置
        ) +
        theme(
            axis.text=element_text(color="black",size=text.size, family = "sans",face = "plain"),
            axis.text.x.top =element_text(color="black",size=text.size, family = "sans",face = "plain",angle = 90,hjust = 0,vjust = 0.5),
            legend.key = element_blank(), #删除图例灰色背景(为了画面更干净)
            # legend.key.size = unit(0.36, "cm"), #调整图例整体大小
            # legend.spacing.y = unit(0.8,"cm"),#调整图例之间间距
            # legend.key.spacing.y = unit(0.2,"cm"),#调整图例之间间距
            legend.text = element_text(color="black",size=legend.text.size, family = "sans",face = "plain"), # 调整图例文本字体
            legend.title = element_text(color="black",size=legend.title.size, family = "sans",face = "plain",margin = margin(b = 12)) # 调整图例标题文本字体
        )



    return(p)
}





#' Heatmap showing correlation of two different matrix,  could add p significance (*) on the heatmap
#'
#' @param df_row dataframe with column names on the row of the correlation heatmap
#' @param df_column dataframe with column names on the column of the correlation heatmap
#' @param cor.method methods for correlation analysis, default spearman
#' @param sig.adding if adding  significance (*) on the heatmap, default T
#' @param pal color for the heatmap , a vector of 3 color numbers c(low, mid, high),  default c("#0074b3", "white", "#982b2b")
#' @param x.lab.size text size of the row name of the heatmap; default 14
#' @param x.lab.angle text angle of the row name of the heatmap; default 90
#' @param y.lab.size text size of the column name of the heatmap; default 14
#' @param y.lab.angle text angle of the column names of the heatmap; default 0
#' @param legend.title.size text size of the legend title, default 14
#' @param legend.text.size text size of the legend text, default 12
#' @param legend.limit  limit of the the legend bar,  a vector of 2 numbers c(low, high), default c(-1, 1)
#' @param legend.midpoint middle point of the legend bar , default 0
#' @param legend.break breaks of the legend bar, a vector of numbers in the limits, default seq(-1,1,0.5)
#'
#' @return a ggplot object of heatmap showing correlation of two different matrix
#' @export
#'
#' @examples  cor_heatmap_plot(df_row = dat2, df_column = dat1, cor.method = 'spearman', sig.adding = F,
#'                            legend.limit = c(-0.2, 1), legend.midpoint = 0.4, legend.break = seq(-0.2, 1, 0.4))
cor_heatmap_plot <- function(df_row, df_column, cor.method = 'spearman', sig.adding=T,
                             pal = c("#0074b3", "white", "#982b2b"),
                             x.lab.size=14, x.lab.angle=90,
                             y.lab.size=14, y.lab.angle=0,
                             legend.title.size=14, legend.text.size=12,
                             legend.limit = c(-1, 1), legend.midpoint=0, legend.break = seq(-1,1,0.5) ){

    # 相关性计算
    dd = linkET::correlate(x = df_row, y = df_column, method = cor.method) %>% linkET::as_md_tbl() # 两个矩阵的相关性 ，需要格式转换为tibble，为长数据
    colnames(dd)[1:2] =  c('row', 'column') # 重命名列

    # 相关性热图可视化
    p = ggplot(data = dd, aes(x = row, y = column)) +
        geom_tile(aes(fill = r),color = "grey") +
        scale_fill_gradient2(low = pal[1], mid = pal[2], high = pal[3],
                             name = "Correlation",
                             limits = legend.limit,  midpoint = legend.midpoint, breaks = legend.break )

    # 添加显著标志
    if(sig.adding==T){

        tmp = case_when(as.vector(dd$p) < 0.001~"****",
                        as.vector(dd$p) < 0.001~"***",
                        as.vector(dd$p) < 0.01~"**",
                        as.vector(dd$p) < 0.05~"*",
                        T~ "")
        dd$label = tmp

        p = p + geom_text(data = dd, aes(x = row, y = column, label = label), vjust = 0.7)
    }


    # 主题调整
    mytheme = theme(panel.grid = element_blank(),
                    legend.position = "right",
                    legend.text = element_text(size = legend.text.size),
                    legend.title = element_text(size = legend.title.size),
                    axis.ticks.y = element_blank(),
                    axis.title = element_blank(),
                    axis.text.x = element_text(colour = 'black', size = x.lab.size, angle = x.lab.angle, hjust = 1),
                    axis.text.y = element_text(colour = 'black', size = y.lab.size, angle = y.lab.angle))

    p = p + theme_minimal() +
        mytheme


    return(p)
}



#'  Heatmap showing correlation of two different matrix,  could show p values with/without significance (P < 0.05)
#'
#' @param df_row dataframe with column names on the row of the correlation heatmap
#' @param df_column dataframe with column names on the column of the correlation heatmap
#' @param cor.method methods for correlation analysis, default spearman
#' @param pal color for the heatmap , a vector of 3 color numbers c(low, mid, high),  default c("#0074b3", "white", "#982b2b")
#' @param x.lab.size text size of the row name of the heatmap; default 14
#' @param x.lab.angle  text angle of the row name of the heatmap; default 90
#' @param y.lab.size text size of the column name of the heatmap; default 14
#' @param y.lab.angle text angle of the column names of the heatmap; default 0
#' @param legend.title.size text size of the legend title, default 14
#' @param legend.text.size  text size of the legend text, default 12
#' @param legend.limit  limit of the the legend bar,  a vector of 2 numbers c(low, high), default c(-1, 1)
#' @param legend.midpoint middle point of the legend bar , default 0
#' @param legend.break breaks of the legend bar, a vector of numbers in the limits, default seq(-1,1,0.5)
#'
#' @return a ggplot object of heatmap showing correlation of two different matrix
#' @export
#'
#' @examples cor_heatmap_plot2(df_row = dat2, df_column = dat1, cor.method = 'spearman',
#'           legend.limit = c(-0.2, 1), legend.midpoint = 0.4, legend.break = seq(-0.2, 1, 0.4), pal = c("#4d685c", "white", "#a84b7c"))
cor_heatmap_plot2 <- function(df_row, df_column, cor.method = 'spearman',
                              pal = c("#0074b3", "white", "#982b2b"),
                              x.lab.size=14, x.lab.angle=90,
                              y.lab.size=14, y.lab.angle=0,
                              legend.title.size=14, legend.text.size=12,
                              legend.limit = c(-1, 1), legend.midpoint=0, legend.break = seq(-1,1,0.5) ){

    # 相关性计算
    dd = linkET::correlate(x = df_row, y = df_column, method = cor.method) %>% linkET::as_md_tbl() # 两个矩阵的相关性 ，需要格式转换为tibble，为长数据
    colnames(dd)[1:2] =  c('row', 'column') # 重命名列

    # 区分p值是否显著
    dd$Group = ifelse(dd$p < 0.05, "p < 0.05", "p > 0.05")

    # 相关性热图可视化--不显著为打叉
    p <- ggplot(data = dd, aes(x = row,y = column)) +
        geom_tile(aes(color = r), fill = "white") +
        geom_point(aes(color = r, shape = factor(Group)), size = 6) +
        scale_shape_manual(values = c(15,4), name = NULL) +  # 映射形状 15 方形， 4 叉叉
        scale_color_gradient2(low = pal[1], mid = pal[2], high = pal[3],
                              name = "Correlation",
                              limits = legend.limit, midpoint = legend.midpoint, breaks = legend.break ) +
        coord_fixed()  # 设置x和y轴比例为1，确保tile为正方形


    # 主题调整
    mytheme = theme(panel.grid = element_blank(),
                    legend.position = "right",
                    legend.text = element_text(size = legend.text.size),
                    legend.title = element_text(size = legend.title.size),
                    axis.ticks.x = element_blank(),
                    axis.ticks.y = element_blank(),
                    axis.title = element_blank(),
                    axis.text.x = element_text(colour = 'black', size = x.lab.size, angle = x.lab.angle, hjust = 0,vjust = 1),
                    axis.text.y = element_text(colour = 'black', size = y.lab.size, angle = y.lab.angle))


    p = p + scale_x_discrete(position = "top") + #x轴移动到顶部
        theme_bw() +
        mytheme

    return(p)
}




#' Bubble plot showing correlation of two different matrix,  bubble size for -log10(p), bubble color of correlation r
#'
#' @param df_row  dataframe with column names on the row of the correlation plot
#' @param df_column dataframe with column names on the column of the correlation plot
#' @param cor.method  methods for correlation analysis, default spearman
#' @param pal color for the bubble , a vector of 3 color numbers c(low, mid, high),  default c("#4d685c", "white", "#a84b7c")
#' @param x.lab.size text size of the row name of the heatmap; default 14
#' @param x.lab.angle text angle of the row name of the heatmap; default 90
#' @param y.lab.size text size of the column name of the heatmap; default 14
#' @param y.lab.angle text angle of the column names of the heatmap; default 0
#' @param legend.title.size text size of the legend title, default 14
#' @param legend.text.size  text size of the legend text, default 12
#' @param bulb.size.limit size limit  for the bubble (-log10P) , a vector of 2 numbers c(low, high),  default c(0, 200)
#' @param bulb.size.break breaks of the bubble size, a vector of numbers in the limits, default seq(0, 200, 50)
#' @param bulb.size.labels labels of the bubble size, a vector of numbers in the limits, default same with bulb.size.break
#' @param bulb.color.limit color limit of the bubble (r),  a vector of 2 numbers c(low, high), default c(-1, 1)
#' @param bulb.color.midpoint middle point of the bubble color , default 0
#' @param bulb.color.break breaks of the bubble color, a vector of numbers in the limits, default seq(-1,1,0.5)
#'
#' @return a ggplot object of bubble plot showing correlation of two different matrix
#' @export
#'
#' @examples cor_buble_plot(df_row = dat2, df_column =  dat1, bulb.color.limit = c(-0.2, 0.8), bulb.color.midpoint = 0.4, bulb.color.break = seq(-0.2,0.8,0.2),
#'           x.lab.size = 12, x.lab.angle = 75, y.lab.size = 12)
cor_buble_plot <- function(df_row, df_column, cor.method = 'spearman',
                           pal =  c("#4d685c", "white", "#a84b7c"),
                           x.lab.size=14, x.lab.angle=90,
                           y.lab.size=14, y.lab.angle=0,
                           legend.title.size=14, legend.text.size=12,
                           bulb.size.limit = c(0, 200), bulb.size.break=seq(0, 200, 50), bulb.size.labels,
                           bulb.color.limit = c(-1, 1), bulb.color.midpoint = 0, bulb.color.break = seq(-1, 1, 0.5) ){

    # 相关性计算
    dd = linkET::correlate(x = df_row, y = df_column, method = cor.method) %>% linkET::as_md_tbl() # 两个矩阵的相关性 ，需要格式转换为tibble，为长数据
    colnames(dd)[1:2] =  c('row', 'column') # 重命名列

    # 转换p值
    dd$log10P = -log10(dd$p)
    dd$log10P[is.infinite(dd$log10P)] = 200  # 替换掉无穷大



    # 气泡大小的断点
    if(missing(bulb.size.labels)){
        bulb.size.labels = bulb.size.break
    }

    # 气泡图
    p = ggplot(data = dd, aes(x = row, y = column, size = log10P)) +
        geom_point(shape = 21, aes(fill = r), position =position_dodge(0))+
        scale_size_continuous(range = c(0, 6), name = "-log10 Pvalue",
                              limits = bulb.size.limit,  breaks = bulb.size.break, labels = bulb.size.labels ) +
        scale_fill_gradient2(low = pal[1], mid = pal[2], high = pal[3],
                             name = "Correlation",
                             limits = bulb.color.limit, midpoint = bulb.color.midpoint, breaks = bulb.color.break )

    # 主题调整
    p = p + theme_minimal() +
        theme(panel.border = element_rect(fill = NA, color = "black", size = 1, linetype = "solid"),
              panel.grid = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title = element_blank(),
              axis.text.x = element_text(colour = 'black', size = x.lab.size, angle = x.lab.angle, hjust = 1 ),
              axis.text.y = element_text(colour = 'black', size = y.lab.size,  angle = y.lab.angle) )

    ####拉长图例
    p = p + theme(legend.position = "bottom",
                  legend.text = element_text(color = "black",size = legend.text.size),
                  legend.title = element_text(size = legend.title.size, color = "black")) +
        guides(fill = guide_colorbar(title.position = "left",title.hjust = 0.5,
                                     barwidth = 15,barheight = 1.5,ticks = TRUE))

    return(p)
}


