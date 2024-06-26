% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/correlationplot.R
\name{cor_link_mantel}
\alias{cor_link_mantel}
\title{corelation showing on two different matrix, not all the correlation of factors in one row, line show with mantel test}
\usage{
cor_link_mantel(
  df_matrix,
  df_link,
  link_column_list,
  cor.method = "spearman",
  text.size = 2.5
)
}
\arguments{
\item{df_matrix}{correlation show using matrix}

\item{df_link}{correlation show using link lines on the factors of the matrix}

\item{link_column_list}{a list object define link points on the df_line,using colnum numbers, can be multi-columns using :}

\item{cor.method}{cor analysis methods ,default spearman}

\item{text.size}{text size ,default 2.5}
}
\value{
a link line corplot using mantel test
}
\description{
corelation showing on two different matrix, not all the correlation of factors in one row, line show with mantel test
}
\examples{
cor_link_mantel(df_matrix= df_expr, df_link=immucell_df, link_column_list=list(B = 1, CD4T = 2, CD8T = 3))
}
