\name{int_nic}
\alias{int_nic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Intervalo de confianza}
\description{Esta funcion calcula el intervalo de confianza de una muestra "x" dada una probabilidad}
\usage{
int_nic(x,p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Vector numerico de valores continuos o discretos}
  \item{p}{Valor de confianza entre cero y uno para el calculo de los intervalos}
}
\details{EL intervalo de confianza es un valor producto del error estandar de la muestra por el valor de Z asociado al valor de confianza}
\value{Un data.frame que despliega los limites inferior y superior del intervalo, asi como la media de la muestra}
\references{
%% ~put references to the literature/web site here ~
}
\author{Nicasio-Arzeta, S.}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
holi <- sample(seq(1,100,0.01),50)
ci <- int_nic(holi,0.95)
ci
}
