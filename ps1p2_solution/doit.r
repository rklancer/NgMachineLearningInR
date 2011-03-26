# source this file to generate 10 plots
source('load_data.r');
source('lwlr.r');
source('plot_lwlr.r');

load_data();
# problem set suggests debugging with a resolution of 50
for (tau in exp(seq(log(0.01), log(5),len=10))) {
  plot_lwlr(X, y, tau, 200)
  print(tau)
}
