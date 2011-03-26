source('load_data.r');
source('lwlr.r');
source('plot_lwlr.r');

load_data();
# problem set suggests debugging with a resolution of 50
plot_lwlr(X, y, 0.1, 50);
