source('load_data.r');
source('lwlr.r');
source('plot_lwlr.r');

load_data();
plot_lwlr(X, y, 0.1, 10);
