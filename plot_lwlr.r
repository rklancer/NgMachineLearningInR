plot_lwlr <- function(X, y, tau, res) {
  x    <- matrix(0,2,1);
  pred <- matrix(0, res, res);
  
  # Loop over a linear grid of points, size res x res, in the square bounded by the points 
  # {(-1,1), (1,1), (1,-1), (-1,-1)}.
  # Set pred[i, j] <- lwlr prediction of y for the x-vector at i,j-th point in the above grid
  
  r <- -1 + (2/(res-1) * 0:(res-1));
  for(i in 1:res) {
    for(j in 1:res) {
      x[1] = r[i];
      x[2] = r[j];
      
      # reversal of indices to [j,i] in Matlab version is not required because R's image(pred) rotates the matrix image
      # anyway (so that x-coordinate is in direction of increasing x[1], and y-coordinate is in direction of increasing
      # x[2])
      pred[i,j] = lwlr(X, y, x, tau);
    }
  }
  
  # produces a semi-readable figure...
  image(r, r, pred, col=heat.colors(res^2));              # plot matrix values with heatmap-like colors
  par(new=TRUE);            # redraw next plot on the same figure (that's counterintuitive, huh?)
  plot( res/2 * (1+X[y==0,1]) + 0.5, res/2 * (1+X[y==0,2]) + 0.5, col='white', pch=1, axes=FALSE, xlab='', ylab='');  # pch=1 -> plot points as "o"s
  par(new=TRUE);
  plot( res/2 * (1+X[y==1,1]) + 0.5, res/2 * (1+X[y==1,2]) + 0.5, col='black', pch=4, axes=FALSE, xlab='', ylab='');  # pch=4 -> plot points as "x"s
  
  pred
  
  # # convert from Matlab:
  # axis off;
  # hold on;
  # imagesc(pred, [-0.4 1.3]);

  # axis equal;
  # axis square;
  # text(res/2 - res/7, res + res/20, ['tau = ' num2str(tau)], 'FontSize', 18);
}
