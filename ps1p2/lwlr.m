function y = lwlr(X_train, y_train, x, tau)

m = size(X_train,1);
n = size(X_train,2);
theta = zeros(n,1);

% compute weights
w = exp(-sum((X_train - repmat(x', m, 1)).^2, 2) / (2*tau));

% perform Newton's method
g = ones(n,1);
while (norm(g) > 1e-6)
  h = 1 ./ (1 + exp(-X_train * theta));
  g = X_train' * (w.*(y_train - h)) - 1e-4*theta;
  H = -X_train' * diag(w.*h.*(1-h)) * X_train - 1e-4*eye(n);
  theta = theta - H \ g;
end

% return predicted y
y = double(x'*theta > 0);
