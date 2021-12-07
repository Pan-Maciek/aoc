function two
  xs=dlmread("input.in");
  mincost=inf;
  for i = min(xs):max(xs)
    mincost = min(mincost, cost(xs, i));
  endfor
  mincost
endfunction

function cost = cost(xs, to)
  xs = abs(xs .- to);
  cost = sum(xs .* (xs .+ 1) ./ 2);
endfunction
  