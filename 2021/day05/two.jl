using DataStructures

if length(ARGS) != 1
  println(stderr, "Usage: julia two.jl <input>")
  exit(1)
end

parseline(line) = parse.(Int, split(line, r"\D+"))

function pointsalong(seg)
  x1, y1, x2, y2 = seg
  if x1 == x2
    ((x1, y) for y in min(y1, y2):max(y1, y2))
  elseif y1 == y2
    ((x, y1) for x in min(x1, x2):max(x1, x2))
  else
    xsign, ysign = sign(x2 - x1), sign(y2 - y1)
    ((x1 + xsign * i, y1 + ysign * i) for i in 0:abs(x2 - x1))
  end
end

segments = parseline.(readlines(ARGS[1]))
hits = DefaultDict(0)
for segment in segments, p in pointsalong(segment)
  hits[p] += 1
end
println(count(>(1), values(hits)))