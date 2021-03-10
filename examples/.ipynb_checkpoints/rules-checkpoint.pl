
rule(
  ipd,
  boundary,
  0,
  if agent(A) then participates(A) where []
).

rule(
  ipd,
  position,
  0,
  if participates(A) then role(A,prisoner) where []
).
