import AI.RBM

-- r = fromLists ([0.01, 0, 0.05], [0.035, (-0.013)], [[0.01, 0.06, 0.25], [-0.072, 0.048, 0.017]])
r = fromLists ([0, 0, 0], [0, 0], [[0.01, 0.06, 0.25], [-0.072, 0.048, 0.017]])
v = vecFromList [1, 0, 1]
e = 0.07

main = do
  res <- sequence $ replicate 2 (learnLoop 60 e r (dupVec 10 v) >>= \rbm -> reconstruct rbm (vecFromList [1, 1, 0]))
  print res
