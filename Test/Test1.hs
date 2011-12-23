import AI.RBM

-- r = fromLists ([0.01, 0, 0.05], [0.035, (-0.013)], [[0.01, 0.06, 0.25], [-0.072, 0.048, 0.017]])
r = fromLists ([0, 0, 0], [0, 0], [[0.01, 0.06, 0.25], [-0.072, 0.048, 0.017]])
v = vecFromList [1, 0, 1]
e = 0.07

main = do
  res <- sequence $ replicate 20 (learnLoop 600 e r (dupVec 100 v) >>= \rbm -> reconstruct rbm (vecFromList [1, 1, 0]))
  mapM_ print res
