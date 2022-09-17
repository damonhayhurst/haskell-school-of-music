applyAll [] v = v
applyAll (fn:fns) v = fn (applyAll fns v)