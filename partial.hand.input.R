Little.GOP.States <- tribble(
    ~state, ~pct.pop, ~gop.sens,
    "WY", 0.18, 2,
    "AK", 0.23, 2,
    "ND", 0.23, 1,
    "SD", 0.27, 2,
    "MT", 0.32, 1,
    "ME", 0.41, 1,
    "ID", 0.53, 1,
    "WV", 0.56, 1,
    "NE", 0.59, 2,
    "KS", 0.89, 2,
    "MS", 0.92, 2,
    "NV", 0.92, 1,
    "AR", 0.92, 2,
    "UT", 0.95, 2,
    "IA", 0.97, 2
)

sum(Little.GOP.States$pct.pop)
sum(Little.GOP.States$gop.sens)
count(Little.GOP.States$state)
