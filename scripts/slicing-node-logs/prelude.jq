def mean: reduce .[] as $n (0; . + $n) / length;
def pow2: . * .;
def variance: . | mean as $mean | map_values(. - $mean | pow2) | mean;
def stdev: . | variance | sqrt;
def quantile(f): sort | .[(length * ([f,0.99999999] | min)) | floor];

# some historical runs printed milliseconds instead of centiseconds: remove the '10 * ' when processing those
def p(f): f | split(".") | (1000 * (.[0] | (. + "Z") | fromdate) + 10 * (.[1] | split("Z") | .[0] | tonumber));

def ppoint: split("@") | {hash: first, slot: (last | tonumber)};
