#!/usr/bin/env python3

import re
import sys
import os.path

_, cmdstan_path = sys.argv

grammar_path = os.path.join(
    cmdstan_path,
    "stan/src/stan/lang/grammars/var_decls_grammar_def.hpp"
)

funcsig_path = os.path.join(
    cmdstan_path,
    "stan/src/stan/lang/function_signatures.h"
)

stan_types = []
with open(grammar_path, 'r') as fd:
    for line in fd.readlines():
        match = re.search(r"%= \(lit\(\"(.+)\"", line)
        if match:
            stan_types.append(match.group(1))
stan_types = sorted(list(set(stan_types)))

print("syn keyword stanTypes " + " ".join(stan_types))

funcsigs = []
with open(funcsig_path, 'r') as fd:
    for line in fd.readlines():
        match = re.search(r"add(?:_\w+?)*\(\"(.+?)\"", line)
        if match:
            funcsigs.append(match.group(1))

stan_distributions = []
for line in set(funcsigs):
    if line.endswith('_lpdf') or line.endswith('_lpmf'):
        stan_distributions.append(line[:-5])

stan_functions = [i for i in set(funcsigs) if not i.endswith("_log")]

suffix = ["lpdf", "lpmf", "cdf", "lcdf", "lccdf", "rng"]
for s in suffix:
    sf = sorted([i for i in stan_functions if i.endswith(s)])
    print("syn keyword stanFunctions %s" % " ".join(sf))

sf = [i for i in stan_functions if i.split("_")[-1] not in suffix]
print("syn keyword stanFunctions %s" % " ".join(sf))
