#!/bin/bash

WordReg="([a-zA-Z][a-zA-Z'-]*)?[a-zA-Z][']?"
echo $(man bash) | grep -E -o "$WordReg" | grep -E ".{4,}" | tr [:upper:] [:lower:] | sort | uniq -c | sort -nr | head -n 3
