#!/bin/bash
Reg="^\[[[:space:]]*[0-9]*\.?[0-9]*\] "
Warn=$Reg"\(WW\)"
Inf=$Reg"\(II\)"
cat /var/log/anaconda/X.log | grep -E "$Warn" | sed -E "s/$Warn/Warning:/" > full.log
cat /var/log/anaconda/X.log | grep -E "$Inf" | sed -E "s/$Inf/Information:/" >> full.log
less full.log
