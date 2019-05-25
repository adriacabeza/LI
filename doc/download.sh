#!/bin/bash

curl http://www.hakank.org/sicstus/|grep '.pl'|grep href|cut -d= -f2|cut -d'"' -f2 > list


for i in $(cat list); do
	curl http://www.hakank.org/sicstus/$i > $i &
done
