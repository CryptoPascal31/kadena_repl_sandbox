#!/bin/bash
rm -f tx.json tx.yaml

kda gen -t on_chain.tkpl
kda combine-sigs tx.yaml
STATUS=`kda local tx.json|jq -r ".[]|.[].body.result.status"`
if [ $STATUS == "success" ]
  then kda local tx.json|jq ".[]|.[].body.result.data"
  else kda local tx.json|jq ".[]|.[].body.result"
fi
