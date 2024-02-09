#!/bin/bash
rm -f tx.json tx.yaml

if [ -z "${NODE_URL}" ]; then
  CHAINWEB_NODE='https://api.chainweb.com'
else
  CHAINWEB_NODE=${NODE_URL}
fi

kda gen -t on_chain.tkpl
kda combine-sigs tx.yaml
STATUS=`kda local tx.json -n ${CHAINWEB_NODE}|jq -r ".[]|.[].body.result.status"`
if [ $STATUS == "success" ]
  then kda local tx.json -n ${CHAINWEB_NODE}|jq ".[]|.[].body.result.data"
  else kda local tx.json -n ${CHAINWEB_NODE}|jq ".[]|.[].body.result"
fi
